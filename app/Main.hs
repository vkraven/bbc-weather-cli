module Main where

import JSONTypes
import Utils
import qualified MainArgs as ARG
import System.Console.CmdArgs (cmdArgsRun)

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import System.Directory (doesFileExist, canonicalizePath)
import Control.Monad.Trans.Except
import Control.Exception (catch, IOException, Exception, SomeException)
import Control.Monad.Catch (catchAll)
import System.Exit (die)
import Data.Aeson (decode, encode, eitherDecode)
import qualified Data.Time.Clock as Time
import qualified Data.Time.LocalTime as Local
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString as BS
import qualified Data.Time.Calendar as Cal (Day, diffDays, dayOfWeek)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (sortOn, intersperse)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Network.URI.Encode as URIEnc (encode)

configFile :: IO String
configFile = do
  existingConf <- getExistingConfFilePath
  if existingConf == ""
    then do
          fp <- makeDefaultPreferredConfFilePath
          BL8.writeFile fp $ encodePretty defaultConfig
          return fp
    else return existingConf


readConfigFile :: String -> ExceptT IOException IO BL.ByteString
readConfigFile f =  ExceptT ((Right <$> BL.readFile f) `catch` (\e -> return . Left $ e))

getConfig :: IO ConfigFile
getConfig = configFile >>= getCustomConfig

getCustomConfig :: String -> IO ConfigFile
getCustomConfig confLoc = do
    canConfLoc <- canonicalizePath confLoc
    confFileExists <- doesFileExist canConfLoc
    if not confFileExists
      then (die $ "Config file at: " ++ canConfLoc ++ " does not exist. Unable to continue.")
      else runExceptT (readConfigFile canConfLoc) >>=
            either printErrorAndExit (return . eitherDecode) >>=
              either printDecodeErrorAndExit return

addQueryParam :: BS.ByteString -> String -> String -> BS.ByteString
addQueryParam query param value = BS.append query $
                                    BL8.toStrict $
                                      BL8.pack $
                                        "&" ++ param ++ "=" ++ value

queryForBBCLocation :: String -> IO (Maybe BL.ByteString)
queryForBBCLocation loc = do
  conf <- getConfig
  man <- newTlsManager
  req <- (parseRequest $ "GET " ++ (locationEndpoint conf)) `catchAll` printUrlParsingErrorAndExit
  let req1 = req {
              queryString = addQueryParam (addQueryParam (queryString req) "api_key" (locationApiKey conf))
                                          "s" (URIEnc.encode loc)
              }
   in do
    res <- httpLbs req1 man
    case statusCode $ responseStatus res of
      200 -> return $ Just $ responseBody res
      _   -> return Nothing

bbcLocationToString :: BBCLocation -> String
bbcLocationToString (BBCLocation iD name container _ _ _ _ _ _ typ) = iD ++ "\t" ++ name ++ ", " ++ container ++ "\tType: " ++ typ

parseBBCLocResults :: Maybe BL.ByteString -> Maybe (Integer, [String])
parseBBCLocResults Nothing = Nothing
parseBBCLocResults (Just res) = let res1 = decode res :: Maybe BBCLocResponse
                                 in case res1 of
                                      Nothing -> Nothing
                                      Just res2 -> Just $
                                                    ((bbclq_totalResults . bbclres_results . bbclr_response $ res2),
                                                     bbcLocationToString <$> (bbclq_results . bbclres_results . bbclr_response $ res2) )

displayBBCLocResults :: Maybe (Integer, [String]) -> IO ()
displayBBCLocResults Nothing = putStrLn "Location search failure"
displayBBCLocResults (Just (total, res)) = putStrLn ("Total Results:\t" ++ (show total)) >>
                                            mapM_ putStrLn res

queryLocationAndDisplay :: String -> IO ()
queryLocationAndDisplay = \x -> queryForBBCLocation x >>= displayBBCLocResults . parseBBCLocResults

printErrorAndExit :: Exception e => e -> IO a
printErrorAndExit e = putStrLn "Encountered error:" >> die (show e)

printDecodeErrorAndExit :: String -> IO a
printDecodeErrorAndExit e = putStrLn "Unable to continue: error when decoding config.json" >> die e

printUrlParsingErrorAndExit :: SomeException -> IO a
printUrlParsingErrorAndExit e = putStrLn "Error when parsing the URL from config.json" >> die (show e)

fetchWeatherForecast :: Manager -> String -> String -> IO (Maybe BL.ByteString)
fetchWeatherForecast man endp loc = do
  req <- (parseRequest $ "GET " ++ endp ++ loc) `catchAll` printUrlParsingErrorAndExit
  res <- httpLbs req man
  case statusCode $ responseStatus res of
    200 -> return $ Just $ responseBody res
    _   -> return Nothing

fetchWeatherForecastFromConfig :: Manager -> ConfigFile -> IO (Maybe BL.ByteString)
fetchWeatherForecastFromConfig man conf = fetchWeatherForecast man (forecastEndpoint conf) (forecastLocationCode conf)

fetchAggregateForecastFromConfig :: Manager -> ConfigFile -> IO (Maybe BL.ByteString)
fetchAggregateForecastFromConfig man conf = fetchWeatherForecast man (aggregateForecastEndpoint conf) (forecastLocationCode conf)

getWeatherObservations :: Weather -> [WeatherData]
getWeatherObservations w = let feats = filter (\x -> (fp_observations . f_properties) x /= Nothing) $ w_features w
                            in case length feats > 0 of
                                 True -> fromJust $ fp_observations . f_properties $ head feats
                                 False -> []

getWeatherForecasts :: Weather -> [WeatherData]
getWeatherForecasts w = let feats = filter (\x -> (fp_forecasts . f_properties) x /= Nothing) $ w_features w
                         in case length feats > 0 of
                              True -> fromJust $ fp_forecasts . f_properties $ head feats
                              False -> []

getClosestWeatherData :: Time.UTCTime -> [WeatherData] -> WeatherData
getClosestWeatherData now w = head $ sortOn (\x -> abs . Time.nominalDiffTimeToSeconds . ((flip Time.diffUTCTime) now) . time_utc . wdata_time $ x) w

convertDetailedTimeStringToUTC :: Local.TimeZone -> DetailedReport -> Time.UTCTime
convertDetailedTimeStringToUTC tz dr = Local.zonedTimeToUTC $
                                         Local.ZonedTime (Local.LocalTime (dr_localDate dr) (Local.timeToTimeOfDay $
                                                                                               Time.secondsToDiffTime $
                                                                                                 60 * 60 * (read (take 2 $ dr_timeslot dr)))) tz

getLocalDay :: IO Cal.Day
getLocalDay = Local.getZonedTime >>=
                 return . Local.zonedTimeToLocalTime >>=
                   return . Local.localDay

getClosestDetailedData :: Local.TimeZone -> Time.UTCTime -> AggregateWeather -> DetailedReport
getClosestDetailedData tz now w = let alldr = concat $ df_reports . awf_detailed <$> forecasts w
                                   in head $
                                        sortOn (\x -> abs . Time.nominalDiffTimeToSeconds .
                                                  ((flip Time.diffUTCTime) now) . convertDetailedTimeStringToUTC tz $ x) alldr

getNextNSummaryData :: Cal.Day -> Int -> AggregateWeather -> [SummaryReport]
getNextNSummaryData today n w = let summs = (sf_report . awf_summary) <$> forecasts w
                                    day_diff_amount = ((flip Cal.diffDays) today) . sr_localDate
                                 in take n $
                                      sortOn day_diff_amount $
                                        filter (\x -> day_diff_amount x >= 0) summs

aggregateWeatherToLocationString :: AggregateWeather -> String
aggregateWeatherToLocationString = concat . (intersperse ", ") . ((<*>) [(awl_name . location), (awl_container . location)]) . pure

aggregateWeatherToLastUpdatedTimeOfDay :: AggregateWeather -> String
aggregateWeatherToLastUpdatedTimeOfDay = show . Local.localTimeOfDay . Local.zonedTimeToLocalTime . lastUpdated


summaryReportToTooltipLine :: SummaryReport -> String
summaryReportToTooltipLine sr = (show . Cal.dayOfWeek $ sr_localDate sr) ++
                                  ", " ++
                                    (show $ sr_localDate sr) ++
                                      ": " ++
                                        (getWeatherTypeIcon $ sr_weatherType sr) ++ "  " ++
                                          "H: " ++ (show $ sr_maxTempC sr) ++ "\xe339 " ++
                                          "L: " ++ (show $ sr_minTempC sr) ++ "\xe339 " ++
                                          "\xe371: " ++ (show $ sr_precipitationProbabilityInPercent sr) ++ "%" ++
                                            " | " ++ (sr_enhancedWeatherDescription sr)

equalizeStringChunksBySep :: String -> [String] -> [String]
equalizeStringChunksBySep "" ttls = ttls
equalizeStringChunksBySep sep ttls = let maxlen = maximum $ length . head <$> (splitOn sep) <$> ttls
                                         padhead x = [head x ++ sep ++ (replicate  (maxlen - (length $ head x))  ' ')] <> (intersperse sep $ tail x)
                                      in concat . padhead <$> (splitOn sep <$> ttls)

summaryReportsToTooltipSummary :: [SummaryReport] -> String
summaryReportsToTooltipSummary = concat . intersperse "\n" .
                                  equalizeStringChunksBySep "%" .
                                   equalizeStringChunksBySep ":" .
                                    (<$>) summaryReportToTooltipLine

aggregateWeatherToTooltip :: Cal.Day -> String -> AggregateWeather -> String
aggregateWeatherToTooltip today dr_str aw = "<big>" ++ (aggregateWeatherToLocationString aw) ++ "</big>\n" ++
                                              "Now: " ++ dr_str ++ "\n\n" ++
                                                (summaryReportsToTooltipSummary $ getNextNSummaryData today 3 aw) ++ "\n" ++
                                                  "<small>Last Updated: " ++ (aggregateWeatherToLastUpdatedTimeOfDay aw) ++ "</small>"

aggregateNWeatherToTooltip :: Integral n => n -> Cal.Day -> String -> AggregateWeather -> String
aggregateNWeatherToTooltip n today dr_str aw = "<big>" ++ (aggregateWeatherToLocationString aw) ++ "</big>\n" ++
                                                "Now: " ++ dr_str ++ "\n\n" ++
                                                   (summaryReportsToTooltipSummary $ getNextNSummaryData today (fromIntegral n) aw) ++ "\n" ++
                                                     "<small>Last Updated: " ++ (aggregateWeatherToLastUpdatedTimeOfDay aw) ++ "</small>"

getSwitchConf :: String -> IO ConfigFile
getSwitchConf "DEFAULT" = getConfig
getSwitchConf x = getCustomConfig x

getSwitchConfFilePath :: String -> IO String
getSwitchConfFilePath "DEFAULT" = getExistingConfFilePath
getSwitchConfFilePath x = canonicalizePath x

mainGetAggregateWeather :: String -> IO AggregateWeather
mainGetAggregateWeather uConfLoc = do
  man <- newTlsManager
  conf <- getSwitchConf uConfLoc
  case forecastLocationCode conf of
    "" -> do
      confloc <- getSwitchConfFilePath uConfLoc
      die $ "No location code has been set in the config file at:\n\t" ++ confloc
    _  -> do
      w <- fetchAggregateForecastFromConfig man conf
      case w of
        Nothing -> die "BBC's weather API did not return a valid HTTP response. Unable to continue."
        Just w1 -> let w2 = decode w1 :: Maybe AggregateWeather
                    in case w2 of
                         Nothing -> die "BBC's weather API did not return a valid JSON payload. Unable to parse."
                         Just w3 -> return w3

waybar :: String -> IO ()
waybar uConfLoc = do
  agg <- mainGetAggregateWeather uConfLoc
  conf <- getSwitchConf uConfLoc
  tz  <- Local.getCurrentTimeZone
  now <- Time.getCurrentTime
  today <- getLocalDay
  let closestDetailed = getClosestDetailedData tz now agg
      summaryDays = case (defaultSummaryDays conf) of
                      Nothing -> defaultSummaryDaysValue
                      Just n  -> n
      jsonout = WaybarJSON (detailedReportToMainStatus closestDetailed)
                           (detailedReportToAltStatus closestDetailed)
                           (aggregateNWeatherToTooltip summaryDays today (detailedReportToAltStatus closestDetailed) agg)
   in BL8.putStr . encode $ jsonout

cliWeatherStatus :: String -> IO ()
cliWeatherStatus uConfLoc = do
  agg <- mainGetAggregateWeather uConfLoc
  conf <- getSwitchConf uConfLoc
  tz  <- Local.getCurrentTimeZone
  now <- Time.getCurrentTime
  today <- getLocalDay
  putStr $ aggregateWeatherToLocationString agg ++ ": "
  putStrLn $ detailedReportToCliStatus $ getClosestDetailedData tz now agg

displayConfig :: IO ()
displayConfig = do
  discoveredConf <- getExistingConfFilePath
  case discoveredConf of
    "" -> die "No existing config.json files were found!\nA default config.json file will be created on first run"
    x -> do
          putStrLn $ "Found an existing config file at: " ++ x ++ "\n"
          conf <- getConfig
          BL8.putStrLn $ encodePretty conf

displayCustomConfig :: String -> IO ()
displayCustomConfig "DEFAULT" = displayConfig
displayCustomConfig confLoc = getCustomConfig confLoc >>=
                               \conf -> BL8.putStrLn $ encodePretty conf

checkConfigLocation :: ConfigFile -> IO Bool
checkConfigLocation (ConfigFile _ _ _ locCode agg _) = do
  putStrLn "Assert: location code is not blank"
  case locCode of
    "" -> putStrLn "FAIL: No location code has been declared.\n \
                     \ Please set a location code in config.json\n \
                     \ Run bbc-weather-cli loc --search= if you don't know your location code" >>
            return False
    lC -> do
      putStrLn $ "OK: Location code is " ++ lC
      man <- newTlsManager
      res <- fetchWeatherForecast man agg lC
      case res of
        Nothing -> do
          putStrLn $ "FAIL: The endpoint " ++ agg ++ lC ++ " did not return an HTTP 200 response."
          return False
        Just _ -> do
          putStrLn "OK: The aggregate weather forecast returned an HTTP 200 response"
          putStrLn "PASSED: Location Code check succeeded!"
          return True


checkConfig :: ConfigFile -> IO ()
checkConfig conf = checkConfigLocation conf >>= print

handleArgs :: ARG.BBCCli -> IO ()
handleArgs (ARG.Json "waybar" conf) = waybar conf
handleArgs (ARG.Json _ _) = die "Other JSON formats are not supported yet"
handleArgs (ARG.Config confLoc False) = displayCustomConfig confLoc
handleArgs (ARG.Config "DEFAULT" True) = getConfig >>= checkConfig
handleArgs (ARG.Config confLoc True) = getCustomConfig confLoc >>= checkConfig
handleArgs (ARG.Location "") = die "No search parameter provided for the location query"
handleArgs (ARG.Location loc) = queryLocationAndDisplay loc
handleArgs (ARG.Cli confLoc) = cliWeatherStatus confLoc
handleArgs _ = putStrLn "No supported arguments given"

main :: IO ()
main = cmdArgsRun ARG.bbccli >>= handleArgs
