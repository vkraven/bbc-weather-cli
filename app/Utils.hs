module Utils (getXdgConfDir,
              getXdgConfFile,
              getUserdataConfDir,
              getUserdataConfFile,
              getHomeConfFile,
              getExistingConfFilePath,
              makeDefaultPreferredConfFilePath,
              getWeatherTypeIcon,
              detailedReportToMainStatus,
              detailedReportToAltStatus,
              detailedReportToCliStatus) where

import JSONTypes

import System.Directory
import System.IO (FilePath)
import Control.Monad (filterM)
import Data.List (intersperse)

bbcConfDir :: String
bbcConfDir = "bbc-weather-cli"

bbcConfFile :: String
bbcConfFile = "config.json"

getXdgConfDir :: IO FilePath
getXdgConfDir = getXdgDirectory XdgConfig bbcConfDir

getXdgConfFile :: IO FilePath
getXdgConfFile = getXdgConfDir >>= return . (\x -> x ++ "/" ++ bbcConfFile)

getUserdataConfDir :: IO FilePath
getUserdataConfDir = getAppUserDataDirectory bbcConfDir

getUserdataConfFile :: IO FilePath
getUserdataConfFile = getUserdataConfDir >>= return . (\x -> x ++ "/" ++ bbcConfFile)

getHomeConfFile :: IO FilePath
getHomeConfFile = getHomeDirectory >>= return . (\x -> x ++ "/." ++ bbcConfDir ++ "-" ++ bbcConfFile)

ioConfFiles :: IO [FilePath]
ioConfFiles = sequence [getXdgConfFile, getUserdataConfFile, getHomeConfFile]

anyConfFilesExist :: IO Bool
anyConfFilesExist = ioConfFiles >>=
                      mapM doesFileExist >>=
                        return . any id

getExistingConfFilePath :: IO FilePath
getExistingConfFilePath = ioConfFiles >>= \iof -> anyConfFilesExist >>=
                            \exists -> if exists
                                        then filterM doesFileExist iof >>= return . head
                                        else return ""

makeDefaultPreferredConfFilePath :: IO FilePath
makeDefaultPreferredConfFilePath = getXdgConfDir >>=
                                     createDirectoryIfMissing True >>
                                       getXdgConfFile

getWeatherTypeIcon :: Integer -> String
getWeatherTypeIcon 0  = "\xf186"   -- ??? - Clear sky (night)
getWeatherTypeIcon 1  = "\xe30d"   -- ??? - Sunny day
getWeatherTypeIcon 2  = "\xe37e"   -- ???  - Partly cloudy (night)
getWeatherTypeIcon 3  = "\xe30c"   -- ??? - Sunny intervals
getWeatherTypeIcon 4  = "\xe37a"   -- ???  - Sandstorm
getWeatherTypeIcon 5  = "\xfa90"   -- ??? - Mist day / night
getWeatherTypeIcon 6  = "\xfa90"   -- ??? - Fog day / night
getWeatherTypeIcon 7  = "\xe376"   -- ??? - White medium level cloud
getWeatherTypeIcon 8  = "\xe312"   -- ??? - Black low level cloud
getWeatherTypeIcon 9  = "\xfa96"   -- ??? - Light rain shower (night)
getWeatherTypeIcon 10 = "\xe30b"   -- ??? - Light rain shower (day)
getWeatherTypeIcon 11 = "\xe34a"   -- ??? -Drizzle
getWeatherTypeIcon 12 = "\xfa96"   -- ??? - Light rain day or night
getWeatherTypeIcon 13 = "\xe325"   -- ??? - Heavy rain shower (night)
getWeatherTypeIcon 14 = "\xe308"   -- ??? - Heavy rain shower (day)
getWeatherTypeIcon 15 = "\xfa95"   -- ??? -  Heavy rain
getWeatherTypeIcon 16 = "\xe3ac"   -- ??? - Sleet shower (night)
getWeatherTypeIcon 17 = "\xe3aa"   -- ??? - Sleet shower (day)
getWeatherTypeIcon 18 = "\xe3ad"   -- ??? - Cloudy with sleet
getWeatherTypeIcon 19 = "\xe321"   -- ??? - Hail shower (night)
getWeatherTypeIcon 20 = "\xe304"   -- ??? - Hail shower (day)
getWeatherTypeIcon 21 = "\xe314"   -- ??? - Cloudy with hail
getWeatherTypeIcon 22 = "\xe327"   -- ??? - Light snow shower (night)
getWeatherTypeIcon 23 = "\xe30a"   -- ??? - Light snow shower (day)
getWeatherTypeIcon 24 = "\xe31a"   -- ??? - Cloudy with light snow
getWeatherTypeIcon 25 = "\xe367"   -- ??? - Heavy snow shower (night)
getWeatherTypeIcon 26 = "\xe365"   -- ??? - Heavy snow shower (day)
getWeatherTypeIcon 27 = "\xfa97"   -- ??? - Cloudy with heavy snow
getWeatherTypeIcon 28 = "\xe337"   -- ??? - Thundery shower (night)
getWeatherTypeIcon 29 = "\xe30e"   -- ??? - Thundery shower (day)
getWeatherTypeIcon 30 = "\xe31d"   -- ??? - Thunderstorms
getWeatherTypeIcon 31 = "\xe208"   -- ??? - Tropical storm (bbc uses some type of spinning thing)
getWeatherTypeIcon 32 = "HAZY"     -- ??? - Hazy
getWeatherTypeIcon 33 = "SANDSTORM"-- Text - Sandstorm
getWeatherTypeIcon 34 = "MIST"     -- Text - Mist
getWeatherTypeIcon 35 = "FOG"      -- Text - Fog
getWeatherTypeIcon 36 = "\xe33d"   -- ??? - Light cloud
getWeatherTypeIcon 37 = "\xf0c2"   -- ??? - Thick cloud
getWeatherTypeIcon 38 = "\xe239"   -- ??? - Thick cloud and drizzle
getWeatherTypeIcon 39 = "\xfa96"   -- ??? - Thick cloud and raindrop
getWeatherTypeIcon 40 = "\xfa96\xe34a"  -- ?????? - Thick cloud and double raindrop
getWeatherTypeIcon 41 = "\xfb7d"   -- ??? - Thick cloud, snowy + rainy
getWeatherTypeIcon 42 = "\xfa91"   -- ??? - Thick cloud and hailstones
getWeatherTypeIcon 43 = "\xfa97"   -- ??? - Thick cloud and snowflake
getWeatherTypeIcon 44 = "\xe31a"   -- ??? - Thick cloud and double snowflake
getWeatherTypeIcon 45 = "\xfa92"   -- ??? - Thick cloud and lightning
getWeatherTypeIcon 46 = "\xe208"   -- ??? - Tropical storm again
getWeatherTypeIcon 47 = "HAZY"    -- Text - Hazy
getWeatherTypeIcon _  = "\xf420"   -- ??? - Unknown status

detailedReportToMainStatus :: DetailedReport -> String
detailedReportToMainStatus dr = (getWeatherTypeIcon $ dr_extendedWeatherType dr) ++
                                  "  " ++
                                    (show $ dr_temperatureC dr) ++
                                      "\xe339" -- ???

detailedReportToAltStatus :: DetailedReport -> String
detailedReportToAltStatus dr = "\xe3a9" ++
                                 " " ++ (show $ dr_windSpeedKph dr) ++ " kph " ++
                                   (dr_windDirectionAbbreviation dr) ++ " " ++
                                     "\xe371:" ++ " " ++ (show $ dr_precipitationProbabilityInPercent dr) ++
                                       "% | " ++ (dr_enhancedWeatherDescription dr)

detailedReportToCliStatus :: DetailedReport -> String
detailedReportToCliStatus = concat . (intersperse " ") . ((<*>) [detailedReportToMainStatus, detailedReportToAltStatus]) . pure
