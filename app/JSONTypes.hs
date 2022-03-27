{-# LANGUAGE DeriveGeneric #-}

module JSONTypes where

import GHC.Generics
import Data.Aeson
import Data.Scientific
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar

data ConfigFile = ConfigFile {
  locationEndpoint :: String,
  locationApiKey :: String,
  forecastEndpoint :: String,
  forecastLocationCode :: String,
  aggregateForecastEndpoint :: String,
  defaultSummaryDays :: Maybe Integer
                             }
                  deriving (Eq, Show, Generic)

instance ToJSON ConfigFile where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ConfigFile


defaultConfig :: ConfigFile
defaultConfig = ConfigFile "https://locator-service.api.bbci.co.uk/locations?stack=aws&locale=en&filter=international&place-types=settlement,airport,district&order=importance&a=true&format=json"
                           "AGbFAKx58hyjQScCXIYrxuEwJh2W2cmv"
                           "https://weather-broker-cdn.api.bbci.co.uk/en/maps/forecasts-observations?locations="
                           ""
                           "https://weather-broker-cdn.api.bbci.co.uk/en/forecast/aggregated/"
                           Nothing

defaultSummaryDaysValue = 3

data WeatherLocation = WeatherLocation {
  wloc_name :: String,
  wloc_container :: String
                                       }
                       deriving (Eq, Show, Generic)

instance ToJSON WeatherLocation where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 5
    }

instance FromJSON WeatherLocation where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 5
    }

data WeatherGeometry = WeatherGeometry {
  wgeo_type :: String,
  wgeo_coordinates :: [Scientific]
  } deriving (Eq, Show, Generic)

instance ToJSON WeatherGeometry where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 5
    }

instance FromJSON WeatherGeometry where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 5
    }

data WindSpeed = WindSpeed {
  ws_mph :: Maybe Integer,
  ws_kph :: Maybe Integer
  } deriving (Eq, Show, Generic)

instance ToJSON WindSpeed where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 3
    }

instance FromJSON WindSpeed where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3
    }

data WindDirection = WindDirection {
  wd_direction :: String,
  wd_description :: String
  } deriving (Eq, Show, Generic)

instance ToJSON WindDirection where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 3
    }

instance FromJSON WindDirection where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3
    }

data WeatherTemperature = WeatherTemperature {
  temp_c :: Integer,
  temp_f :: Integer
  } deriving (Eq, Show, Generic)

instance ToJSON WeatherTemperature where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 5
    }

instance FromJSON WeatherTemperature where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 5
    }

data WeatherTime = WeatherTime {
  time_utc :: UTCTime,
  time_timezone :: String,
  time_offset :: String
  } deriving (Eq, Show, Generic)

instance ToJSON WeatherTime where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 5
    }

instance FromJSON WeatherTime where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 5
    }

data WeatherData = WeatherData {
  wdata_time :: WeatherTime,
  wdata_temperature :: WeatherTemperature,
  wdata_windDirection :: WindDirection,
  wdata_averageWindSpeed :: WindSpeed,
  wdata_maxWindGustSpeed :: WindSpeed
  } deriving (Eq, Show, Generic)

instance ToJSON WeatherData where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 6
    }

instance FromJSON WeatherData where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 6
    }

data FeatureProperties = FeatureProperties {
  fp_observations :: Maybe [WeatherData],
  fp_forecasts :: Maybe [WeatherData],
  fp_location :: WeatherLocation
  } deriving (Eq, Show, Generic)

instance ToJSON FeatureProperties where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 3
    }

instance FromJSON FeatureProperties where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3
    }

data Features = Features {
  f_type :: String,
  f_id :: String,
  f_geometry :: WeatherGeometry,
  f_properties :: FeatureProperties
  } deriving (Eq, Show, Generic)

instance ToJSON Features where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 2
    }

instance FromJSON Features where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 2
    }

data Weather = Weather {
  w_type :: String,
  w_features :: [Features]
  } deriving (Eq, Show, Generic)

instance ToJSON Weather where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 2
    }

instance FromJSON Weather where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 2
    }

data AggregateWeatherLocation = AggregateWeatherLocation {
  awl_id :: String,
  awl_name :: String,
  awl_container :: String,
  awl_latitude :: Scientific,
  awl_longitude :: Scientific
  } deriving (Eq, Show, Generic)

instance ToJSON AggregateWeatherLocation where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 4
    }

instance FromJSON AggregateWeatherLocation where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 4
    }


data AggregateWeather = AggregateWeather {
  forecasts :: [AggregateWeatherForecast],
  isNight :: Bool,
  issueDate :: ZonedTime,
  lastUpdated :: ZonedTime,
  location :: AggregateWeatherLocation,
  night :: Bool
  } deriving (Show, Generic)

instance ToJSON AggregateWeather where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AggregateWeather


data AggregateWeatherForecast = AggregateWeatherForecast {
  awf_summary :: SummaryForecast,
  awf_detailed :: DetailedForecast
  } deriving (Show, Generic)

instance ToJSON AggregateWeatherForecast where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 4
    }

instance FromJSON AggregateWeatherForecast where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 4
    }


data SummaryReport = SummaryReport {
  sr_enhancedWeatherDescription :: String,
  sr_gustSpeedKph :: Integer,
  sr_gustSpeedMph :: Integer,
  sr_localDate :: Day,
  sr_lowermaxTemperatureC :: Maybe Integer,
  sr_lowermaxTemperatureF :: Maybe Integer,
  sr_lowerminTemperatureC :: Maybe Integer,
  sr_lowerminTemperatureF :: Maybe Integer,
  sr_maxTempC :: Integer,
  sr_maxTempF :: Integer,
  sr_minTempC :: Integer,
  sr_minTempF :: Integer,
  sr_mostLikelyHighTemperatureC :: Integer,
  sr_mostLikelyHighTemperatureF :: Integer,
  sr_mostLikelyLowTemperatureC :: Integer,
  sr_mostLikelyLowTemperatureF :: Integer,
  sr_pollenIndex :: Maybe Integer,
  sr_pollenIndexBand :: Maybe String,
  sr_pollenIndexIconText :: Maybe String,
  sr_pollenIndexText :: Maybe String,
  sr_pollutionIndex :: Maybe Integer,
  sr_pollutionIndexBand :: Maybe String,
  sr_pollutionIndexIconText :: Maybe String,
  sr_pollutionIndexText :: Maybe String,
  sr_precipitationProbabilityInPercent :: Integer,
  sr_precipitationProbabilityText :: String,
  sr_sunrise :: String,
  sr_sunset :: String,
  sr_uppermaxTemperatureC :: Maybe Integer,
  sr_uppermaxTemperatureF :: Maybe Integer,
  sr_upperminTemperatureC :: Maybe Integer,
  sr_upperminTemperatureF :: Maybe Integer,
  sr_uvIndex :: Integer,
  sr_uvIndexBand :: String,
  sr_uvIndexIconText :: String,
  sr_uvIndexText :: String,
  sr_weatherType :: Integer,
  sr_weatherTypeText :: String,
  sr_windDescription :: String,
  sr_windDirection :: String,
  sr_windDirectionAbbreviation :: String,
  sr_windDirectionFull :: String,
  sr_windSpeedKph :: Integer,
  sr_windSpeedMph :: Integer
  } deriving (Eq, Show, Generic)

instance ToJSON SummaryReport where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 3
    }

instance FromJSON SummaryReport where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3
    }


data SummaryForecast = SummaryForecast {
  sf_issueDate :: ZonedTime,
  sf_lastUpdated :: ZonedTime,
  sf_report :: SummaryReport
  } deriving (Show, Generic)

instance ToJSON SummaryForecast where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 3
    }

instance FromJSON SummaryForecast where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3
    }

data DetailedForecast = DetailedForecast {
  df_issueDate :: ZonedTime,
  df_lastUpdated :: ZonedTime,
  df_reports :: [DetailedReport]
  } deriving (Show, Generic)

instance ToJSON DetailedForecast where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 3
    }

instance FromJSON DetailedForecast where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3
    }

data DetailedReport = DetailedReport {
  dr_enhancedWeatherDescription :: String,
  dr_extendedWeatherType :: Integer,
  dr_feelsLikeTemperatureC :: Integer,
  dr_feelsLikeTemperatureF :: Integer,
  dr_gustSpeedKph :: Integer,
  dr_gustSpeedMph :: Integer,
  dr_humidity :: Integer,
  dr_localDate :: Day,
  dr_precipitationProbabilityInPercent :: Integer,
  dr_precipitationProbabilityText :: String,
  dr_pressure :: Integer,
  dr_temperatureC :: Integer,
  dr_temperatureF :: Integer,
  dr_timeslot :: String,
  dr_timeslotLength :: Integer,
  dr_visibility :: String,
  dr_weatherType :: Integer,
  dr_weatherTypeText :: String,
  dr_windDescription :: String,
  dr_windDirection :: String,
  dr_windDirectionAbbreviation :: String,
  dr_windDirectionFull :: String,
  dr_windSpeedKph :: Integer,
  dr_windSpeedMph :: Integer
  } deriving (Eq, Show, Generic)

instance ToJSON DetailedReport where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 3
    }

instance FromJSON DetailedReport where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3
    }

data WaybarJSON = WaybarJSON {
  wj_text :: String,
  wj_alt :: String,
  wj_tooltip :: String
  } deriving (Eq, Show, Generic)

instance ToJSON WaybarJSON where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 3
    }

instance FromJSON WaybarJSON where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3
    }

data BBCLocation = BBCLocation {
  bbcl_id :: String,
  bbcl_name :: String,
  bbcl_container :: String,
  bbcl_containerId :: Integer,
  bbcl_language :: String,
  bbcl_timezone :: String,
  bbcl_country :: String,
  bbcl_latitude :: Scientific,
  bbcl_longitude :: Scientific,
  bbcl_placeType :: String
  } deriving (Eq, Show, Generic)

instance ToJSON BBCLocation where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 5
    }

instance FromJSON BBCLocation where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 5
    }

data BBCLocQuery = BBCLocQuery {
  bbclq_results :: [BBCLocation],
  bbclq_totalResults :: Integer
  } deriving (Eq, Show, Generic)

instance ToJSON BBCLocQuery where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 6
    }

instance FromJSON BBCLocQuery where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 6
    }

data BBCLocResults = BBCLocResults {
  bbclres_results :: BBCLocQuery
  } deriving (Eq, Show, Generic)

instance ToJSON BBCLocResults where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 8
    }

instance FromJSON BBCLocResults where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 8
    }


data BBCLocResponse = BBCLocResponse {
  bbclr_response :: BBCLocResults
  } deriving (Eq, Show, Generic)

instance ToJSON BBCLocResponse where
  toEncoding = genericToEncoding defaultOptions {
    fieldLabelModifier = drop 6
    }

instance FromJSON BBCLocResponse where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 6
    }
