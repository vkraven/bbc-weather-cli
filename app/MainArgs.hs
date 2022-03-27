{-# LANGUAGE DeriveDataTypeable #-}

module MainArgs where
import System.Console.CmdArgs

version :: String
version = "0.5.0.0"

data BBCCli = Json { format :: String
                   , conf :: String
                   }
            | Config { conf :: String
                     , check :: Bool
                     }
            | Location { search :: String }
            | Cli { conf :: String }
            deriving (Data, Typeable, Show, Eq)

json = Json { format = "waybar" &= help "Specify a specific output format (e.g. waybar)\
                                         \ \nDefaults to format=waybar" &= opt "waybar"
            , conf = confFlag "DEFAULT"
            } &= help "Fetches and prints the weather to stdout" &= auto

confFlag x = x &= help "Path to a custom config file" &= opt "DEFAULT" &= typFile

config = Config { conf = confFlag "DEFAULT"
                , check = def &= help "Check config.json for potential issues"
                } &= help "View and modify bbc-weather-cli's config"

location = Location { search = def &= help "The string to search by\nE.g. --search=\"london\""
                    } &= help "Query the BBC Location API with a search string"

cli = Cli { conf = confFlag "DEFAULT" }

bbccli = cmdArgsMode $ modes [json, config, location, cli]
  &= helpArg [help "Fetch and display the weather, or modify configurations"
             , name "h"]
  &= program "bbc-weather-cli"
  &= summary ("bbc-weather-cli v." ++ version ++ "\nFetches and displays the weather from BBC's Weather API")

runBbccli = cmdArgsRun bbccli >>= print
