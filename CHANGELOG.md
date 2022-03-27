# Revision history for bbc-weather-cli

## 0.5.0.0 -- 2022-03-27

* Added basic cli mode

## 0.4.0.0 -- 2022-03-22

* Added default parameter for number of summary days to display, `defaultSummaryDays`
* Added cmdArgs for a CLI interface
* Fixed JSON parsing bug where `pollenIndexBand` and `pollutionIndexBand` were being parsed as integers
* Added cli location query options with `bbc-weather-cli loc --search=`
* Added support for custom configuration file locations `bbc-weather-cli json --conf=` and `bbc-weather-cli config --conf=`
