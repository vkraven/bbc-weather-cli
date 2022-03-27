# bbc-weather-cli
A cli tool to fetch weather reports from the BBC's JSON API. 

I originally wrote this for a custom waybar module on my sway wm + waybar desktop. And also because I didn't want to parse that BBC XML RSS feed.

## Using with Waybar

I have an example waybar config in `waybar/example-config.json`. To use `bbc-weather-cli` with waybar, add the `custom/bbcweather` into your waybar config's modules declaration.

## Requirements

A font with adequate icon sets, such as [Nerd Fonts](https://www.nerdfonts.com/).

## Usage

`bbc-weather-cli` prints a waybar-formatted JSON to stdout by default.

``` shell
$ bbc-weather-cli
{"text":"  11","alt":" 11 kph E : 1% | Sunny intervals and light winds","tooltip":"<big>London, Greater London</big>\nNow:  11 kph E : 1% | Sunny intervals and light winds\n\nSunday, 2022-03-27:    H: 18 L: 4 : 1%  | Partly cloudy and a gentle breeze\nMonday, 2022-03-28:    H: 18 L: 9 : 5%  | Light cloud and light winds\nTuesday, 2022-03-29: 殺  H: 11 L: 6 : 64% | Light rain and a gentle breeze\n<small>Last Updated: 17:02:52.807</small>"}
```

To ease setting up location configs, there is also `bbc-weather-cli location --search=`, which returns the BBC location code in the first textual column.

``` shell
$ bbc-weather-cli loc -s london
Total Results:  15
2643743 London, Greater London  Type: settlement
6058560 London, Canada  Type: settlement
2643736 Londonderry, Derry City And Strabane    Type: settlement
...

```

There's also a basic CLI output mode, which prints a basic summary of the current weather report to stdout.

``` shell
$ bbc-weather-cli cli
London, Greater London:   11  11 kph E : 1% | Sunny intervals and light winds
```

To use a custom configuration location, you can add `--conf=path/to/config.json` to the default, json, or cli arg modes.

``` shell
$ bbc-weather-cli json --conf=test-config.json
{"text":"  18","alt":" 10 kph N : 9% | Light cloud and light winds","tooltip":"<big>Melbourne, Australia</big>\nNow:  10 kph N : 9% | Light cloud and light winds\n\nSunday, 2022-03-27:    H: 25 L: 18 : 10% | Light cloud and a gentle breeze\nMonday, 2022-03-28:    H: 25 L: 15 : 15% | Light cloud and a moderate breeze\nTuesday, 2022-03-29:   H: 22 L: 14 : 10% | Light cloud and a moderate breeze\n<small>Last Updated: 03:16:44.396</small>"}

$ bbc-weather-cli cli -ctest.json
Melbourne, Australia:   18  10 kph N : 9% | Light cloud and light winds
```

## Configuration

`bbc-weather-cli` expects a `config.json` file that holds the JSON API web endpoint location, and the weather settlement's BBC location code. It will search for a `config.json` file at the following locations in order:

1. `$XDG_CONFIG_HOME/bbc-weather-cli/config.json` -- (Usually `$HOME/.config/bbc-weather-cli/config.json`)
2. The user data directory -- (Usually `$HOME/.bbc-weather-cli/config.json`)
3. The home directory -- (Usually `$HOME/.bbc-weather-cli-config.json`)

These paths will differ on other OSes (e.g. Windows or MacOS).
If multiple config files exist, the first location will be prioritised. I.e., a config file in `XDG_CONFIG_HOME` will be used even if one exists in user data.

If no config files exist, the program will create one on first run in `XDG_CONFIG_HOME`. This default config will not have a location code - please set the `forecastLocationCode` JSON parameter with a string value of the BBC location code. E.g.:

``` json
{
    "locationEndpoint": ... ,
    ...
    "forecastLocationCode": "2158177" // Set this value 
}
```

## Building

Although Haskell is pure joy to work with, Haskell's build tools are not quite so joyous. Instructions follow for all the Haskell build tools supported.

If you're building on Windows, stack is probably your best bet.

### Nix 

Simple build:

``` shell
$ nix-build release.nix
...
$ ./result/bin/bbc-weather-cli 
```

To install it for your user/system on Nix (single-user) or NixOS, I recommend adding this package to a custom overlay and using `nix-env`, `configuration.nix`, or `home-manager`.

### Cabal

``` shell
$ cabal build ## Builds into the dist-newstyle folder.
              ## Copy the binary into $HOME/.local/bin if desired
```

### Stack

``` shell
$ stack build
$ stack install ## Installs into $HOME/.local/bin by default
```

### Stack + Nix

``` shell
$ stack --nix build
$ stack install
```

Depending on your setup, you may need to enter a `nix-shell` with `pkgs.zlib` as a build input.

### Haskell Platform (NOTE: Deprecated)

``` shell
$ cd app
$ ghc Main.hs -o bbc-weather-cli
```

## Changelog

Please see CHANGELOG.md
