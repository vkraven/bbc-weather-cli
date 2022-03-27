{ mkDerivation, aeson, aeson-pretty, base, bytestring, cmdargs
, directory, exceptions, http-client, http-client-tls, http-types
, lib, scientific, split, time, transformers, uri-encode
}:
mkDerivation {
  pname = "bbc-weather-cli";
  version = "0.5.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring cmdargs directory exceptions
    http-client http-client-tls http-types scientific split time
    transformers uri-encode
  ];
  homepage = "https://github.com/vkraven/bbc-weather-cli";
  description = "A cli tool to fetch BBC weather reports and forecasts";
  license = lib.licenses.gpl3Plus;
}
