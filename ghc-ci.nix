{ mkDerivation, aeson, base, circlehs, directory, filepath
, http-api-data, mtl, optparse-applicative, process, servant
, servant-client, servant-server, stdenv, temporary, text
, transformers, unordered-containers, wai, warp
}:
mkDerivation {
  pname = "ghc-ci";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base circlehs http-api-data servant text
  ];
  executableHaskellDepends = [
    aeson base circlehs directory filepath mtl optparse-applicative
    process servant-client servant-server temporary text transformers
    unordered-containers wai warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
