{ mkDerivation, aeson, base, http-client, http-client-tls, mtl
, servant, servant-client, stdenv, text, time, transformers
, unordered-containers, fetchFromGitHub
}:
mkDerivation {
  pname = "circlehs";
  version = "0.0.3";
  src = fetchFromGitHub {
    owner = "alpmestan";
    repo = "circlehs";
    rev = "3a171aedc1ef613986fbe65fdf3d8512e3d45237";
    sha256 = "01f72vv4339c9mg2i6413lhjz7j356f47q67dbsj8yifkw8m9443";
  };
  libraryHaskellDepends = [
    aeson base http-client http-client-tls mtl servant servant-client
    text time transformers unordered-containers
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/denisshevchenko/circlehs";
  description = "The CircleCI REST API for Haskell";
  license = stdenv.lib.licenses.mit;
}
