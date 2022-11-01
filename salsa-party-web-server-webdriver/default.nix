{ mkDerivation, aeson, base, bytestring, genvalidity-sydtest, lib
, mtl, password, persistent, pretty-show, QuickCheck
, salsa-party-data, salsa-party-web-server
, salsa-party-web-server-gen, sydtest, sydtest-discover
, sydtest-webdriver, sydtest-webdriver-screenshot
, sydtest-webdriver-yesod, text, time, typed-uuid, uuid, webdriver
, yesod
}:
mkDerivation {
  pname = "salsa-party-web-server-webdriver";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring mtl persistent salsa-party-data
    salsa-party-web-server salsa-party-web-server-gen sydtest
    sydtest-webdriver sydtest-webdriver-screenshot
    sydtest-webdriver-yesod text time webdriver yesod
  ];
  testHaskellDepends = [
    base genvalidity-sydtest mtl password persistent pretty-show
    QuickCheck salsa-party-data salsa-party-web-server
    salsa-party-web-server-gen sydtest sydtest-webdriver
    sydtest-webdriver-screenshot sydtest-webdriver-yesod time
    typed-uuid uuid webdriver yesod
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
