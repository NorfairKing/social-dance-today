{ mkDerivation, base, bytestring, genvalidity-sydtest, http-client
, ical, lib, linkcheck, monad-logger, network-uri, salsa-party-data
, salsa-party-web-server, salsa-party-web-server-gen, scalpel
, seocheck, sydtest, sydtest-discover, sydtest-yesod, text, time
, yesod-auth
}:
mkDerivation {
  pname = "salsa-party-web-server-e2e";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring genvalidity-sydtest ical linkcheck monad-logger
    network-uri salsa-party-data salsa-party-web-server
    salsa-party-web-server-gen scalpel seocheck sydtest sydtest-yesod
    text time yesod-auth
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base http-client salsa-party-web-server-gen sydtest sydtest-yesod
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "salsa-party-web-server-e2e";
}
