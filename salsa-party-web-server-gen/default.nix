{ mkDerivation, aeson, aeson-pretty, base, bytestring, cache
, case-insensitive, containers, criterion, deepseq, esqueleto
, filepath, genvalidity, genvalidity-bytestring
, genvalidity-persistent, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-sydtest-persistent
, genvalidity-text, genvalidity-time, genvalidity-typed-uuid
, http-client, ical, lib, microlens, monad-logger, mtl, password
, path, path-io, path-pieces, persistent, persistent-sqlite
, pretty-show, QuickCheck, salsa-party-data, salsa-party-data-gen
, salsa-party-web-server, shakespeare, sydtest, sydtest-aeson
, sydtest-discover, sydtest-persistent, sydtest-persistent-sqlite
, sydtest-wai, sydtest-yesod, text, time, typed-uuid, uuid, vector
, yesod, yesod-auth, yesod-core
}:
mkDerivation {
  pname = "salsa-party-web-server-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cache filepath genvalidity genvalidity-bytestring
    genvalidity-persistent genvalidity-sydtest genvalidity-time
    genvalidity-typed-uuid http-client microlens monad-logger mtl
    password path-io persistent persistent-sqlite pretty-show
    QuickCheck salsa-party-data salsa-party-data-gen
    salsa-party-web-server sydtest sydtest-persistent-sqlite
    sydtest-wai sydtest-yesod text time yesod yesod-auth
  ];
  testHaskellDepends = [
    aeson aeson-pretty base bytestring cache case-insensitive
    containers esqueleto genvalidity genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-sydtest-persistent
    genvalidity-text genvalidity-time http-client ical monad-logger mtl
    password path path-io path-pieces persistent pretty-show QuickCheck
    salsa-party-data salsa-party-data-gen salsa-party-web-server
    shakespeare sydtest sydtest-aeson sydtest-persistent sydtest-yesod
    text time typed-uuid uuid yesod yesod-auth yesod-core
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base cache containers criterion deepseq genvalidity monad-logger
    persistent QuickCheck salsa-party-data salsa-party-data-gen
    salsa-party-web-server sydtest time vector
  ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
