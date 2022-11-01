{ mkDerivation, base, criterion, genvalidity
, genvalidity-bytestring, genvalidity-criterion
, genvalidity-persistent, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-sydtest-persistent
, genvalidity-text, genvalidity-time, genvalidity-typed-uuid, lib
, microlens, monad-logger, password, path-pieces, persistent
, persistent-sqlite, QuickCheck, salsa-party-data, sydtest
, sydtest-aeson, sydtest-discover, sydtest-persistent
, sydtest-persistent-sqlite, text, time, unliftio, yesod-form
}:
mkDerivation {
  pname = "salsa-party-data-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-bytestring genvalidity-persistent
    genvalidity-text genvalidity-time genvalidity-typed-uuid microlens
    monad-logger password persistent persistent-sqlite QuickCheck
    salsa-party-data sydtest text yesod-form
  ];
  testHaskellDepends = [
    base genvalidity genvalidity-sydtest genvalidity-sydtest-aeson
    genvalidity-sydtest-persistent path-pieces persistent QuickCheck
    salsa-party-data sydtest sydtest-aeson sydtest-persistent
    sydtest-persistent-sqlite time unliftio
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base criterion genvalidity-criterion salsa-party-data
  ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
