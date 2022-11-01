{ mkDerivation, aeson, autodocodec, base, base64-bytestring
, bytestring, case-insensitive, conduit, containers
, cryptohash-sha256, deepseq, file-embed, hashable, http-api-data
, http-types, lib, monad-logger, network-uri, password, path-pieces
, persistent, random, resourcet, shakespeare, template-haskell
, text, text-icu, time, typed-uuid, unliftio, uuid, validity
, validity-bytestring, validity-persistent, validity-text
, validity-time, yaml, yesod, yesod-core, yesod-form
}:
mkDerivation {
  pname = "salsa-party-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec base base64-bytestring bytestring
    case-insensitive conduit containers cryptohash-sha256 deepseq
    file-embed hashable http-api-data http-types monad-logger
    network-uri password path-pieces persistent random resourcet
    shakespeare template-haskell text text-icu time typed-uuid unliftio
    uuid validity validity-bytestring validity-persistent validity-text
    validity-time yaml yesod yesod-core yesod-form
  ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
