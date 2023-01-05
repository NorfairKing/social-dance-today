{ mkDerivation, aeson, aeson-pretty, amazonka, amazonka-ses
, autodocodec, autodocodec-yaml, base, blaze-html, bytestring
, cache, clock, conduit, containers, data-default, deepseq
, edit-distance, envparse, esqueleto, file-embed, hashable
, http-client, http-client-tls, http-date, http-types, ical
, JuicyPixels, JuicyPixels-extra, lib, looper, microlens
, monad-logger, mtl, network-uri, optparse-applicative, path
, path-io, persistent, persistent-pagination, persistent-sqlite
, pretty-relative-time, pretty-show, random, random-shuffle
, resourcet, retry, safe, safe-coloured-text, salsa-party-data
, scalpel, scientific, shakespeare, sydtest, sydtest-discover
, tagsoup, template-haskell, text, time, token-limiter-concurrent
, typed-process, typed-uuid, unliftio, validity, validity-text
, validity-time, vector, vector-algorithms, wai-extra, warp
, xml-conduit, xml-types, yesod, yesod-auth, yesod-autoreload
, yesod-core, yesod-sitemap, yesod-static, yesod-static-remote
, zip-archive
}:
mkDerivation {
  pname = "salsa-party-web-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty amazonka amazonka-ses autodocodec
    autodocodec-yaml base blaze-html bytestring cache clock conduit
    containers data-default deepseq edit-distance envparse esqueleto
    file-embed hashable http-client http-client-tls http-date
    http-types ical JuicyPixels JuicyPixels-extra looper microlens
    monad-logger mtl network-uri optparse-applicative path path-io
    persistent persistent-pagination persistent-sqlite
    pretty-relative-time pretty-show random random-shuffle resourcet
    retry safe safe-coloured-text salsa-party-data scalpel scientific
    shakespeare tagsoup template-haskell text time
    token-limiter-concurrent typed-process typed-uuid unliftio validity
    validity-text validity-time vector vector-algorithms wai-extra warp
    xml-conduit xml-types yesod yesod-auth yesod-autoreload yesod-core
    yesod-sitemap yesod-static yesod-static-remote zip-archive
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring containers path path-io sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "salsa-party-web-server";
}
