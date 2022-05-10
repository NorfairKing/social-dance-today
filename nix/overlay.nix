final: previous:
with final.lib;
with final.haskell.lib;

let
  sources = import ./sources.nix;
in
{
  salsaPartyPackages =
    let
      staticDir = final.gitignoreSource ../static;
      salsaPartyPkg =
        name:
        overrideCabal
          (
            final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}"))
              "--no-hpack"
              { }
          )
          (old: {
            configureFlags = (old.configureFlags or [ ]) ++ [
              # Optimisations
              "--ghc-options=-O2"
              # Extra warnings
              "--ghc-options=-Wall"
              "--ghc-options=-Wincomplete-uni-patterns"
              "--ghc-options=-Wincomplete-record-updates"
              "--ghc-options=-Wpartial-fields"
              "--ghc-options=-Widentities"
              "--ghc-options=-Wredundant-constraints"
              "--ghc-options=-Wcpp-undef"
              "--ghc-options=-Werror"
            ];
            doBenchmark = true;
            doHaddock = false;
            doCoverage = false;
            doHoogle = false;
            doCheck = false; # Only check the release version.
            hyperlinkSource = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
            buildDepends = (old.buildInputs or [ ]) ++ (with final; [
              haskellPackages.autoexporter
            ]);
            preConfigure = (old.preConfigure or "") + ''
              export SALSA_PARTY_STATIC_DIR=${staticDir}
            '';
            # Ugly hack because we can't just add flags to the 'test' invocation.
            # Show test output as we go, instead of all at once afterwards.
            testTarget = (old.testTarget or "") + " --show-details=direct";
          });
      salsaPartyPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (salsaPartyPkg name);
      salsaPartyPkgWithOwnComp = name: salsaPartyPkgWithComp name name;
      withRemoteStaticResources = pkg: resources: overrideCabal pkg (
        old:
        {
          preConfigure =
            let
              copyResource = path: resource:
                ''
                  local path="${path}"
                  mkdir --parents $(dirname "''$path")
                  ln -s ${resource} "''$path"
                '';
              copyScript = concatStringsSep "\n" (mapAttrsToList copyResource resources);
            in
            ''
              ${old.preConfigure or ""}
              ${copyScript}
            '';
        }
      );
      salsa-party-web-server = withRemoteStaticResources (salsaPartyPkg "salsa-party-web-server") {
        "static/bulma.css" = builtins.fetchurl {
          url = "https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css";
          sha256 = "0nbwcsa1gi36f2aq9y96bap7glkp40k3g2bjb9s1vmg0011sri1v";
        };
        "static/sentry.js" = builtins.fetchurl {
          url = "https://browser.sentry-cdn.com/6.19.6/bundle.tracing.min.js";
          sha256 = "sha256:1wzf3l06y5xj56k5p8a0h2yzavkqpz107cxkkmyif86c2qm5sqkr";
        };
        "static/instantpage.js" = builtins.fetchurl {
          url = "https://instant.page/5.1.0";
          sha256 = "sha256:03ryk64a2dxrs65fwpjy2n03nvxd68mdi414pmwd7b7k3lvk8p7s";
        };
      };
      salsa-party-web-server-e2e = salsaPartyPkg "salsa-party-web-server-e2e";
      salsa-party-web-server-gen = salsaPartyPkg "salsa-party-web-server-gen";
      salsa-party-web-server-webdriver = final.haskellPackages.sydtest-webdriver.enableWebdriver (salsaPartyPkg "salsa-party-web-server-webdriver");
    in
    {
      inherit salsa-party-web-server;
      inherit salsa-party-web-server-e2e;
      inherit salsa-party-web-server-gen;
      inherit salsa-party-web-server-webdriver;
    };

  salsaPartyReleasePackages = mapAttrs (_: pkg: justStaticExecutables (doCheck pkg)) final.salsaPartyPackages;

  salsaPartyRelease = final.symlinkJoin {
    name = "salsa-party-release";
    paths = final.lib.attrValues final.salsaPartyReleasePackages;
  };

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super:
                final.salsaPartyPackages // {
                  # base64-bytestring = self.callHackage "base64-bytestring" "1.0.0.3" { };
                  yesod-autoreload = self.callCabal2nix "yesod-autoreload" sources.yesod-autoreload { };
                  # Tests access the internet
                  yesod-static-remote = dontCheck (self.callCabal2nix "yesod-static-remote" sources.yesod-static-remote { });
                  token-limiter-concurrent = self.callCabal2nix "token-limiter-concurrent" sources.token-limiter-concurrent { };
                  iCalendar = self.callCabal2nix "iCalendar" sources.iCalendar { };
                }
            );
      }
    );
}
