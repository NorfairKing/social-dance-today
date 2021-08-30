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
              "--ghc-options=-Wall"
              "--ghc-options=-Wincomplete-uni-patterns"
              "--ghc-options=-Wincomplete-record-updates"
              "--ghc-options=-Wpartial-fields"
              "--ghc-options=-Widentities"
              "--ghc-options=-Wredundant-constraints"
              "--ghc-options=-Wcpp-undef"
              "--ghc-options=-Wcompat"
              "--ghc-options=-Werror"
            ];
            doBenchmark = true;
            doHaddock = false;
            doCoverage = false;
            doHoogle = false;
            hyperlinkSource = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
            buildDepends = (old.buildInputs or [ ]) ++ (with final; [
              haskellPackages.autoexporter
            ]);
            preConfigure = (old.preConfigure or "") + ''
              export SALSA_PARTY_STATIC_DIR=${staticDir}
            '';
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
                  cp ${resource} "''$path"
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
          url = "https://browser.sentry-cdn.com/6.10.0/bundle.tracing.min.js";
          sha256 = "sha256:0idyjhnhv6sc6q8mr5f820qcy72s5x4al04llhz5cvgjgixlg3x5";
        };
      };
      salsa-party-web-server-e2e = salsaPartyPkg "salsa-party-web-server-e2e";
      salsa-party-web-server-gen = overrideCabal (salsaPartyPkg "salsa-party-web-server-gen") (old: {
        preConfigure = (old.preConfigure or "") + ''
          # https://github.com/NixOS/nixpkgs/issues/136207
          export FONTCONFIG_FILE=${final.makeFontsConf { fontDirectories = [];}}
        '';
        testDepends = (old.testDepends or [ ]) ++ (with final; [
          chromedriver
          chromium
          selenium-server-standalone
        ]);
      });
    in
    {
      inherit salsa-party-web-server;
      inherit salsa-party-web-server-e2e;
      inherit salsa-party-web-server-gen;
    };

  salsaPartyRelease =
    final.symlinkJoin {
      name = "salsa-party-release";
      paths = final.lib.attrValues final.salsaPartyPackages;
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
                let
                  # envparse
                  envparseRepo =
                    final.fetchFromGitHub {
                      owner = "supki";
                      repo = "envparse";
                      rev = "de5944fb09e9d941fafa35c0f05446af348e7b4d";
                      sha256 = "sha256:0piljyzplj3bjylnxqfl4zpc3vc88i9fjhsj06bk7xj48dv3jg3b";
                    };
                  envparsePkg =
                    dontCheck (
                      self.callCabal2nix "envparse" envparseRepo { }
                    );

                in
                final.salsaPartyPackages // {
                  envparse = envparsePkg;
                  yesod-autoreload = self.callCabal2nix "yesod-autoreload" sources.yesod-autoreload { };
                  # Tests access the internet
                  yesod-static-remote = dontCheck (self.callCabal2nix "yesod-static-remote" sources.yesod-static-remote { });
                  token-limiter-concurrent = self.callCabal2nix "token-limiter-concurrent"
                    (builtins.fetchGit {
                      url = "https://github.com/NorfairKing/token-limiter-concurrent";
                      rev = "336f8bb6d1281c7a11e9b11da9bc1a86b5d89057";
                    })
                    { };
                  iCalendar = self.callCabal2nix "iCalendar"
                    (builtins.fetchGit {
                      url = "https://github.com/NorfairKing/iCalendar";
                      rev = "d8acdad06988e527debef034458ddc74715f77ce";
                    })
                    { };
                }
            );
      }
    );
}
