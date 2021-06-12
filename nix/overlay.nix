final: previous:
with final.lib;
with final.haskell.lib;

let
  sources = import ./sources.nix;
in
{
  salsaPartyPackages =
    let
      salsaPartyPkg =
        name:
        doBenchmark (
          addBuildDepend
            (
              failOnAllWarnings (
                disableLibraryProfiling (
                  final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }
                )
              )
            )
            (final.haskellPackages.autoexporter)
        );
      salsaPartyPkgWithComp =
        exeName: name:
        generateOptparseApplicativeCompletion exeName (salsaPartyPkg name);
      salsaPartyPkgWithOwnComp = name: salsaPartyPkgWithComp name name;
      withStaticResources = pkg: resources: overrideCabal pkg (
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
      salsa-party-web-server = withStaticResources (salsaPartyPkg "salsa-party-web-server") {
        "static/favicon.ico" = builtins.fetchurl {
          url = "https://cs-syd.eu/logo/res/favicon.ico";
          sha256 = "0ahvcky6lrcpk2vd41558bjgh3x80mpkz4cl7smka534ypm5arz9";
        };
        "static/bulma.css" = builtins.fetchurl {
          url = "https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css";
          sha256 = "0nbwcsa1gi36f2aq9y96bap7glkp40k3g2bjb9s1vmg0011sri1v";
        };
      };
    in
    {
      inherit salsa-party-web-server;
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
                  base16Repo =
                    final.fetchFromGitHub {
                      owner = "emilypi";
                      repo = "base16";
                      rev = "f340b4a9a496320010930368e503ba6b7907f725";
                      sha256 = "sha256:1c6910h9y3nmj2277d7bif3nilgacp4qafl4g5b3r2c0295hbq7z";
                    };
                  base16Pkg = self.callCabal2nix "base16" base16Repo { };

                in
                final.salsaPartyPackages // {
                  envparse = envparsePkg;
                  yesod-autoreload = self.callCabal2nix "yesod-autoreload" sources.yesod-autoreload { };
                  yesod-static-remote = dontCheck (self.callCabal2nix "yesod-static-remote" sources.yesod-static-remote { });
                  token-limiter = unmarkBroken (doJailbreak super.token-limiter);
                  base16 = base16Pkg;
                }
            );
      }
    );
}
