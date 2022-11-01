final: prev:
with final.lib;
with final.haskell.lib;

{

  salsaPartyReleasePackages = mapAttrs (_: pkg: justStaticExecutables (doCheck pkg)) final.haskellPackages.salsaPartyPackages;

  salsaPartyRelease = final.symlinkJoin {
    name = "salsa-party-release";
    paths = final.lib.attrValues final.salsaPartyReleasePackages;
  };

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        let
          salsaPartyPackages =
            let
              salsaPartyPkg =
                name:
                overrideCabal (self.callPackage (../${name}) { })
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
                    doCheck = false; # Only check the release version.
                    enableLibraryProfiling = false;
                    enableExecutableProfiling = false;
                    buildDepends = (old.buildInputs or [ ]) ++ (with final; [
                      haskellPackages.autoexporter
                    ]);
                    preConfigure = (old.preConfigure or "") + ''
                      export SALSA_PARTY_STATIC_DIR=${../static}
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
            in
            {
              salsa-party-data = salsaPartyPkg "salsa-party-data";
              salsa-party-data-gen = salsaPartyPkg "salsa-party-data-gen";
              salsa-party-web-server = withRemoteStaticResources (salsaPartyPkg "salsa-party-web-server") {
                "static/bulma.css" = builtins.fetchurl {
                  url = "https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css";
                  sha256 = "0nbwcsa1gi36f2aq9y96bap7glkp40k3g2bjb9s1vmg0011sri1v";
                };
                "static/sentry.js" = builtins.fetchurl {
                  url = "https://browser.sentry-cdn.com/7.12.1/bundle.tracing.min.js";
                  sha256 = "sha256:08i3w728xdlzyvq4j9k1sm3ydfc6ch61cgs0wxvxqhc002vph5fa";
                };
                "static/instantpage.js" = builtins.fetchurl {
                  url = "https://instant.page/5.1.0";
                  sha256 = "sha256:03ryk64a2dxrs65fwpjy2n03nvxd68mdi414pmwd7b7k3lvk8p7s";
                };
              };
              salsa-party-web-server-e2e = salsaPartyPkg "salsa-party-web-server-e2e";
              salsa-party-web-server-gen = salsaPartyPkg "salsa-party-web-server-gen";
              salsa-party-web-server-webdriver = final.haskellPackages.sydtest-webdriver.enableWebdriver (salsaPartyPkg "salsa-party-web-server-webdriver");
            };
        in
        {
          inherit salsaPartyPackages;
        } // salsaPartyPackages
    );
  });
}
