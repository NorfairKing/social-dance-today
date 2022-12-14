{
  description = "salsa-party";
  nixConfig = {
    extra-substituters = "https://social-dance-today.cachix.org";
    extra-trusted-public-keys = "social-dance-today.cachix.org-1:IMOTHXO8rDm0QGlOw6dt170EHEm3a3I6omrWia/Sg1A=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    haskell-dependency-graph-nix.url = "github:NorfairKing/haskell-dependency-graph-nix";
    haskell-dependency-graph-nix.inputs.nixpkgs.follows = "nixpkgs";
    haskell-dependency-graph-nix.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    ical.url = "github:NorfairKing/ical?ref=flake";
    ical.flake = false;
    token-limiter-concurrent.url = "github:NorfairKing/token-limiter-concurrent?ref=flake";
    token-limiter-concurrent.flake = false;
    typed-uuid.url = "github:NorfairKing/typed-uuid?ref=flake";
    typed-uuid.flake = false;
    looper.url = "github:NorfairKing/looper?ref=flake";
    looper.flake = false;
    yesod-autoreload.url = "github:NorfairKing/yesod-autoreload?ref=flake";
    yesod-autoreload.flake = false;
    yesod-static-remote.url = "github:NorfairKing/yesod-static-remote?ref=flake";
    yesod-static-remote.flake = false;
    pretty-relative-time.url = "github:NorfairKing/pretty-relative-time?ref=flake";
    pretty-relative-time.flake = false;
    linkcheck.url = "github:NorfairKing/linkcheck?ref=flake";
    linkcheck.flake = false;
    seocheck.url = "github:NorfairKing/seocheck?ref=flake";
    seocheck.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , haskell-dependency-graph-nix
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    , ical
    , token-limiter-concurrent
    , typed-uuid
    , looper
    , yesod-autoreload
    , yesod-static-remote
    , pretty-relative-time
    , linkcheck
    , seocheck
    , dekking
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (import (validity + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (autodocodec + "/nix/overlay.nix"))
          (import (ical + "/nix/overlay.nix"))
          (import (token-limiter-concurrent + "/nix/overlay.nix"))
          (import (typed-uuid + "/nix/overlay.nix"))
          (import (looper + "/nix/overlay.nix"))
          (import (yesod-autoreload + "/nix/overlay.nix"))
          (import (yesod-static-remote + "/nix/overlay.nix"))
          (import (pretty-relative-time + "/nix/overlay.nix"))
          (import (linkcheck + "/nix/overlay.nix"))
          (import (seocheck + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
          self.overlays.${system}
        ];
      };
      pkgs = pkgsFor nixpkgs;
      mkNixosModule = import ./nix/nixos-module.nix {
        inherit (pkgs) salsaPartyRelease;
        inherit (pkgs.haskellPackages) looper;
      };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.salsaPartyRelease;
      checks.${system} = {
        release = self.packages.${system}.default;
        shell = self.devShells.${system}.default;
        nixos-module-test = import ./nix/nixos-module-test.nix {
          inherit (pkgs) nixosTest;
          salsa-party-nixos-module-factory = self.nixosModuleFactories.${system}.default;
        };
        dependency-graph = haskell-dependency-graph-nix.lib.${system}.makeDependencyGraph {
          packages = builtins.attrNames pkgs.haskellPackages.salsaPartyPackages;
          inherit (pkgs) haskellPackages;
        };
        coverage-report = pkgs.dekking.makeCoverageReport {
          name = "test-coverage-report";
          packages = [
            "salsa-party-data"
            "salsa-party-web-server"
          ];
          coverage = [
            "salsa-party-data-gen"
            "salsa-party-web-server-gen"
            "salsa-party-web-server-e2e"
            "salsa-party-web-server-webdriver"
          ];
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "salsa-party-shell";
        packages = p: builtins.attrValues p.salsaPartyPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          bzip2
          cabal-install
          chromedriver
          chromium
          haskellPackages.eventlog2html
          icu
          niv
          selenium-server-standalone
          zlib
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = ''
          ${self.checks.${system}.pre-commit.shellHook}
          ${pkgs.haskellPackages.sydtest-webdriver.setupFontsConfigScript}
        '';
      };
      nixosModules.${system}.default = mkNixosModule { envname = "production"; };
      nixosModuleFactories.${system}.default = mkNixosModule;
    };
}
