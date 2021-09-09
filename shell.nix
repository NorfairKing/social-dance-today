let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
  fontconfigDir = pkgs.callPackage ./nix/fonts-conf.nix { };
in
pkgs.haskell.lib.buildStackProject {
  name = "salsa-party-nix-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    bzip2
    chromedriver
    chromium
    git
    haskellPackages.autoexporter
    killall
    selenium-server-standalone
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook + ''
    export FONTCONFIG_SYSROOT=${fontconfigDir}
  '';
}
