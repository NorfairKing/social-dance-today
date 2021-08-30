let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
pkgs.haskell.lib.buildStackProject {
  name = "salsa-party-nix-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    git
    haskellPackages.autoexporter
    killall
    selenium-server-standalone
    chromium
    chromedriver
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook;
}
