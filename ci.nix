let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
{
  "release" = pkgs.salsaPartyRelease;
  "pre-commit-hooks" = pre-commit.run;
  "hoogle" = pkgs.salsaPartyHoogle;
  "nixos-module-test" = import ./nix/nixos-module-test.nix {
    inherit sources;
    pkgs = pkgs;
  };
}
