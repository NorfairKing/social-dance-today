let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit-hooks = import ./nix/pre-commit.nix { inherit sources; };
in
{
  "release" = pkgs.salsaPartyRelease;
  "pre-commit-hooks" = pre-commit-hooks.run;
  "nixos-module-test" = import ./nix/nixos-module-test.nix {
    inherit sources;
    pkgs = pkgs;
  };
  "nixos-webdriver-test" = import ./nix/nixos-webdriver-test.nix {
    inherit sources;
    pkgs = pkgs;
  };
}
