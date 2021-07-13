let
  pkgs = import ../nix/pkgs.nix { };
in
pkgs.mkShell {
  name = "webdriver-shell";
  buildInputs = [
    (import ./env.nix { inherit pkgs; })
  ];
}

