let
  pkgs = import ../nix/pkgs.nix { };
  my-python-packages = python-packages: with python-packages; [
    selenium
    virtual-display
  ];
  python-with-my-packages = pkgs.python3.withPackages my-python-packages;
in
pkgs.mkShell {
  name = "webdriver-shell";
  buildInputs = with pkgs; [
    python-with-my-packages
    chromedriver
    geckodriver
    chromium
    firefox
  ];
}

