{ pkgs ? import ../nix/pkgs.nix { }
}:
let
  my-python-packages = python-packages: with python-packages; [
    selenium
    virtual-display
  ];
  python-with-my-packages = pkgs.python3.withPackages my-python-packages;
in
with pkgs;  [
  python-with-my-packages
  chromedriver
  geckodriver
  chromium
  firefox
]

