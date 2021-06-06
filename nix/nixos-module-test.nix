{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
}:
let
  salsa-party-production = import (./nixos-module.nix) { envname = "production"; salsaPartyPackages = pkgs.salsaPartyPackages; };
  port = 8001;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "salsa-party-module-test";
    machine = {
      imports = [
        salsa-party-production
      ];
      services.salsa-party.production = {
        enable = true;
        web-server = {
          enable = true;
          inherit port;
        };
      };
    };
    testScript = ''
      from shlex import quote

      machine.start()
      machine.wait_for_unit("multi-user.target")

      machine.wait_for_open_port(${builtins.toString port})
    '';
  }
)
