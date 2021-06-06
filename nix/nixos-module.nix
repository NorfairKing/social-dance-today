{ envname, salsaPartyPackages ? (import ./pkgs.nix { }).salsaPartyPackages }:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.salsa-party."${envname}";
  concatAttrs = attrList: fold (x: y: x // y) { } attrList;
in
{
  options.services.salsa-party."${envname}" =
    {
      enable = mkEnableOption "Salsa/Party Service";
      web-server =
        mkOption {
          type =
            types.submodule {
              options =
                {
                  enable = mkEnableOption "Salsa/Party WEB Server";
                  log-level =
                    mkOption {
                      type = types.str;
                      example = "LevelDebug";
                      default = "LevelWarn";
                      description = "The log level to use";
                    };
                  hosts =
                    mkOption {
                      type = types.listOf (types.str);
                      example = "salsa-party.cs-syd.eu";
                      description = "The host to serve web requests on";
                    };
                  port =
                    mkOption {
                      type = types.int;
                      example = 8001;
                      description = "The port to serve web requests on";
                    };
                };
            };
          default = null;
        };
    };
  config =
    let
      working-dir = "/www/salsa-party/${envname}/";
      # The docs server
      web-server-working-dir = working-dir + "web-server/";
      # The api server
      web-server-service =
        with cfg.web-server;
        optionalAttrs enable {
          "salsa-party-web-server-${envname}" = {
            description = "Salsa/Party WEB Server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "SALSA_PARTY_WEB_SERVER_LOG_LEVEL" =
                  "${builtins.toString log-level}";
                "SALSA_PARTY_WEB_SERVER_PORT" =
                  "${builtins.toString port}";
              };
            script =
              ''
                mkdir -p "${web-server-working-dir}"
                cd ${web-server-working-dir};
                ${salsaPartyPackages.salsa-party-web-server}/bin/salsa-party-web-server
              '';
            serviceConfig =
              {
                Restart = "always";
                RestartSec = 1;
                Nice = 15;
              };
            unitConfig =
              {
                StartLimitIntervalSec = 0;
                # ensure Restart=always is always honoured
              };
          };
        };
      web-server-host =
        with cfg.web-server;

        optionalAttrs (enable && hosts != [ ]) {
          "${head hosts}" =
            {
              enableACME = true;
              forceSSL = true;
              locations."/" = {
                proxyPass = "http://localhost:${builtins.toString port}";
                # Just to make sure we don't run into 413 errors on big syncs
                extraConfig = ''
                  client_max_body_size 0;
                '';
              };
              serverAliases = tail hosts;
            };
        };
    in
    mkIf cfg.enable {
      systemd.services =
        concatAttrs [
          web-server-service
        ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional cfg.web-server.enable cfg.web-server.port)
      ];
      services.nginx.virtualHosts =
        concatAttrs [
          web-server-host
        ];
    };
}
