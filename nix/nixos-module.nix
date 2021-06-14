{ envname, salsaPartyPackages ? (import ./pkgs.nix { }).salsaPartyPackages }:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.salsa-party."${envname}";
  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };

  toYamlFile = pkgs.callPackage ./to-yaml.nix { };
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
                  config =
                    mkOption {
                      description = "The contents of the config file, as an attribute set. This will be translated to Yaml and put in the right place along with the rest of the options defined in this submodule.";
                      default = { };
                    };
                  log-level =                    mkOption {
                      type = types.str;
                      example = "LevelDebug";
                      default = "LevelWarn";
                      description = "The log level to use";
                    };
                  hosts =                    mkOption {
                      type = types.listOf (types.str);
                      example = "salsa-party.cs-syd.eu";
                      default = [];
                      description = "The host to serve web requests on";
                    };
                  port =                    mkOption {
                      type = types.int;
                      example = 8001;
                      description = "The port to serve web requests on";
                    };
                  google-api-key =                         mkOption {
                    type = types.nullOr types.str;
                    example = "XXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXX";
                    default = null;
                    description = "The google API key";
                  };
                  google-analytics-tracking =                         mkOption {
                    type = types.nullOr types.str;
                    example = "X-XXXXXXXXXX";
                    default = null;
                    description = "The google Analytics tracking code";
                  };
                  google-search-console-verification =                         mkOption {
                    type = types.nullOr types.str;
                    example = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXX";
                    default = null;
                    description = "The google Search Console verification code";
                  };
                };
            };
          default = null;
        };
    };
  config =
    let
      nullOrOption =
        name: opt: optionalAttrs (!builtins.isNull opt) { "${name}" = opt; };
      nullOrOptionHead =
        name: opt: optionalAttrs (!builtins.isNull opt && opt != [ ]) { "${name}" = builtins.head opt; };

      web-server-config =with cfg.web-server;
        mergeListRecursively  [
          (nullOrOption "log-level" log-level)
          (nullOrOptionHead "host" hosts)
          (nullOrOption "port" port)
          (nullOrOption "google-api-key" google-api-key)
          (nullOrOption "google-analytics-tracking" google-analytics-tracking)
          (nullOrOption "google-search-console-verification" google-search-console-verification)
          cfg.web-server.config
        ];
      web-server-config-file = toYamlFile "salsa-web-server-config" web-server-config;

      working-dir = "/www/salsa-party/${envname}/";
      # The docs server
      web-server-working-dir = working-dir + "web-server/";
      # The api server
      web-server-service =
        optionalAttrs (cfg.web-server.enable or false) {
          "salsa-party-web-server-${envname}" = {
            description = "Salsa/Party WEB Server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "SALSA_PARTY_WEB_SERVER_CONFIG_FILE" =
                  "${web-server-config-file}";
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
              };
              serverAliases = tail hosts;
            };
        };
    in
    mkIf cfg.enable {
      systemd.services =
        mergeListRecursively [
          web-server-service
        ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional cfg.web-server.enable cfg.web-server.port)
      ];
      services.nginx.virtualHosts =
        mergeListRecursively [
          web-server-host
        ];
    };
}
