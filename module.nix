{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.ghc-ci;
in {
  options.services.ghc-ci = {
    enable = mkEnableOption "GHC CI Server";
    port = mkOption {
      default = 8888;
      type = types.nullOr types.int;
      description = "Port to listen to for incoming build requests";
    };
    github-user = mkOption {
      default = "ghc";
      type = types.nullOr types.str;
      description = "Github username of the staging repo";
    };
    github-repo = mkOption {
      default = "ghc-diffs";
      type = types.nullOr types.str;
      description = "Github project name of the staging repo";
    };
    circleci-token-file = mkOption {
      type = types.path;
      description = "Path to a file containing the CircleCI API token";
    };
    github-push-keyfile = mkOption {
      type = types.path;
      description = "Path to the github key used for pushing to the staging repo";
    };
    workdir = mkOption {
      type = types.str;
      description = "Directory in which to clone GHC";
    };
    gitlab-url = mkOption {
      type = types.str;
      default = "https://gitlab.staging.haskell.org";
      description = "Root URL for the gitlab instance";
    };
    pkg = mkOption {
      type = types.package;
      description = "ghc-ci package to use";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.ghc-ci = {
      description = "GHC CI server";
      after = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.gitAndTools.git ];
      serviceConfig = {
        ExecStart = ''
          ${cfg.pkg}/bin/ghc-ci-server \
              --user=${cfg.github-user} \
              --project=${cfg.github-repo} \
              --circleci=${cfg.circleci-token-file} \
              --key=${cfg.github-push-keyfile} \
              --repos=${cfg.workdir} \
              --gitlab=${cfg.gitlab-url} \
              --port=${toString cfg.port}
        '';
        Type = "simple";
        User = "ghcci";
        Group = "ghcci";
        Restart = "on-failure";
      };
    };

    users.groups.ghcci = {};

    users.users.ghcci = {
      description = "GHC CI server";
      createHome = true;
      group = "ghcci";
      home = cfg.workdir;
    };
  };
}
