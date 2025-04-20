{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [inputs.emacs-overlay.overlay];
      };
      emacsPackages = pkgs.emacsPackagesFor pkgs.emacs;
      emacs = emacsPackages.emacsWithPackages (epkgs: with epkgs; [
        dash
        s
        f
        ht
        request
        xmlgen
        elfeed
      ]);
      hackernews-src = pkgs.stdenv.mkDerivation {
        name = "hackernews-src";
        src = ./.;
        phases = [ "unpackPhase" "installPhase" ];
        installPhase = ''
          mkdir -p $out
          cp mrgreen.png main.css feeds.eld hn.el $out
        '';
      };
      hackernews = pkgs.writeShellScriptBin "hackernews" ''
        export PATH=${pkgs.curl}/bin:$PATH
        curl https://httpstat.us/200
        ${emacs}/bin/emacs --fg-daemon=hackernews --quick --load "${hackernews-src}/hn.el" --eval '(hn/main)'
      '';
      jackin = pkgs.writeShellScriptBin "hackernews-jackin" ''
        ${emacs}/bin/emacsclient --socket-name=hackernews --create-frame --eval
      '';
      hackernewsModule = { config, lib, ... }:
        let
          cfg = config.colonq.services.hackernews;
        in {
          options.colonq.services.hackernews = {
            enable = lib.mkEnableOption "Enable the Hacker News server";
            dataDir = lib.mkOption {
              type = lib.types.path;
              default = "/var/lib/hackernews";
              description = "Path to store feed cache";
            };
          };
          config = lib.mkIf cfg.enable {
            users.users.hackernews = {
              isSystemUser = true;
              group = "hackernews";
            };
            users.groups.hackernews = {};
            systemd.services."colonq.hackernews" = {
              wantedBy = ["multi-user.target"];
              serviceConfig = {
                User = "hackernews";
                RuntimeMaxSec = 21600;
                Restart = "always";
                Environment = "HACKERNEWS_DATA_DIR=${cfg.dataDir}";
                ExecStart = "${hackernews}/bin/hackernews";
              };
            };
          };
        };
    in {
      devShells.x86_64-linux.default = pkgs.mkShell {
        buildInputs = [
          emacs
        ];
      };
      packages.x86_64-linux = {
        inherit emacs hackernews jackin;
        default = hackernews;
      };
      nixosModules = {
        hackernews = hackernewsModule;
      };
      overlay = self: super: {
        hackernews = {
          inherit emacs hackernews jackin;
        };
      };
    };
}
