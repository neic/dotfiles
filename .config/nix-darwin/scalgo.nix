{ config, lib, pkgs, ... }: {
  system.primaryUser = "md";
  nixpkgs.hostPlatform = "aarch64-darwin";

  my.pythonPackages = [ pkgs.python312Packages.sadmin-deploy ];

  environment.systemPackages = with pkgs;
    let
      sadmin = pkgs.stdenv.mkDerivation rec {
        pname = "simple-admin";
        version = "v0.1.5";

        src = pkgs.fetchzip {
          url =
            "https://github.com/antialize/simple-admin/releases/download/${version}/sadmin-client-osx.zip";
          sha256 = "sha256-iR9qyj72KmUtM/1WyV8ILG/bzJnVow4ZUOlCuVoqMOU=";
        };
        nativeBuildInputs = [ pkgs.installShellFiles ];

        installPhase = ''
          install -D './sadmin' "$out/bin/sadmin"
        '';
      };
    in [ gdal sadmin ];
}
