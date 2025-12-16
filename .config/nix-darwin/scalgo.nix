{ config, lib, pkgs, ... }: {
  system.primaryUser = "md";
  environment.systemPackages = with pkgs;
    let
      sadmin = pkgs.stdenv.mkDerivation rec {
        pname = "simple-admin";
        version = "v0.1.3";

        src = pkgs.fetchzip {
          url =
            "https://github.com/antialize/simple-admin/releases/download/${version}/sadmin-client-osx.zip";
          sha256 = "sha256-s9XDO67cRVEUfvd5ppSHqYhn9gfmTiY2ej3gDmBrbuI";
        };
        nativeBuildInputs = [ pkgs.installShellFiles ];

        installPhase = ''
          install -D './sadmin' "$out/bin/sadmin"
        '';
      };
    in [ gdal sadmin ];
}
