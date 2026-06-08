{ config, lib, pkgs, ... }: {
  system.primaryUser = "md";
  nixpkgs.hostPlatform = "aarch64-darwin";

  my.pythonPackages = [ pkgs.python312Packages.sadmin-deploy ];

  environment.systemPackages = with pkgs; [ gdal sadmin ];
}
