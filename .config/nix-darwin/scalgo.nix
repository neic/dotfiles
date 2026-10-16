{ config, lib, pkgs, ... }: {
  system.primaryUser = "md";
  nixpkgs.hostPlatform = "aarch64-darwin";

  my.pythonPackages = [ pkgs.python314Packages.sadmin-deploy ];
}
