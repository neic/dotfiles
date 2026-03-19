{ config, lib, pkgs, ... }:

{
  system.primaryUser = "neic";
  nixpkgs.hostPlatform = "aarch64-darwin";
  environment.systemPackages = with pkgs; [ ];
}
