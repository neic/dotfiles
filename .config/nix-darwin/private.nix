{ config, lib, pkgs, ... }:

{
  system.primaryUser = "neic";
  environment.systemPackages = with pkgs; [ ];
}
