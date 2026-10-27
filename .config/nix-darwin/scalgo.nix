{ config, lib, pkgs, ... }: {
  system.primaryUser = "md";
  nixpkgs.hostPlatform = "aarch64-darwin";
}
