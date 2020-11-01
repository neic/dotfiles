{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
        gitAndTools.gitFull
    ];

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  programs.zsh.enable = true;

  system.stateVersion = 4;
}
