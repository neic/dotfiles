{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
        # Applications
        gnupg
        pass

        # Spelling
        aspell
        aspellDicts.da
        aspellDicts.en
        aspellDicts.en-computers
        languagetool

        # Linters
        black
        shellcheck

        # System utils
        colordiff
        coreutils
        gitAndTools.gitFull
        htop
        ncdu
        pv
        pwgen
        silver-searcher
        trash-cli
        tree

        # Network
        httpie
        nmap
        openssh
        rsync
        wget

        # Archiveing
        gzip
        unrar
        unzip
        zstd

        # Media
        atomicparsley
        ffmpeg
        imagemagick
        youtube-dl

        # Infrastructure CLIs
        kubectl
    ];

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  programs.zsh.enable = true;

  nixpkgs.config.allowUnfree = true;
  system.stateVersion = 4;
}
