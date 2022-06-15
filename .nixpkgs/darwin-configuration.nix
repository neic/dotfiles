# https://daiderd.com/nix-darwin/manual/index.htm

{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
        # Applications
        gnupg
        pass

        # Spelling
        (aspellWithDicts (ds: [ ds.en ds.da]))
        languagetool

        # Linters
        black
        shellcheck
        yamllint

        # LSP
        nodePackages.yaml-language-server
        terraform-lsp

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
        nmap
        openssh
        rsync
        wget

        # Archiving
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
        ansible
        google-cloud-sdk
        kubectl
        terraform

        # Programming
        cmake
        libtool
        python310
    ];

  fonts.fontDir.enable = true;
  fonts.fonts = with pkgs; [
    dejavu_fonts
    gyre-fonts
    source-code-pro
  ];

  networking.hostName = "mac-n-cheese";

  system.defaults = {
    NSGlobalDomain = {
      # Keyboard
      InitialKeyRepeat = 10;
      KeyRepeat = 2;
    };

    trackpad = {
      ActuationStrength = 0; # Trackpad > Point & Click > Silent clicking = off
      FirstClickThreshold = 0; # Trackpad > Point & Click > Click = Light
      SecondClickThreshold = 0; # Trackpad > Point & Click > Click = Light
    };

    dock = {
      autohide = true;
      orientation = "left";
      tilesize = 24;
      static-only = true;
    };
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  programs.zsh = {
   enable = true;
   enableCompletion = false;  # compinit is called from .zshrc
  };

  nixpkgs.config.allowUnfree = true;
  system.stateVersion = 4;
}
