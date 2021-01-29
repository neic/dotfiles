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
        google-cloud-sdk
        kubectl
        terraform_0_14
        terraform-lsp

        python38
    ];

  fonts.fonts = with pkgs; [
    dejavu_fonts
    gyre-fonts
    source-code-pro
  ];

  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    config = {
      layout = "bsp";
      window_border = "off";
    };
    extraConfig = "
      yabai -m rule --add app=Gramps manage=off
    ";
  };

  networking.hostName = "mac-n-cheese";

  system.defaults = {
    NSGlobalDomain = {
      # Keyboard
      InitialKeyRepeat = 10;
      KeyRepeat = 1;
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
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  programs.zsh.enable = true;

  nixpkgs.config.allowUnfree = true;
  system.stateVersion = 4;
}
