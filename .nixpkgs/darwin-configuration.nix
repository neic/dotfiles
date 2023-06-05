# https://daiderd.com/nix-darwin/manual/index.html

{ config, pkgs, ... }:

{
  imports = [ ~/.nixpkgs/local-configuration.nix ];

  environment.systemPackages = with pkgs; [
        # Applications
        browserpass
        gnupg
        pass

        # Spelling
        (aspellWithDicts (ds: [ ds.en ds.da]))
        languagetool

        # Linters
        black
        pre-commit
        shellcheck
        yamllint

        # LSP
        nodePackages.yaml-language-server
        terraform-lsp

        # System utils
        colordiff
        coreutils
        findutils
        fzf
        gitFull
        htop
        jq
        ncdu_1
        pv
        pwgen
        reuse
        ripgrep
        silver-searcher
        trash-cli
        tree
        yq-go

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
        azure-cli
        fluxcd
        glab
        (google-cloud-sdk.withExtraComponents [
          google-cloud-sdk.components.gke-gcloud-auth-plugin
        ])
        k9s
        kubectl
        kubectx
        sops
        terraform

        # Programming
        colima
        docker-client
        python310
    ];

  homebrew.enable = true;
  homebrew.onActivation.autoUpdate = true;
  homebrew.onActivation.cleanup = "zap";
  homebrew.onActivation.upgrade = true;
  homebrew.taps = [
    "homebrew/cask"
    "homebrew/cask-versions"
    "railwaycat/emacsmacport"
  ];
  homebrew.casks = [
    "google-chrome"
    "firefox"

    "emacs-mac"
    "iterm2"
    "gpg-suite"

    "little-snitch"
    "flux"
    "nextcloud"

    "thunderbird"
    "element"
    "gramps"
    "josm"

    "docker"
    "virtualbox-beta"

    "steam"
    "nvidia-geforce-now"
    "discord"

    "vlc"
    "spotify"

    "wireshark"
  ];
  homebrew.masApps = {
    "Tailscale" = 1475387142;
    "Microsoft Remote Desktop" = 1295203466;
    "iMovie" = 408981434;
    "DaVinci Resolve" = 571213070;
    "Xcode" = 497799835;
  };

  fonts.fontDir.enable = true;
  fonts.fonts = with pkgs; [
    dejavu_fonts
    gyre-fonts
    source-code-pro
  ];

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

  security.pam.enableSudoTouchIdAuth = true;

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  programs.zsh = {
   enable = true;
   enableCompletion = false;  # compinit is called from .zshrc
   enableSyntaxHighlighting = true;
  };

  environment.loginShell = "${pkgs.zsh}/bin/zsh -l";
  environment.variables = {
    SHELL = "${pkgs.zsh}/bin/zsh";
  };

  nixpkgs.config.allowUnfree = true;
  system.stateVersion = 4;

  services.yabai = {
    enable = true;
    config = {
      window_border = "off";
      layout = "bsp";
    };

    extraConfig = ''
      yabai -m rule --add app='System Settings' manage=off
      yabai -m rule --add app=Gramps manage=off
      yabai -m rule --add app=josm manage=off
    '';
  };
}
