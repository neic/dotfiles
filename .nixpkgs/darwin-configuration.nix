# https://daiderd.com/nix-darwin/manual/index.html

{ config, pkgs, ... }:
let
  sadmin = pkgs.stdenv.mkDerivation rec {
    pname = "simple-admin";
    version = "v0.0.40";

    src = pkgs.fetchzip {
      url = "https://github.com/antialize/simple-admin/releases/download/${version}/sadmin-client-osx.zip";
      sha256 = "x32lRmzBCIoNLsAT3yLxYNu4VDoG1QS0Q+44J1mZk+s=";
    };
    nativeBuildInputs = [ pkgs.installShellFiles ];

    installPhase = ''
    install -D './sadmin' "$out/bin/sadmin"
    '';
  };
in
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

        # Linters and formatters
        black
        dockfmt
        jsbeautifier
        nixfmt
        nodePackages.stylelint
        pre-commit
        ruff
        rustfmt
        shellcheck
        shfmt
        yamllint

        # LSP
        clang
        nodePackages.bash-language-server
        nodePackages.pyright
        nodePackages.yaml-language-server
        rust-analyzer
        terraform-lsp

        # System utils
        colordiff
        coreutils
        coreutils-prefixed  # for emacs dired
        fd  # emacs
        findutils
        fzf
        gitFull
        gnugrep # emacs
        htop
        jq
        ncdu_1
        pv
        pwgen
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
        glab
        (google-cloud-sdk.withExtraComponents [
          google-cloud-sdk.components.gke-gcloud-auth-plugin
        ])
        k9s
        kubectl
        kubectx
        terraform

        # Programming
        colima
        docker-client
        (python310.withPackages(ps: with ps; [
          openai
          pyflakes
          isort
          (buildPythonPackage rec {
            pname = "sadmin-deploy";
            version = "2.1.0";
            src = builtins.fetchGit {
              url = "git@git.scalgo.com:scalgo/sadmin-deploy.git";
            };
            propagatedBuildInputs = [ requests pyaml ];
          })
        ]))

        # Scalgo
        sadmin
    ];

  homebrew.enable = true;
  homebrew.onActivation.autoUpdate = true;
  homebrew.onActivation.cleanup = "zap";
  homebrew.onActivation.upgrade = true;
  homebrew.taps = [
    "homebrew/cask"
    "homebrew/cask-versions"
  ];
  homebrew.casks = [
    "google-chrome"
    "firefox"

    "iterm2"
    "gpg-suite"

    "little-snitch"
    "flux"
    "nextcloud"
    "karabiner-elements"

    "thunderbird"
    "element"
    "gramps"
    "josm"
    "qgis"

    "virtualbox-beta"

    "steam"
    "discord"

    "vlc"
    "spotify"

    "wireshark"
  ];
  homebrew.masApps = {
    "Tailscale" = 1475387142;
    "Microsoft Remote Desktop" = 1295203466;
    "DaVinci Resolve" = 571213070;
    "Xcode" = 497799835;
  };

  fonts.fontDir.enable = true;
  fonts.fonts = with pkgs; [
    (nerdfonts.override {
      fonts = [
        "IBMPlexMono"
        "NerdFontsSymbolsOnly"
      ]; })
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
      wvous-bl-corner = 1;  #Disabled
      wvous-br-corner = 1;  #Disabled
      wvous-tl-corner = 1;  #Disabled
      wvous-tr-corner = 1;  #Disabled
    };
    finder = {
      AppleShowAllExtensions = true;
      AppleShowAllFiles = true;
      FXPreferredViewStyle = "Nlsv";
      QuitMenuItem = true;
      ShowPathbar = true;
    };
    loginwindow = {
      GuestEnabled = false;
    };
    menuExtraClock = {
      ShowDate = 0;
      ShowSeconds = true;
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
