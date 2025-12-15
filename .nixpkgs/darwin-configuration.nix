# https://nix-darwin.github.io/nix-darwin/manual/index.html

{ config, pkgs, ... }:
{
  imports =
    [
      (import (builtins.fetchTarball {
    url =
      "https://github.com/hraban/mac-app-util/archive/548672d0cb661ce11d08ee8bde92b87d2a75c872.tar.gz";
    sha256 = "1w80vjcnaysjlzxsp3v4pxq4yswbjvxs8ann2bk0m7rkjljnzz6m";
  }) { }).darwinModules.default
      /Users/neic/.nixpkgs/local-configuration.nix
    ];

  environment.systemPackages = with pkgs; let
  sadmin = pkgs.stdenv.mkDerivation rec {
    pname = "simple-admin";
    version = "v0.1.3";

    src = pkgs.fetchzip {
      url =
        "https://github.com/antialize/simple-admin/releases/download/${version}/sadmin-client-osx.zip";
      sha256 = "sha256-s9XDO67cRVEUfvd5ppSHqYhn9gfmTiY2ej3gDmBrbuI";
    };
    nativeBuildInputs = [ pkgs.installShellFiles ];

    installPhase = ''
      install -D './sadmin' "$out/bin/sadmin"
    '';
  };
  kyrat = pkgs.stdenv.mkDerivation rec {
    pname = "kyrat";
    version = "1";

    src = builtins.fetchGit {
      url = "git@github.com:fsquillace/kyrat.git";
      ref = "master";
      rev = "47b57643d4743fe2c1f2bb783ad275e1f0693faf";
    };
    nativeBuildInputs = [ pkgs.installShellFiles ];

    installPhase = ''
      install -D './lib/core.sh' "$out/lib/core.sh"
      install -D './bin/kyrat' "$out/bin/kyrat"
    '';
  }; in [

    # Applications
    browserpass
    discord
    emacs-macport
    gnupg
    iterm2
    josm
    karabiner-elements
    ollama # There is a launchd.user.agents further down.
    pass

    # Spelling
    (aspellWithDicts (ds: [ ds.en ds.da ]))
    languagetool

    # Linters and formatters
    black
    dockfmt
    jsbeautifier
    nixfmt-classic
    nodePackages.stylelint
    pre-commit
    ruff
    rustfmt
    shellcheck
    shfmt
    yamllint

    # LSP
    llvmPackages_18.clang-tools
    llvmPackages_18.clang-unwrapped
    nodePackages.bash-language-server
    nodePackages.yaml-language-server
    pyright
    rust-analyzer
    terraform-lsp

    # System utils
    colordiff
    coreutils
    coreutils-prefixed # for emacs dired
    fd # emacs
    findutils
    fzf
    gitFull
    git-lfs
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
    kyrat
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
    yt-dlp

    # Infrastructure CLIs
    glab
    (google-cloud-sdk.withExtraComponents
      [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
    k9s
    kubectl
    kubectx
    opentofu

    # Programming
    colima
    dive
    docker-client
    (python312.withPackages (ps:
      with ps; [ # Same version as Ubuntu 24.04
        gdal
        isort
        openai
        pyflakes
        (buildPythonPackage rec {
          pname = "sadmin-deploy";
          version = "2.3.2";
          src = builtins.fetchGit {
            url = "git@git.i.scalgo.com:scalgo/sadmin-deploy.git";
          };
          propagatedBuildInputs = [ requests pyaml ];
          pyproject = true;
          build-system = [ setuptools ];
        })
      ]))

    # Scalgo
    gdal
    sadmin
  ];

  homebrew.enable = true;
  homebrew.onActivation.autoUpdate = true;
  homebrew.onActivation.cleanup = "zap";
  homebrew.onActivation.upgrade = true;
  homebrew.taps = [ "homebrew/cask-versions" ];
  homebrew.casks = [
    "darktable"
    "docker-desktop" # not in nixpkg
    "element" # nixpkg not build for darwin
    "firefox" # nixpkg not build for darwin
    "flux-app" # nixpkg not build for darwin
    "google-chrome" # not in nixpkg
    "gpg-suite" # not in nixpkg
    "gramps" # nixpkg does not include .app
    "home-assistant"
    "little-snitch" # nixpkg not build for darwin
    "nextcloud" # nixpkg not build for darwin
    "qgis" # nixpkg not build for darwin
    "spotify" # Download often breaks in nixpkgs
    "steam" # nixpkg not build for darwin
    "vlc" # nixpkg not build for darwin
    "wireshark-app" # qt 6 is broken in nixpkgs
  ];
  homebrew.masApps = {
    "Tailscale" = 1475387142;
    "Windows App" = 1295203466;
    "Xcode" = 497799835;
  };

  fonts.packages = with pkgs; [ nerd-fonts.blex-mono nerd-fonts.symbols-only ];

  launchd.user.agents = {
    ollama-serve = {
      command = "${pkgs.ollama}/bin/ollama serve";
      serviceConfig = {
        KeepAlive = true;
        RunAtLoad = true;
        StandardOutPath = "/tmp/ollama-serve.out.log";
        StandardErrorPath = "/tmp/ollama-serve.err.log";
      };
    };
    org-pull = {
      command = "${pkgs.gitFull}/bin/git -C ~/org pull";
      serviceConfig = {
        StartInterval = 60;
        StandardOutPath = "/tmp/org-pull.out.log";
        StandardErrorPath = "/tmp/org-pull.err.log";
      };
    };
  };

  system.defaults = {
    NSGlobalDomain = {
      # Keyboard
      InitialKeyRepeat = 10;
      KeyRepeat = 2;
      ApplePressAndHoldEnabled = false; # Accent menu
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
      wvous-bl-corner = 1; # Disabled
      wvous-br-corner = 1; # Disabled
      wvous-tl-corner = 1; # Disabled
      wvous-tr-corner = 1; # Disabled
    };
    finder = {
      AppleShowAllExtensions = true;
      AppleShowAllFiles = true;
      FXPreferredViewStyle = "Nlsv";
      QuitMenuItem = true;
      ShowPathbar = true;
    };
    loginwindow = { GuestEnabled = false; };
    menuExtraClock = {
      ShowDate = 0;
      ShowSeconds = true;
    };
  };

  security.pam.services.sudo_local.touchIdAuth = true;

  programs.zsh = {
    enable = true;
    enableCompletion = false; # compinit is called from .zshrc
    enableSyntaxHighlighting = true;
  };

  environment.variables = { SHELL = "${pkgs.zsh}/bin/zsh"; };

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
      yabai -m rule --add title='.*Gramps' manage=off
      yabai -m rule --add app=josm manage=off
    '';
  };
}
