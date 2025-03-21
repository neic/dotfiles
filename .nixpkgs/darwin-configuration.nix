# https://daiderd.com/nix-darwin/manual/index.html

{ config, pkgs, ... }:
let
  sadmin = pkgs.stdenv.mkDerivation rec {
    pname = "simple-admin";
    version = "v0.0.55";

    src = pkgs.fetchzip {
      url =
        "https://github.com/antialize/simple-admin/releases/download/${version}/sadmin-client-osx.zip";
      sha256 = "MGAVq5BD0q5q1gcuTWMPJNaKp4dkaC2uwc/UCvfD8Cc=";
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
    };
    nativeBuildInputs = [ pkgs.installShellFiles ];

    installPhase = ''
      install -D './lib/core.sh' "$out/lib/core.sh"
      install -D './bin/kyrat' "$out/bin/kyrat"
    '';
  };
  pkgs_2023-12-26 = import (builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/c407032be28ca2236f45c49cfb2b8b3885294f7f.tar.gz";
    sha256 = "1a95d5g5frzgbywpq7z0az8ap99fljqk3pkm296asrvns8qcv5bv";
  }) { };
  ruff_0_8_1 = pkgs.runCommandLocal "ruff_0_8_1" { } ''
    mkdir -p $out/bin
    ln -s ${pkgs_2023-12-26.ruff}/bin/ruff $out/bin/ruff081
  '';

  mac-app-util = import (builtins.fetchTarball {
    url =
      "https://github.com/hraban/mac-app-util/archive/548672d0cb661ce11d08ee8bde92b87d2a75c872.tar.gz";
    sha256 = "1w80vjcnaysjlzxsp3v4pxq4yswbjvxs8ann2bk0m7rkjljnzz6m";
  }) { };
in {
  imports =
    [ mac-app-util.darwinModules.default ~/.nixpkgs/local-configuration.nix ];

  environment.systemPackages = with pkgs; [
    # Applications
    browserpass
    discord
    # Install emacs-mac 29.4 from the work branch of Mitsuharu Yamamoto's
    # repo. This code is combined from parts of default.nix and sources.nix
    # from
    # https://github.com/NixOS/nixpkgs/tree/nixos-unstable/pkgs/applications/editors/emacs
    (callPackage # default.nix
      (import
        "${pkgs.path}/pkgs/applications/editors/emacs/make-emacs.nix" { # sources.nix
          pname = "emacs-mac";
          version = "29.4";
          variant = "macport";
          src = pkgs.fetchFromBitbucket { # New code adapted from sources.nix
            owner = "mituharu";
            repo = "emacs-mac";
            rev = "7cc5e67629363d9e98f65e4e652f83bb4e0ee674";
            hash = "sha256-Uv0AX0d5JLgxHlBD70OIDOO/ImMA6hH1fs5hCuMxw7c";
          };
          meta = {
            description = "Emacs with macOS-specific patches";
            license = lib.licenses.gpl3Plus;
            maintainers = [ ];
          };
        }) {
          # default.nix
          inherit (pkgs.darwin) sigtool;
          inherit (pkgs.darwin.apple_sdk.frameworks)
            Accelerate AppKit Carbon Cocoa GSS ImageCaptureCore ImageIO IOKit
            OSAKit Quartz QuartzCore WebKit;
          inherit (pkgs.darwin.apple_sdk_11_0.frameworks)
            UniformTypeIdentifiers;
        })
    gnupg
    iterm2
    josm
    karabiner-elements
    ollama # There is a launchd.user.agents further down.
    pass
    spotify
    wireshark

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
    ruff_0_8_1
    rustfmt
    shellcheck
    shfmt
    yamllint

    # LSP
    clang-tools_17 # clang-format
    clang_17
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
    gdal
    sadmin
  ];

  nixpkgs.config.permittedInsecurePackages = [ "emacs-mac-macport-29.4" ];

  homebrew.enable = true;
  homebrew.onActivation.autoUpdate = true;
  homebrew.onActivation.cleanup = "zap";
  homebrew.onActivation.upgrade = true;
  homebrew.taps = [ "homebrew/cask-versions" ];
  homebrew.casks = [
    "docker" # not in nixpkg
    "element" # nixpkg not build for darwin
    "firefox" # nixpkg not build for darwin
    "flux" # nixpkg not build for darwin
    "google-chrome" # not in nixpkg
    "gpg-suite" # not in nixpkg
    "gramps" # nixpkg does not include .app
    "little-snitch" # nixpkg not build for darwin
    "nextcloud" # nixpkg not build for darwin
    "qgis" # nixpkg not build for darwin
    "steam" # nixpkg not build for darwin
    "virtualbox@beta" # nixpkg not build for darwin
    "vlc" # nixpkg not build for darwin
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
      yabai -m rule --add app=Gramps manage=off
      yabai -m rule --add app=josm manage=off
    '';
  };
}
