# https://nix-darwin.github.io/nix-darwin/manual/index.html

{ config, pkgs, lib, ... }:
{
  options.my.pythonPackages = lib.mkOption {
    type = lib.types.listOf lib.types.package;
    default = [ ];
    description = "List of extra python packages to include in the system python environment.";
  };

  config = {
    my.pythonPackages = with pkgs.python312Packages; [
      gdal
      isort
      openai
      pyflakes
    ];

    environment.systemPackages = with pkgs; [
      # Applications
      browserpass
      discord
      (emacs-macport.overrideAttrs (oldAttrs: {
        env = (oldAttrs.env or { }) // {
          NIX_CFLAGS_COMPILE = (oldAttrs.env.NIX_CFLAGS_COMPILE or "")
            + " -DFD_SETSIZE=10000 -D_DARWIN_UNLIMITED_SELECT";
        };
      }))
      gnupg
      iterm2
      josm
      karabiner-elements
      #ollama # There is a launchd.user.agents further down.
      pass
      sops

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
      copilot-language-server
      llvmPackages_18.clang-tools
      llvmPackages_18.clang-unwrapped
      nodePackages.bash-language-server
      nodePackages.yaml-language-server
      pyright
      rust-analyzer
      terraform-lsp
      ty

      # System utils
      bash # Newer bash for nix-shell
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
      claude-code
      unstable.github-copilot-cli
      dive
      docker-client
      unstable.gemini-cli-bin
      (python312.withPackages (_: config.my.pythonPackages))
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
      "inkscape"
      "little-snitch" # nixpkg not build for darwin
      "nextcloud" # nixpkg not build for darwin
      "qgis" # nixpkg not build for darwin
      "spotify" # Download often breaks in nixpkgs
      "steam" # nixpkg not build for darwin
      "vlc" # nixpkg not build for darwin
      "wireshark-app" # qt 6 is broken in nixpkgs
    ];
    # Use `mas purchase <id>` when this fails.
    homebrew.masApps = {
      "Tailscale" = 1475387142;
      "Windows App" = 1295203466;
      "Xcode" = 497799835;
    };

    fonts.packages = with pkgs; [ nerd-fonts.blex-mono nerd-fonts.symbols-only ];

    launchd.user.agents = {
      # ollama-serve = {
      #   command = "${pkgs.ollama}/bin/ollama serve";
      #   serviceConfig = {
      #     KeepAlive = true;
      #     RunAtLoad = true;
      #     StandardOutPath = "/tmp/ollama-serve.out.log";
      #     StandardErrorPath = "/tmp/ollama-serve.err.log";
      #   };
      # };
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
        TrackpadTwoFingerDoubleTapGesture =
          false; # Trackpad > Scroll & Zoom > Smart zoom = off
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
        mru-spaces = false;
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
    system.stateVersion = 6;
    nix.extraOptions = ''
      experimental-features = nix-command flakes
      extra-platforms = x86_64-darwin aarch64-darwin
    '';

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
  };
}
