{
  description = "neic darwin";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.11-darwin";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mac-app-util = {
      url = "github:hraban/mac-app-util";
      inputs.cl-nix-lite.url = "github:r4v3n6101/cl-nix-lite/url-fix";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    kyrat-src = {
      url = "git+ssh://git@github.com/fsquillace/kyrat?ref=master&rev=47b57643d4743fe2c1f2bb783ad275e1f0693faf";
      flake = false;
    };
    claude-agent-acp-src = {
      url =
        "git+ssh://git@github.com/agentclientprotocol/claude-agent-acp?ref=0.40.0&rev=f1736a9371bd2d33e10b2a0b31c65d9c0cf4a18b";
      flake = false;
    };
    sadmin-deploy-src = {
      url = "git+ssh://git@git.i.scalgo.com/scalgo/sadmin-deploy.git?ref=refs/tags/v2.3.3&rev=c03e9964eb002dd7ee1aa5cfabf6fe6ef6dc2ef6";
      flake = false;
    };
    simple-admin-src = {
      url = "https://github.com/antialize/simple-admin/releases/download/v0.1.5/sadmin-client-osx.zip";
      flake = false;
    };
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs-unstable, ... }: {
    darwinConfigurations =
      let
        # Overlay to expose unstable packages and our custom sources
        overlay-unstable = final: prev: {
          unstable = import nixpkgs-unstable {
            inherit (prev.stdenv.hostPlatform) system;
            config.allowUnfree = true;
          };

          kyrat = prev.stdenv.mkDerivation rec {
            pname = "kyrat";
            version = "1";
            src = inputs.kyrat-src;
            nativeBuildInputs = [ prev.installShellFiles ];

            installPhase = ''
              install -D './lib/core.sh' "$out/lib/core.sh"
              install -D './bin/kyrat' "$out/bin/kyrat"
            '';
          };

          claude-agent-acp = final.buildNpmPackage rec {
            pname = "claude-agent-acp";
            version = "0.40.0";
            src = inputs.claude-agent-acp-src;
            npmDepsHash = "sha256-sEZHjBgGUSd37IJV44NYKldyPKRHPfiXz0SPw2W6Zps=";

            meta = {
              description =
                "ACP-compatible coding agent powered by the Claude Agent SDK";
              homepage =
                "https://github.com/agentclientprotocol/claude-agent-acp";
              license = final.lib.licenses.asl20;
              mainProgram = "claude-agent-acp";
            };
          };

          sadmin = prev.stdenv.mkDerivation {
            pname = "simple-admin";
            version = "v0.1.5";
            src = inputs.simple-admin-src;
            nativeBuildInputs = [ prev.installShellFiles ];
            installPhase = ''
              install -D './sadmin' "$out/bin/sadmin"
            '';
          };

          browserpass = import ./browserpass-native-passage.nix {
            pkgs = prev;
            age-with-plugins = import ./age-with-plugins.nix { pkgs = prev; };
          };

          python312 = prev.python312.override {
            packageOverrides = pfinal: pprev: {
              sadmin-deploy = pfinal.buildPythonPackage rec {
                pname = "sadmin-deploy";
                version = "2.3.3";
                src = inputs.sadmin-deploy-src;
                propagatedBuildInputs = [ pfinal.requests pfinal.pyaml ];
                pyproject = true;
                build-system = [ pfinal.setuptools ];
              };
            };
          };
        };

        shared-modules = [
          inputs.mac-app-util.darwinModules.default
          { nixpkgs.overlays = [ overlay-unstable ]; }
          ./configuration.nix
        ];
      in
      {
        "mdm2" = nix-darwin.lib.darwinSystem {
          modules = shared-modules ++ [
            inputs.sops-nix.darwinModules.sops
            ./private.nix
          ];
        };
        "mdsc" = nix-darwin.lib.darwinSystem {
          modules = shared-modules ++ [
            ./scalgo.nix
          ];
        };
      };
  };
}
