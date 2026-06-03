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
    sadmin-deploy-src = {
      url = "git+ssh://git@git.i.scalgo.com/scalgo/sadmin-deploy.git?ref=refs/tags/v2.3.2&rev=0eb98a457fb643f3a9d6d18214215dcb4b3605ef";
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

          python312 = prev.python312.override {
            packageOverrides = pfinal: pprev: {
              sadmin-deploy = pfinal.buildPythonPackage rec {
                pname = "sadmin-deploy";
                version = "2.3.2";
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
