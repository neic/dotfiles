{
  description = "neic darwin";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.11-darwin";
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
  };

  outputs = inputs@{ self, nix-darwin, ... }: {
    darwinConfigurations = {
      "mdm2" = nix-darwin.lib.darwinSystem {
        modules = [
          inputs.mac-app-util.darwinModules.default
          inputs.sops-nix.darwinModules.sops
          ./configuration.nix
          ./private.nix
        ];
      };
      "mdsc" = nix-darwin.lib.darwinSystem {
        modules = [
          inputs.mac-app-util.darwinModules.default
          ./configuration.nix
          ./scalgo.nix
        ];
      };
    };
  };
}
