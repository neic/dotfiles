{
  description = "neic darwin";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.11-darwin";
    nix-darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-25.11";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    mac-app-util.url = "github:hraban/mac-app-util";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, mac-app-util }: {
    darwinConfigurations = {
      "mdm2" = nix-darwin.lib.darwinSystem {
        modules = [
          mac-app-util.darwinModules.default
          ./configuration.nix
          ./private.nix
        ];
      };
      "mdsc" = nix-darwin.lib.darwinSystem {
        modules = [
          mac-app-util.darwinModules.default
          ./configuration.nix
          ./scalgo.nix
        ];
      };
    };
  };
}
