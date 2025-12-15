{
  description = "neic darwin";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.11-darwin";
    nix-darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-25.11";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    mac-app-util.url = "github:hraban/mac-app-util";
    # nixpkgs-2023-12-26.url =
    #   "github:nixos/nixpkgs/c407032be28ca2236f45c49cfb2b8b3885294f7f";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, mac-app-util,
    # nixpkgs-2023-12-26,
    }: {
      darwinConfigurations."mdm2" = nix-darwin.lib.darwinSystem {
        specialArgs = let system = "aarch64-darwin";
        in {
          #nixpkgs-2023-12-26 = import nixpkgs-2023-12-26 { inherit system; };
        };

        modules = [ mac-app-util.darwinModules.default ./configuration.nix ];
      };
    };
}
