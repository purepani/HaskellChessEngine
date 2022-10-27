{
  description = "Chess Engine";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.hix.url = "github:tek/hix";

  outputs = { self, nixpkgs, flake-utils, haskellNix, hix }:
    #flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      nixpkgs-unfree = import "${nixpkgs}" {
        system = "x86_64-linux";
        config.allowUnFree = true;
      };
      arena = nixpkgs-unfree.arena;
    in   
      hix.lib.flake {
        base = ./.;
        packages = {ChessEngine = ./.;};
        ghcid.shellConfig = {
          buildInputs = [arena];
        };
        #overrides = {buildInputs, super, self...}: {arena = nixpkgs-unfree.arena; };

      }; 

    #  );
}
