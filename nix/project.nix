{ inputs, pkgs, lib }:

let
  cabalProject = pkgs.haskell-nix.cabalProject' (
    
    { config, pkgs, ... }:

    {
      name = "ernie";

      compiler-nix-name = lib.mkDefault "ghc984";

      src = lib.cleanSource ../.;

      flake.variants = {
        ghc984 = {}; # Alias for the default variant
        ghc966.compiler-nix-name = "ghc966";
        ghc9102.compiler-nix-name = "ghc9102";
        ghc9122.compiler-nix-name = "ghc9122";
      };

      inputMap = { };

      modules = [{
        packages = {};
      }];
    }
  );

in

cabalProject