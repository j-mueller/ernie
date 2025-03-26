{ repoRoot, inputs, pkgs, lib, system }:

let
  sha256map = {
  };

  modules = [{ }];

  cabalProject = pkgs.haskell-nix.cabalProject' {
    inherit modules sha256map;
    src = ../.;
    name = "ernie";
    compiler-nix-name = "ghc982";
    # index-state = "2024-10-16T00:00:00Z";
    inputMap = {
    };
    shell.withHoogle = false;
  };

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in
project