{
  nixpkgs ? import <nixpkgs> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {
    nixpkgs = nixpkgs;
    compiler = compiler;
  },
  compiler ? "ghc96"
}:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  tools = haskell-tools compiler;
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      # Don't check necessarily until we can include non-Haskell stuff for tests
      archery = lib.doBenchmark (lib.dontHaddock (lib.dontCheck (self.callCabal2nix "archery" (gitignore ./.) {})));
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.archery
    ];
    shellHook = ''
      gen-hie > hie.yaml
      for i in $(find -type f | grep -v dist-newstyle); do krank $i; done
    '';
    buildInputs = tools.defaultBuildTools ++ [ nixpkgs.gettext nixpkgs.nodejs_20 nixpkgs.php82 nixpkgs.tinycc ];
    nativeBuildInputs = tools.defaultBuildTools ++ [ nixpkgs.gettext nixpkgs.nodejs_20 nixpkgs.php82 nixpkgs.tinycc ];
    withHoogle = false;
  };
  exe = lib.justStaticExecutables (myHaskellPackages.archery);
in
{
  inherit shell;
  archery = lib.justStaticExecutables (myHaskellPackages.archery);
}
