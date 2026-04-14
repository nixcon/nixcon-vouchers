{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    servant-effectful = {
      url = "github:Kleidukos/servant-effectful";
      flake = false;
    };
    wai-middleware-auth = {
      url = "github:ners/wai-middleware-auth";
      flake = false;
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      pname = "nixcon-vouchers";
      overlay = lib.composeManyExtensions [
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (hfinal: hprev: with prev.haskell.lib.compose; {
                ${pname} = hfinal.callCabal2nix pname ./. { };
                haxl = (unmarkBroken hprev.haxl).overrideAttrs {
                  postPatch = "rm Setup.hs";
                };
                servant-effectful = hfinal.callCabal2nix "servant-effectful" inputs.servant-effectful { };
                wai-middleware-auth = hfinal.callCabal2nix "wai-middleware-auth" inputs.wai-middleware-auth { };
                wreq-effectful = doJailbreak (unmarkBroken hprev.wreq-effectful);
              })
            ];
          };
        })
      ];
    in
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps = with lib; foldlAttrs
            (acc: name: hp':
              let
                hp = tryEval hp';
                version = getVersion hp.value.ghc;
                majorMinor = versions.majorMinor version;
                ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
              in
              if hp.value ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.4" && versionOlder version "9.13"
              then acc // { ${ghcName} = hp.value; }
              else acc
            )
            { default = pkgs.haskellPackages; }
            pkgs.haskell.packages;
        in
        {
          legacyPackages.${system} = pkgs;
          packages.${system}.default = hps.default.${pname};
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: [ hp.${pname} ];
                nativeBuildInputs = with pkgs; [
                  dhall-lsp-server
                  haskellPackages.cabal-gild
                  haskellPackages.cabal-install
                  haskellPackages.fourmolu
                  hp.haskell-language-server
                  nixpkgs-fmt
                  treefmt
                ];
              };
            });
          formatter.${system} = pkgs.treefmt;
        }
      ) // {
      overlays.default = overlay;
      nixosModules.default = { config }:
        let
          cfg = config.services.nixcon-contributor-verify;
        in
        {
          options = { };
          config = lib.mkIf cfg.enable { };
        };
    };
}
