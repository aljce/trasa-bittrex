{ package ? "trasa-bittrex", bootstrap ? (import <nixpkgs> {}) }:
let
  trasa-commit = builtins.fromJSON (builtins.readFile ./nix/trasa.json);
  trasa-src = bootstrap.fetchFromGitHub {
    owner = "haskell-trasa";
    repo  = "trasa";
    inherit (trasa-commit) rev sha256;
  };
  trasa-platform = import "${trasa-src}/nix" {};
  nixpkgs = trasa-platform.nixpkgs;
  overrides = trasa-platform.overrides.override {
    overrides = self: super: with nixpkgs.haskell.lib; {
      trasa-bittrex = self.callCabal2nix "trasa-bittrex" ./. {};
    };
  };
  drv = overrides.${package};
in
if nixpkgs.lib.inNixShell then
  drv.env
else
  drv
