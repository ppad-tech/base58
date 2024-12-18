{
  description = "Pure Haskell base58 and base58check encodings.";

  inputs = {
    ppad-sha256 = {
      type = "git";
      url  = "git://git.ppad.tech/sha256.git";
      ref  = "master";
    };
    flake-utils.follows = "ppad-sha256/flake-utils";
    nixpkgs.follows = "ppad-sha256/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, ppad-sha256 }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        lib = "ppad-base58";

        pkgs = import nixpkgs { inherit system; };
        hlib = pkgs.haskell.lib;

        sha256 = ppad-sha256.packages.${system}.default;

        hpkgs = pkgs.haskell.packages.ghc981.extend (new: old: {
          ppad-sha256 = sha256;
          ${lib} = new.callCabal2nixWithOptions lib ./. "--enable-profiling" {
            ppad-sha256 = new.ppad-sha256;
          };
        });

        cc    = pkgs.stdenv.cc;
        ghc   = hpkgs.ghc;
        cabal = hpkgs.cabal-install;
      in
        {
          packages.default = hpkgs.${lib};

          devShells.default = hpkgs.shellFor {
            packages = p: [
              (hlib.doBenchmark p.${lib})
            ];

            buildInputs = [
              cabal
              cc
            ];

            inputsFrom = builtins.attrValues self.packages.${system};

            doBenchmark = true;

            shellHook = ''
              PS1="[${lib}] \w$ "
              echo "entering ${system} shell, using"
              echo "cc:    $(${cc}/bin/cc --version)"
              echo "ghc:   $(${ghc}/bin/ghc --version)"
              echo "cabal: $(${cabal}/bin/cabal --version)"
            '';
          };
        }
      );
}

