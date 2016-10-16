{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, bytestring, graphql
      , QuickCheck, stdenv, tasty, tasty-hspec, tasty-hunit
      , tasty-quickcheck, text
      }:
      mkDerivation {
        pname = "typed-graphql";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson attoparsec base bytestring graphql text
        ];
        testHaskellDepends = [
          aeson attoparsec base bytestring graphql QuickCheck tasty
          tasty-hspec tasty-hunit tasty-quickcheck text
        ];
        description = "Servant-style combinators for graphql";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
