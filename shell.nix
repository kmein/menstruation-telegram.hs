{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-qq, base, ConfigFile, containers
      , filepath, hspec, http-client, mtl, QuickCheck, regex-tdfa, safe
      , scientific, servant-client, stdenv, telegram-bot-simple, text
      , time
      }:
      mkDerivation {
        pname = "menstruation";
        version = "0.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base containers safe scientific text
        ];
        executableHaskellDepends = [
          aeson base ConfigFile containers filepath http-client mtl
          regex-tdfa safe scientific servant-client telegram-bot-simple text
          time
        ];
        testHaskellDepends = [ aeson aeson-qq base hspec QuickCheck ];
        doHaddock = false;
        description = "Regel dein Essen";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
