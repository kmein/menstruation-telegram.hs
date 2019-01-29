{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, ConfigFile, containers, filepath
      , http-api-data, http-client, http-client-tls, mtl, regex-tdfa
      , safe, servant-client, stdenv, telegram-bot-simple, text, time
      }:
      mkDerivation {
        pname = "menstruation-telegram";
        version = "0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base ConfigFile containers filepath http-api-data http-client
          http-client-tls mtl regex-tdfa safe servant-client
          telegram-bot-simple text time
        ];
        description = "Regel dein Essen. (Telegram bot)";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
