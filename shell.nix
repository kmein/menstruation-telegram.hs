{ pkgs ? import <nixpkgs> {} }:
with pkgs;
stdenv.mkDerivation {
  name = "menstruation-telegram.hs";
  MENSTRUATION_ENDPOINT = "http://0.0.0.0:49080";
  MENSTRUATION_TOKEN = builtins.readFile ./Tokenfile;
}
