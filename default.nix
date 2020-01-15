{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let blessings-package = fetchgit {
    url = "https://cgit.krebsco.de/blessings";
    rev = "v2.2.0";
    sha256 = "1pb56dgf3jj2kq3cbbppwzyg3ccgqy9xara62hkjwyxzdx20clk1";
  };
in haskellPackages.callCabal2nix "zen" ./. {
  blessings = haskellPackages.callCabal2nix "blessings" blessings-package {};
}
