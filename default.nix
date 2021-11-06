{ pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/75f4ba05c63be3f147bcc2f7bd4ba1f029cedcb1.tar.gz") {} }:
with pkgs;
haskellPackages.developPackage {
  root = ./.;
  source-overrides.blessings = fetchgit {
    url = "https://cgit.krebsco.de/blessings";
    rev = "v2.2.0";
    sha256 = "1pb56dgf3jj2kq3cbbppwzyg3ccgqy9xara62hkjwyxzdx20clk1";
  };
  modifier = drv: haskell.lib.addBuildTools drv (with haskellPackages; [
    cabal-install
    ghcid
    (hoogleLocal { packages = drv.propagatedBuildInputs; })
  ]);
}
