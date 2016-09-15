{ mkDerivation, base, containers, data-default, ghcjs-dom, mtl
, reflex, reflex-dom, reflex-dom-contrib, stdenv, text
}:
mkDerivation {
  pname = "reflex-dom-sui";
  version = "0.1.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers data-default ghcjs-dom mtl reflex reflex-dom
    reflex-dom-contrib text
  ];
  homepage = "https://github.com/githubuser/reflex-dom-sui#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
