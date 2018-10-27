# {ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "elder-melder-env";
  buildInputs =
              [
                xorg.libXrandr
                xorg.libXScrnSaver
                xorg.libXext
                xorg.libX11
                # xorg.libXcomposite
              ];
  depsBuildBuild = [ xorg.libXcomposite ];
  # nativeBuildInputs = [ xorg.libXcomposite ];
  # unpackPhase = ''
  #   echo "BUILDING ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  # '';
}
