#include "/nix/store/n5i1zdpmk2b1s3z96649xh8f9kr3g96s-ghc-8.4.3/lib/ghc-8.4.3/template-hsc.h"
#line 12 "Xcomposite.hsc"
#include <X11/extensions/Xcomposite.h>

int main (void)
{
    hsc_line (1, "Xcomposite.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("{-# LANGUAGE ForeignFunctionInterface #-}\n"
           "", hsc_stdout());
    hsc_fputs ("{-# LANGUAGE CPP #-}\n"
           "module Xcomposite where\n"
           "\n"
           "-- import Graphics.X11.Types (Window (..))\n"
           "import Graphics.X11.Types (Window)\n"
           "import Graphics.X11.Xlib.Types (Display (..))\n"
           "import Foreign.C\n"
           "\n"
           "import System.IO.Unsafe\n"
           "\n"
           "", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_fputs ("\n"
           "foreign import ccall unsafe \"XCompositeGetOverlayWindow\"\n"
           "  -- compositeGetOverlayWindow :: Display -> Window -> Window\n"
           "  compositeGetOverlayWindow :: Display -> Window -> IO Window\n"
           "", hsc_stdout());
    return 0;
}
