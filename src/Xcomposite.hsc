{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Xcomposite ( compositeRedirectAutomatic
                  , compositeRedirectManual
                  , releaseOverlayWindow
                  , getOverlayWindow
                  , redirectSubwindows
                  ) where

import Graphics.X11.Types (Window)
import Graphics.X11.Xlib.Types (Display (..))
import Foreign.C
import Foreign.C.Types (CInt)

-- import System.IO.Unsafe

#include <X11/extensions/Xcomposite.h>

compositeRedirectAutomatic :: CInt
compositeRedirectAutomatic = 0

compositeRedirectManual :: CInt
compositeRedirectManual = 1

foreign import ccall "XCompositeReleaseOverlayWindow"
  releaseOverlayWindow :: Display -> Window -> IO ()

foreign import ccall "XCompositeGetOverlayWindow"
  getOverlayWindow :: Display -> Window -> IO Window

foreign import ccall "XCompositeRedirectSubwindows"
  redirectSubwindows :: Display -> Window -> CInt -> IO ()
