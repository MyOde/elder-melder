{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Xdbe where

import           Foreign.C
import           Foreign.C.Types         (CUChar (..), CInt (..))
import           Foreign.Ptr             (Ptr (..))
import           Graphics.X11.Types      (Drawable, Window, VisualID)
import           Graphics.X11.Xlib.Types (Display (..))

#include <X11/extensions/Xdbe.h>

-- https://opensource.apple.com/source/X11proto/X11proto-57/xextproto/xextproto-7.2.0/dbe.h.auto.html
undefined = 0
background = 1
untouched = 2
copied = 3

xdbeBadBuffer = 0

dbeProtocolName = "DOUBLE-BUFFER"

dbeMajorVersion = 1
dbeMinorVersion = 0

dbeNumberEvents = 0
dbeBadBuffer = 0
dbeNumberErrors = dbeBadBuffer + 1


-- https://github.com/cubanismo/libXext/blob/master/include/X11/extensions/Xdbe.h
-- https://github.com/cubanismo/libXext/blob/master/src/Xdbe.c
-- https://tronche.com/gui/x/xlib/utilities/visual.html#XVisualInfo
data BackBuffer = Drawable
data SwapAction = CUChar
data SwapInfo = SwapInfo { swapWindow :: Window
                         , swapAction :: SwapAction
                         }

data VisualInfo = { visual :: VisualID
                  , depth :: CInt
                  , perflevel :: CInt
                  }

data ScreenVisualInfo = { count :: CInt
                        -- , visinfo :: VisualInfo
                        , visinfo :: Ptr VisualInfo
                        }

foreign import ccall "XdbeQueryExtension"
  queryExtension :: Display -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall "XdbeGetVisualInfo"
  getVisualInfo :: Display -> [Drawable] -> Ptr CInt -> IO ScreenVisualInfo

foreign import ccall "XdbeAllocateBackBufferName"
  allocateBackBufferName :: Display -> Window -> SwapAction -> IO BackBuffer

foreign import ccall "XdbeDeallocateBackBufferName"
  allocateBackBufferName :: Display -> BackBuffer -> IO ()

foreign import ccall "XdbeSwapBuffers"
  swapBuffers :: Display -> Ptr SwapInfo -> CInt -> IO ()
