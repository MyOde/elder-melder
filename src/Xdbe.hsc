{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances     #-}
module Xdbe ( undefined
            , background
            , untouched
            , copied
            , xdbeBadBuffer
            , dbeProtocolName
            , dbeMajorVersion
            , dbeMinorVersion
            , dbeNumberEvents
            , dbeBadBuffer
            , dbeNumberEvents
            , BackBuffer
            , SwapAction
            , SwapInfo (..)
            , VisualInfo (..)
            , ScreenVisualInfo (..)
            , queryExtension
            , getVisualInfo
            , allocateBackBufferName
            , deallocateBackBufferName
            , swapBuffers
            ) where

import           Foreign.C.Types          (CInt (..), CUChar (..))
import           Foreign.Marshal.Alloc    (calloc, free)
import           Foreign.Marshal.Array    (newArray)
import           Foreign.Ptr              (Ptr)
import           Prelude                  hiding (undefined)

import           Foreign.Storable.Generic (GStorable, peek, poke)
import           Graphics.X11.Types       (Drawable, VisualID, Window)
import           Graphics.X11.Xlib.Types (Display (..))

import           Generics.Deriving        (Generic)

#include <X11/Xlib.h>
#include <X11/extensions/Xdbe.h>

-- https://opensource.apple.com/source/X11proto/X11proto-57/xextproto/xextproto-7.2.0/dbe.h.auto.html
undefined, background, untouched, copied :: CUChar
undefined = 0
background = 1
untouched = 2
copied = 3

xdbeBadBuffer :: Int
xdbeBadBuffer = 0

dbeProtocolName :: String
dbeProtocolName = "DOUBLE-BUFFER"

dbeMajorVersion, dbeMinorVersion :: Int
dbeMajorVersion = 1
dbeMinorVersion = 0

dbeNumberEvents, dbeBadBuffer, dbeNumberErrors :: Int
dbeNumberEvents = 0
dbeBadBuffer = 0
dbeNumberErrors = dbeBadBuffer + 1


-- https://github.com/cubanismo/libXext/blob/master/include/X11/extensions/Xdbe.h
-- https://github.com/cubanismo/libXext/blob/master/src/Xdbe.c
-- https://tronche.com/gui/x/xlib/utilities/visual.html#XVisualInfo
type BackBuffer = Drawable
type SwapAction = CUChar
data SwapInfo = SwapInfo { swapWindow :: Window
                         , swapAction :: SwapAction
                         } deriving (Generic)

data VisualInfo = VisualInfo { visual    :: VisualID
                  , depth :: CInt
                  , perflevel :: CInt
                             } deriving (Generic)

data ScreenVisualInfo = ScreenVisualInfo { count   :: CInt
                        , visinfo :: Ptr VisualInfo
                                         } deriving (Generic, Show)

instance GStorable SwapInfo
instance GStorable VisualInfo
instance GStorable ScreenVisualInfo

foreign import ccall "XdbeQueryExtension"
  queryExtension' :: Display -> Ptr CInt -> Ptr CInt -> IO ()

queryExtension :: Display -> IO (Int, Int)
queryExtension display = do
  major <- calloc
  minor <- calloc
  queryExtension' display major minor
  majorVersion <- peek major
  minorVersion <- peek minor
  free major >> free minor
  return (fromIntegral majorVersion, fromIntegral minorVersion)

foreign import ccall "XdbeGetVisualInfo"
  getVisualInfo' :: Display -> Ptr Drawable -> Ptr CInt -> IO (Ptr ScreenVisualInfo)

-- TODO Probably need to free the pointer memory
getVisualInfo :: Display -> [Drawable] -> Int -> IO (ScreenVisualInfo, Int)
getVisualInfo display drawables numberOfScreens = do
  drawablePtr <- newArray drawables
  screenPtr <- calloc
  poke screenPtr (CInt $ fromIntegral numberOfScreens)
  (,)
    <$> (peek =<< getVisualInfo' display drawablePtr screenPtr)
    <*> (fromIntegral <$> peek screenPtr)

foreign import ccall "XdbeAllocateBackBufferName"
  allocateBackBufferName :: Display -> Window -> SwapAction -> IO BackBuffer

foreign import ccall "XdbeDeallocateBackBufferName"
  deallocateBackBufferName :: Display -> BackBuffer -> IO ()

foreign import ccall "XdbeSwapBuffers"
  swapBuffers' :: Display -> Ptr SwapInfo -> CInt -> IO ()

-- TODO Probably need to free the pointer memory
swapBuffers :: Display -> [SwapInfo] -> IO ()
swapBuffers display swapInfos = do
  swapInfoPtr <- newArray swapInfos
  swapBuffers' display swapInfoPtr (CInt $ fromIntegral $ length swapInfos)
