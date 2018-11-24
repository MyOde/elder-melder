module Meld ( createPrettyImage
            , getRootImage
            , imageDraw
            ) where

import           Control.Monad.Reader    (liftIO)
import           Data.Int                ()
import           Foreign.C.Types         (CChar (..), CUChar (..), CUInt (..),
                                          CULong)
import           Foreign.Marshal.Alloc   (mallocBytes)
import           Foreign.Ptr
import           Foreign.Storable        (sizeOf)
import           Foreign.Storable.Generic (GStorable, peek, poke)
import           Graphics.X11.Types      (Window, zPixmap)
import           Graphics.X11.Xlib.Image (Image, createImage, getImage,
                                          putImage)
import           Graphics.X11.Xlib.Types  (GC, MyImage (..))
import           XDefaultsTransformer    (XDefaultsT, askDisplay, askGc,
                                          askRootHeight, askRootWidth,
                                          askRootWindow, askVisual)

sizeOfCUChar :: Int
sizeOfCUChar = sizeOf $ CUChar 0
sizeOfCChar :: Int
sizeOfCChar = sizeOf $ CChar 0

allPlanes :: CULong
allPlanes = maxBound

prettyHeight, prettyWidth :: Int
prettyHeight = 100
prettyWidth = 100

-- TODO Somehow merge this stuff in wherever You're using it
getRootImage :: XDefaultsT MyImage
getRootImage = do
  display <- askDisplay
  root <- askRootWindow
  width <- askRootWidth
  height <- askRootHeight
  image <- liftIO $ getImage display root 0 0 (CUInt width) (CUInt height) allPlanes zPixmap
  myImage <- liftIO $ peek image
  return myImage

-- TODO maybe try to replace the window type with a drawable?
-- TODO The name does not fully represent the meaning
imageDraw :: Window -> Image -> GC -> XDefaultsT ()
imageDraw drawable image gc = do
  display <- askDisplay
  height <- askRootHeight
  width <- askRootWidth
  liftIO $ putImage display drawable gc image 0 0 0 0 width height

-- TODO Throw this out - this is rather dumb
createPrettyImage :: Ptr CChar -> XDefaultsT (Ptr MyImage)
createPrettyImage dataz = do
  display <- askDisplay
  visual <- askVisual
  -- dataPointer <- liftIO $ mallocBytes $ prettyWidth * prettyHeight * sizeOfCUChar
  image <- liftIO $ createImage
    display
    visual
    8
    zPixmap
    0
    -- dataPointer
    dataz
    (fromIntegral prettyWidth)
    (fromIntegral prettyHeight)
    8
    (fromIntegral $ prettyWidth * sizeOfCChar)

  return $ image
