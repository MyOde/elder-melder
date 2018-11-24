module Lib where

import Control.Concurrent (threadDelay)
import           Control.Monad.Reader      (liftIO, runReaderT)
import           Foreign.Marshal.Alloc     (mallocBytes)
import           Graphics.X11.Types        (substructureNotifyMask)
import           Graphics.X11.Xlib.Context (createGC, freeGC, setBackground,
                                            setForeground)
import           Graphics.X11.Xlib.Display (blackPixel, closeDisplay,
                                            defaultColormap, defaultGC,
                                            defaultRootWindow, defaultScreen,
                                            defaultVisual, openDisplay,
                                            whitePixel)
import           Graphics.X11.Xlib.Event   (XEvent (..), XEventPtr,
                                            allocaXEvent, nextEvent)
import Graphics.X11.Xlib.Event (flush, selectInput)
import           Graphics.X11.Xlib.Extras  (Event (ConfigureEvent, ExposeEvent, KeyEvent),
                                            getEvent)
import           Graphics.X11.Xlib.Image   (destroyImage, putImage)
import           Graphics.X11.Xlib.Misc    (createPixmap, drawRectangle,
                                            getGeometry)
import           Graphics.X11.Xlib.Types   (MyImage (..))
import           Meld                      (createPrettyImage, getRootImage,
                                            imageDraw)
import           Xcomposite                (compositeRedirectAutomatic,
                                            compositeRedirectManual,
                                            getOverlayWindow,
                                            redirectSubwindows,
                                            releaseOverlayWindow)
-- import           Xdbe                      (queryExtension)
import           Foreign.C.Types           (CChar (..), CUChar (..))
import           Foreign.Marshal.Array     (newArray, peekArray, pokeArray)
import           Foreign.Storable.Generic  (GStorable, peek, poke)
import           Foreign.Storable.Generic  (sizeOf)
import           Xdbe
import           XDefaultsTransformer      (XDefaultValues (..),
                                            XDefaultsT (runDefaults),
                                            askBlackPixel, askColormap,
                                            askDisplay, askGc, askRootHeight,
                                            askRootWidth, askRootWindow,
                                            askWhitePixel)

sizeOfCUChar :: Int
sizeOfCUChar = sizeOf $ CUChar 0

proggo :: XDefaultsT ()
proggo = do
  display <- askDisplay
  width <- askRootWidth
  height <- askRootHeight
  rootWindow <- askRootWindow
  blackP <- askBlackPixel
  whiteP <- askWhitePixel
  geecee <- askGc
  liftIO $ redirectSubwindows display rootWindow compositeRedirectAutomatic
  liftIO $ selectInput display rootWindow substructureNotifyMask
  backgroundBuffer <- liftIO $ allocateBackBufferName display rootWindow copied
  newGC <- liftIO $ createGC display backgroundBuffer
  -- newGC <- liftIO $ createGC display pixmap
  liftIO $ setForeground display newGC blackP
  liftIO $ setBackground display newGC blackP
  let blackies = allBlackity
  -- dataz <- liftIO $ newArray blackies

  dataPointer <- liftIO $ mallocBytes $ ((fromIntegral width) * (fromIntegral height)* sizeOfCUChar)
  liftIO $ pokeArray dataPointer allBlackity
  image <- createPrettyImage dataPointer
  pixmap <- liftIO $ createPixmap display rootWindow width height 8
  liftIO $ putImage display pixmap newGC image 0 0 0 0 width height
  -- liftIO $ drawRectangle display backgroundBuffer newGC 100 100 500 500

  -- liftIO $ flush display
  liftIO $ print "UUUUUUU"
  liftIO $ swapBuffers display $ [SwapInfo rootWindow background]
  liftIO $ print "BBBBBBBBBBBBbUUUUUUU"
  liftIO $ print "CCCCCCCCCCBBBBBBBBBBBBbUUUUUUU"

  liftIO $ threadDelay 1000000

  -- liftIO $ print "DDDDDDDDDDDdCCCCCCCCCCBBBBBBBBBBBBbUUUUUUU"

  -- liftIO $ swapBuffers display $ [SwapInfo rootWindow background]
  -- liftIO $ print "abuuuga"
  liftIO $ flush display

  -- liftIO $ print "aruuuba"
  -- liftIO $ threadDelay 5000000

  -- liftIO $ swapBuffers display $ [SwapInfo rootWindow background]
  -- liftIO $ flush display

  -- liftIO $ threadDelay 500000

  -- liftIO $ swapBuffers display $ [SwapInfo rootWindow background]
  -- liftIO $ flush display
  -- image <- getRootImage
  -- imageDraw rootWindow image newGC
  -- liftIO $ destroyImage image

mainish :: IO ()
mainish = do
  getDefaults >>= runReaderT (runDefaults proggo)

getDefaults :: IO XDefaultValues
getDefaults = do
  defaultDisplay <- openDisplay ""
  let screenNumber = defaultScreen defaultDisplay
  let rootWindow = (defaultRootWindow defaultDisplay)
  (_, _, _, width, height, _, _) <- getGeometry defaultDisplay rootWindow
  return $ XDefaultValues
    defaultDisplay
    rootWindow
    width
    height
    (defaultGC defaultDisplay screenNumber)
    (defaultColormap defaultDisplay screenNumber)
    (defaultVisual defaultDisplay screenNumber)
    (blackPixel defaultDisplay screenNumber)
    (whitePixel defaultDisplay screenNumber)

uCharSize :: Int
uCharSize = sizeOf $ CUChar 0

blackity :: CChar
blackity = 100

overallSize = 4147200

allBlackity = take overallSize (repeat blackity)

arrFirst :: [CChar] -> CChar
arrFirst (a:_) = a

doGetImageData' = do
  image <- getRootImage
  width <- fromIntegral <$> askRootWidth
  height <- fromIntegral <$> askRootHeight
  let size = width * height * uCharSize
  peekers <- liftIO $ peekArray size $ ximage_data image
  liftIO $ print $ arrFirst peekers
  liftIO $ pokeArray (ximage_data image) allBlackity
  peekers2 <- liftIO $ peekArray size $ ximage_data image
  liftIO $ print $ arrFirst peekers2
  return ()

doGetImageData =
  getDefaults >>= runReaderT (runDefaults doGetImageData')

doAllocateBackBufferName' = do
  display <- askDisplay
  root <- askRootWindow
  liftIO $ allocateBackBufferName display root background

doAllocateBackBufferName =
  getDefaults >>= runReaderT (runDefaults doAllocateBackBufferName')

doGetVisualInfo' = do
  display <- askDisplay
  root <- askRootWindow
  liftIO $ getVisualInfo display [root] 1

doGetVisualInfo =
  getDefaults >>= runReaderT (runDefaults doGetVisualInfo')

doQueryExtension' = do
  display <- askDisplay
  liftIO $ queryExtension display

doQueryExtension =
  getDefaults >>= runReaderT (runDefaults doQueryExtension')

