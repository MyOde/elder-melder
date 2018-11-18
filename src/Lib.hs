module Lib where

import Control.Concurrent (threadDelay)
import           Control.Monad.Reader      (liftIO, runReaderT)
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
import           Graphics.X11.Xlib.Image   (destroyImage)
import           Graphics.X11.Xlib.Misc    (drawRectangle, getGeometry)
import           Graphics.X11.Xlib.Types   (MyImage (..))
import           Meld                      (getRootImage, imageDraw)
import           Xcomposite                (compositeRedirectAutomatic,
                                            compositeRedirectManual,
                                            getOverlayWindow,
                                            redirectSubwindows,
                                            releaseOverlayWindow)
-- import           Xdbe                      (queryExtension)
import           Foreign.Storable.Generic  (GStorable, peek, poke)
import           Xdbe
import           XDefaultsTransformer      (XDefaultValues (..),
                                            XDefaultsT (runDefaults),
                                            askBlackPixel, askColormap,
                                            askDisplay, askGc, askRootHeight,
                                            askRootWidth, askRootWindow,
                                            askWhitePixel)

proggo :: XDefaultsT ()
proggo = do
  display <- askDisplay
  width <- askRootWidth
  height <- askRootHeight
  rootWindow <- askRootWindow
  blackP <- askBlackPixel
  whiteP <- askWhitePixel
  -- liftIO $ redirectSubwindows display rootWindow compositeRedirectAutomatic
  -- liftIO $ selectInput display rootWindow substructureNotifyMask
  backgroundBuffer <- liftIO $ allocateBackBufferName display rootWindow copied
  newGC <- liftIO $ createGC display backgroundBuffer
  liftIO $ setForeground display newGC blackP
  liftIO $ setBackground display newGC blackP
  liftIO $ drawRectangle display backgroundBuffer newGC 100 100 500 500

  liftIO $ flush display
  liftIO $ print "UUUUUUU"
  liftIO $ swapBuffers display $ [SwapInfo rootWindow background]
  liftIO $ print "BBBBBBBBBBBBbUUUUUUU"
  liftIO $ flush display
  liftIO $ print "CCCCCCCCCCBBBBBBBBBBBBbUUUUUUU"

  liftIO $ threadDelay 5000000

  liftIO $ print "DDDDDDDDDDDdCCCCCCCCCCBBBBBBBBBBBBbUUUUUUU"

  liftIO $ swapBuffers display $ [SwapInfo rootWindow copied]
  liftIO $ flush display

  liftIO $ threadDelay 5000000

  liftIO $ swapBuffers display $ [SwapInfo rootWindow copied]
  liftIO $ flush display

  liftIO $ threadDelay 500000

  liftIO $ swapBuffers display $ [SwapInfo rootWindow copied]
  liftIO $ flush display
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

doGetImageData' = do
  image <- getRootImage
  return image

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

