module Lib where

import Control.Concurrent (threadDelay)
import           Control.Monad.Reader      (liftIO, runReaderT)
import           Graphics.X11.Types        (substructureNotifyMask)
import           Graphics.X11.Xlib.Context (createGC, freeGC, setForeground)
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
import           Graphics.X11.Xlib.Misc    (getGeometry)
import           Meld                      (getRootImage, imageDraw)
import           Xcomposite                (compositeRedirectAutomatic,
                                            compositeRedirectManual,
                                            getOverlayWindow,
                                            redirectSubwindows,
                                            releaseOverlayWindow)
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
  liftIO $ flush display
  newGC <- liftIO $ createGC display rootWindow
  image <- getRootImage
  imageDraw rootWindow image newGC
  liftIO $ destroyImage image

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
