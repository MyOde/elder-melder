{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where

-- import Graphics.X11.Xlib.Screen (defaultColormapOfScreen)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Bits ((.|.))
import Graphics.X11.Xlib.Image (getImage, Image)
import Graphics.X11.Xlib.Event (flush, selectInput)
import Graphics.X11.Xlib.Context (setForeground)
import Graphics.X11.Xlib.Color (allocColor)
import Graphics.X11.Xlib.Display (openDisplay, defaultRootWindow, defaultGC, defaultScreen, defaultColormap)
import Graphics.X11.Xlib.Types (Display, GC, Color (..))
import Graphics.X11.Xlib.Misc (drawRectangle, fillRectangle)
import Graphics.X11.Types (Window, Colormap, doRed, doGreen, doBlue, exposureMask)
import Graphics.X11.Xfixes (xfixesCreateRegion, xfixesSetWindowShapeRegion, xfixesDestroyRegion, shapeBounding, shapeInput)
import Foreign.C.Types()
import Foreign.Ptr (nullPtr)
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, liftIO, runReaderT)

import Xcomposite (getOverlayWindow, redirectSubwindows, compositeRedirectManual, compositeRedirectAutomatic, releaseOverlayWindow)

data XDefaultValues = XDefaultValues { display :: Display
                                     , root :: Window
                                     , gc :: GC
                                     , colormap :: Colormap
                                     }

newtype XDefaultsT a = XDefaultsT
  { runDefaults :: ReaderT XDefaultValues IO a
  } deriving (Applicative, Monad, Functor, MonadReader XDefaultValues, MonadIO)

askDisplay :: XDefaultsT Display
askDisplay = display <$> ask
askRootWindow :: XDefaultsT Window
askRootWindow = root <$> ask
askGc :: XDefaultsT GC
askGc = gc <$> ask
askColormap :: XDefaultsT Colormap
askColormap = colormap <$> ask

someColor :: Color
someColor = Color { color_pixel = 0
                  , color_red = 254
                  , color_green = 0
                  , color_blue = 0
                  , color_flags = doRed .|. doGreen .|. doBlue
                  }


-- https://stackoverflow.com/questions/21780789/x11-draw-on-overlay-window
allowInputPassthrough :: Display -> Window -> IO ()
allowInputPassthrough display window = do
  print "BEFORE REGION"
  region <- xfixesCreateRegion display nullPtr 0
  print "BEFORE FIRST"
  xfixesSetWindowShapeRegion display window shapeBounding 0 0 0
  print "BEFORE SECOND"
  xfixesSetWindowShapeRegion display window shapeInput 0 0 region
  print "BOTH DONE"
  xfixesDestroyRegion display region
  selectInput display window exposureMask

prepareOverlay :: XDefaultsT Window
prepareOverlay = do
  display <- askDisplay
  rootWindow <- askRootWindow
  -- TODO Not sure if this ordering matters or not
  liftIO $ allowInputPassthrough display rootWindow
  overlay <- liftIO $ getOverlayWindow display rootWindow
  return overlay

lprint :: String -> XDefaultsT ()
lprint val = liftIO $ print val

draw :: Window -> XDefaultsT ()
draw overlay = do
  display <- askDisplay
  colormap <- askColormap
  gc <- askGc
  lprint "BEFORE ALLOCO"
  alloco <- liftIO $ allocColor display colormap someColor
  lprint "BEFORE foreground"
  liftIO $ setForeground display gc (color_pixel alloco)
  lprint "BEFORE draw"
  liftIO $ drawRectangle display overlay gc 50 50 500 500
  lprint "BEFORE fill"
  liftIO $ fillRectangle display overlay gc 50 50 500 500
  lprint "BEFORE flush"
  liftIO $ flush display
  liftIO $ threadDelay 100000
  draw overlay

proggo :: XDefaultsT ()
proggo = do
  overlay <- prepareOverlay
  display <- askDisplay
  rootWindow <- askRootWindow
  liftIO $ redirectSubwindows display rootWindow compositeRedirectAutomatic
  draw overlay

mainish :: IO ()
mainish = do
  getBackgroundWindow >>= runReaderT (runDefaults proggo)

getBackgroundWindow :: IO XDefaultValues
getBackgroundWindow = do
  defaultDisplay <- openDisplay ""
  let screenNumber = defaultScreen defaultDisplay
  return $ XDefaultValues
    defaultDisplay
    (defaultRootWindow defaultDisplay)
    (defaultGC defaultDisplay screenNumber)
    (defaultColormap defaultDisplay screenNumber)


-- changeColors :: IO ()
-- changeColors = do
--   (display, rootWindow, gc, colormap) <- getBackgroundWindow
--   -- image <- getImage display rootWindow
--   overlay <- getOverlayWindow display rootWindow
--   -- let overlay = compositeGetOverlayWindow display rootWindow in do
--   print overlay
--   alloco <- allocColor display colormap someColor
--   setForeground display gc (color_pixel alloco)
--   drawRectangle display overlay gc 50 50 500 500
--   fillRectangle display overlay gc 50 50 500 500
--   flush display
--     -- return ()


-- testOverlayGetter :: IO ()
-- testOverlayGetter = do
--   (display, rootWindow, gc, colormap) <- getBackgroundWindow
--   -- image <- getImage display rootWindow
--   overlay <- getOverlayWindow display rootWindow
--   -- redirectSubwindows display rootWindow compositeRedirectAutomatic
--   redirectSubwindows display rootWindow compositeRedirectManual
--   flush display
--   threadDelay 5000000
--   releaseOverlayWindow display rootWindow
--   flush display
