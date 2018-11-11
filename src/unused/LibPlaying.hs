module LibPlaying where

-- import Graphics.X11.Xlib.Screen (defaultColormapOfScreen)
import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forever)
import           Control.Monad.Reader      (MonadIO, MonadReader, ReaderT, ask,
                                            liftIO, runReaderT)
import           Data.Bits                 ((.|.))
import           Foreign.C.Types           ()
import           Foreign.Ptr               (nullPtr)
import           Graphics.X11.Types        (Colormap, Window, doBlue, doGreen,
                                            doRed, exposureMask,
                                            structureNotifyMask,
                                            substructureNotifyMask)
import           Graphics.X11.Xfixes       (shapeBounding, shapeInput,
                                            xfixesCreateRegion,
                                            xfixesDestroyRegion,
                                            xfixesSetWindowShapeRegion)
import           Graphics.X11.Xlib.Atom    (internAtom)
import           Graphics.X11.Xlib.Color   (allocColor)
import           Graphics.X11.Xlib.Context (createGC, freeGC, setForeground)
import           Graphics.X11.Xlib.Display (blackPixel, closeDisplay,
                                            defaultColormap, defaultGC,
                                            defaultRootWindow, defaultScreen,
                                            defaultVisual, openDisplay,
                                            whitePixel)
import           Graphics.X11.Xlib.Event   (XEvent (..), XEventPtr,
                                            allocaXEvent, nextEvent)
import           Graphics.X11.Xlib.Event   (flush, selectInput)
import           Graphics.X11.Xlib.Extras  (Event (ConfigureEvent, ExposeEvent, KeyEvent),
                                            getEvent)
import           Graphics.X11.Xlib.Image   (Image, getImage)
import           Graphics.X11.Xlib.Image   (destroyImage)
import           Graphics.X11.Xlib.Misc    (drawRectangle, fillRectangle,
                                            getGeometry, getInputFocus,
                                            setTextProperty)
import           Graphics.X11.Xlib.Types   (Color (..), Display, GC)
import           Graphics.X11.Xlib.Window  (createSimpleWindow, mapWindow,
                                            moveResizeWindow)

import           Meld                      (drawOnEntireOverlay,
                                            drawOnEntireOverlay2, getRootImage)
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
  print "ALLES DONE"

prepareOverlay :: XDefaultsT Window
prepareOverlay = do
  display <- askDisplay
  rootWindow <- askRootWindow
  -- TODO Not sure if this ordering matters or not
  overlay <- liftIO $ getOverlayWindow display rootWindow
  liftIO $ allowInputPassthrough display overlay
  return overlay

lprint :: String -> XDefaultsT ()
lprint val = liftIO $ print val

-- draw :: Window -> XDefaultsT ()
-- draw overlay = do
--   display <- askDisplay
--   colormap <- askColormap
--   gc <- askGc
--   root <- askRootWindow
--   (activeWindow, _) <- liftIO $ getInputFocus display
--   (_, rootXPos, rooYPos, rootWidth, rootHeight, rootBorderWidth, rootDepth) <- liftIO $ getGeometry display root
--   (_, xPos, yPos, width, height, borderwidth, depth) <- liftIO $ getGeometry display activeWindow
--   lprint "BEFORE ALLOCO"
--   alloco <- liftIO $ allocColor display colormap someColor
--   lprint "BEFORE foreground"
--   liftIO $ setForeground display gc (color_pixel alloco)
--   lprint "BEFORE draw"
--   liftIO $ drawRectangle display overlay gc 50 50 500 500
--   lprint "BEFORE fill"
--   liftIO $ fillRectangle display overlay gc 50 50 500 500
--   lprint "BEFORE flush"
--   liftIO $ flush display
--   liftIO $ threadDelay 100000
--   draw overlay

loop :: Window -> XDefaultsT ()
loop overlay = do
  display <- askDisplay
  event <- liftIO $ allocaXEvent $ \ev ->
    nextEvent display ev >> getEvent ev
  liftIO $ print event
  case event of
    (ExposeEvent _ _ _ _ _ _ _ _ _ _) -> do
      draw overlay
      loop overlay
    (KeyEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) -> do
      liftIO $ releaseOverlayWindow display overlay
      liftIO $ closeDisplay display
    (_) -> loop overlay

draw :: Window -> XDefaultsT ()
draw overlay = do
  display <- askDisplay
  liftIO $ print "BOO"
  liftIO $ threadDelay 500000
  liftIO $ print "BAAA"
  image <- getRootImage
  liftIO $ print "BRRRR"
  drawOnEntireOverlay overlay image
  liftIO $ print "BAAAAAARRRR"
  -- liftIO $ flush display
  -- draw overlay

proggo :: XDefaultsT ()
proggo = do
  display <- askDisplay
  width <- askRootWidth
  height <- askRootHeight
  rootWindow <- askRootWindow
  blackP <- askBlackPixel
  whiteP <- askWhitePixel
  liftIO $ redirectSubwindows display rootWindow compositeRedirectAutomatic
  liftIO $ selectInput display rootWindow substructureNotifyMask
  newWindow <- liftIO $ createSimpleWindow display rootWindow 0 0 width height 0 blackP whiteP
  -- typeAtom <- liftIO $ internAtom display "_NET_WM_WINDOW_TYPE" False
  -- stateAtom <- liftIO $ internAtom display "_NET_WM_STATE" False

  -- liftIO $ setTextProperty display newWindow "_NET_WM_WINDOW_TYPE_DESKTOP" typeAtom
  -- liftIO $ setTextProperty display newWindow "_NET_WM_STATE_FULLSCREEN" stateAtom
  -- overlay <- prepareOverlay
  liftIO $ selectInput display newWindow structureNotifyMask
  liftIO $ mapWindow display newWindow
  liftIO $ moveResizeWindow display newWindow 0 0 width height
  liftIO $ flush display
  newGC <- liftIO $ createGC display rootWindow
  image <- getRootImage
  drawOnEntireOverlay2 rootWindow image newGC
  -- drawOnEntireOverlay2 rootWindow image newGC
  liftIO $ drawRectangle display rootWindow newGC 120 150 50 60
  -- drawOnEntireOverlay2 overlay image newGC
  -- looperPoopers
  -- loopDelayer overlay
  loopDelayer rootWindow

loopDelayer :: Window -> XDefaultsT ()
loopDelayer overlay = do
  display <- askDisplay
  image <- getRootImage
  width <- askRootWidth
  height <- askRootHeight

  event <- liftIO $ allocaXEvent $ \ev ->
    nextEvent display ev >> getEvent ev
  liftIO $ print event
  case event of
    (ConfigureEvent _ _ _ _ _ _ _ _ _ _ _ _ _) -> do
      liftIO $ moveResizeWindow display overlay 0 0 width height
      loopDelayer overlay
    -- (ExposeEvent _ _ _ _ _ _ _ _ _ _) -> do
    --   draw overlay
    --   loop overlay
    -- (KeyEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) -> do
    --   liftIO $ releaseOverlayWindow display overlay
    --   liftIO $ closeDisplay display
    (_) -> do
      newGC <- liftIO $ createGC display overlay
      drawOnEntireOverlay2 overlay image newGC
      liftIO $ drawRectangle display overlay newGC 120 150 50 60
      liftIO $ destroyImage image
      liftIO $ freeGC display newGC
      -- liftIO $ threadDelay 1000000
      liftIO $ threadDelay 10000
      loopDelayer overlay

looperPoopers :: XDefaultsT ()
looperPoopers = do
  display <- askDisplay
  event <- liftIO $ allocaXEvent $ \ev ->
    nextEvent display ev >> getEvent ev
  case event of
    (KeyEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) -> do
      liftIO $ closeDisplay display
    (_) -> do
      liftIO $ print "yes yes"
      looperPoopers
  -- overlay <- prepareOverlay
  -- loop overlay

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
