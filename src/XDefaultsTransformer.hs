{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module XDefaultsTransformer ( XDefaultValues (..)
                            , XDefaultsT (..)
                            , askDisplay
                            , askRootWindow
                            , askGc
                            , askColormap
                            , askVisual
                            , askRootHeight
                            , askRootWidth
                            , askBlackPixel
                            , askWhitePixel
                            ) where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask)
import Graphics.X11.Xlib.Types (Display, GC, Visual, Dimension, Pixel)
import Graphics.X11.Types (Window, Colormap)

data XDefaultValues = XDefaultValues { display :: Display
                                     , root :: Window
                                     , rootWidth :: Dimension
                                     , rootHeight :: Dimension
                                     , gc :: GC
                                     , colormap :: Colormap
                                     , visual :: Visual
                                     , displayBlackPixel :: Pixel
                                     , displayWhitePixel :: Pixel
                                     }

newtype XDefaultsT a = XDefaultsT
  { runDefaults :: ReaderT XDefaultValues IO a
  } deriving (Applicative, Monad, Functor, MonadReader XDefaultValues, MonadIO)

askDisplay :: XDefaultsT Display
askDisplay = display <$> ask
askRootWindow :: XDefaultsT Window
askRootWindow = root <$> ask
askRootHeight :: XDefaultsT Dimension
askRootHeight = rootHeight <$> ask
askRootWidth :: XDefaultsT Dimension
askRootWidth = rootWidth <$> ask
askGc :: XDefaultsT GC
askGc = gc <$> ask
askColormap :: XDefaultsT Colormap
askColormap = colormap <$> ask
askVisual :: XDefaultsT Visual
askVisual = visual <$> ask
askBlackPixel, askWhitePixel :: XDefaultsT Pixel
askBlackPixel = displayBlackPixel <$> ask
askWhitePixel = displayWhitePixel <$> ask

