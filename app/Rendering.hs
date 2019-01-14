{-# LANGUAGE NoMonomorphismRestriction #-}

module Rendering 
    where

import UI.NCurses
import Data.Time.Clock (UTCTime)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM_)
import Data.Array (assocs)

import Control.Monad.Reader

import TileGrid
import qualified TetrisData as TD
import qualified Menus

data RenderDelta = UpdateTime UTCTime
                 | DrawPiece  Piece
                 | DrawBoard  TileGrid
                 | ChangeMenu Menus.Screen
                 | MenuSelect Int

type Render = ReaderT Window Curses

runRender :: Render a -> Window -> Curses a
runRender r w = runReaderT r w <* render

applyUpdate :: Update a -> Render a
applyUpdate u = do
    w <- ask
    lift $ updateWindow w u

-- Setting Background Colors
text, red, blue, magenta, green, yellow, cyan, white, black :: Render ColorID
red     = lift $ newColorID ColorBlack ColorRed     1
orange  = lift $ newColorID ColorRed   ColorYellow  2
blue    = lift $ newColorID ColorBlack ColorBlue    3
magenta = lift $ newColorID ColorBlack ColorMagenta 4
green   = lift $ newColorID ColorBlack ColorGreen   5
yellow  = lift $ newColorID ColorBlack ColorYellow  6
cyan    = lift $ newColorID ColorBlack ColorCyan    7
white   = lift $ newColorID ColorBlack ColorWhite   8
black   = lift $ newColorID ColorBlack ColorBlack   9
text    = lift $ newColorID ColorWhite ColorBlack   10

-- Drawing Boxes
boxify :: Update ()
boxify = drawBorder Nothing Nothing Nothing Nothing 
                    Nothing Nothing Nothing Nothing

-- Coloring
color :: Shape -> Render ColorID
color S = green
color Z = red
color L = orange
color J = blue
color T = magenta
color O = yellow
color I = cyan

fillTile :: Tile -> Render ()
fillTile t = do
    id <- case t of
        Empty  -> black
        Full t -> color t
    applyUpdate $ do
        setColor id
        let fillText = if t == Full L then "X" else " "
        drawString fillText

drawTile :: Integer -> Integer -> Tile -> Render ()
drawTile x y t = do
    applyUpdate $ moveCursor (y+1) (x+1)
    fillTile t

-- Rendering a Grid with a tile transformation
drawGridTrans :: (Tile -> Tile) -> TileGrid -> Render ()
drawGridTrans tileTransformation tg = forM_ (assocs tg) f
    where
    f ((x,y),t) | t == Empty = return ()
                | otherwise  = drawTile x y $ tileTransformation t

-- Pre-supplied transformations
drawGrid  = drawGridTrans id
clearGrid = drawGridTrans $ const Empty

-- Rendering a Menu
