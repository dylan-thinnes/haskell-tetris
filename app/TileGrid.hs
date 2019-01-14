{-# LANGUAGE TemplateHaskell #-}
module TileGrid where

import Data.Array
import Data.Bifunctor
import Data.Tuple (swap)
import Control.Lens hiding (Empty)
import System.Random

type Grid = Array (Integer,Integer)
type TileGrid = Array (Integer,Integer) Tile
type MaskGrid = Array (Integer,Integer) Bool

data Tile = Empty | Full Shape
    deriving (Show, Eq)

-- Piece Definitions
data Piece = Piece 
    { _position :: Position 
    , _shape :: Shape 
    , _orientation :: Orientation
    }
    deriving Show

type Position = (Integer, Integer)

data Shape = S | Z | L | J | T | O | I
    deriving (Show, Enum, Bounded, Eq)

instance Random Shape where
    random g = first toEnum $ random g
    randomR (lo,hi) g = first toEnum $ randomR (fromEnum lo, fromEnum hi) g

data Orientation = Up | Ri | Do | Le 
    deriving (Show, Enum, Bounded, Eq)
         
makeLenses ''Piece

-- Generation
listToGrid :: Integer -> Integer -> [a] -> Grid a
listToGrid w h values = listArray ((0,0),(w-1,h-1)) values

emptyGrid :: Integer -> Integer -> TileGrid
emptyGrid w h = listArray ((0,0),(w-1,h-1)) $ repeat Empty

-- General Transformations of Arrays
mapIndices :: (Ix i, Ix j) => (i -> j) -> ((i,i) -> (j,j)) -> Array i e -> Array j e
mapIndices indexTransform boundsTransform arr = array bounds' indices
    where
    indices = map (\(i,e) -> (indexTransform i,e)) $ assocs arr
    bounds' = boundsTransform $ bounds arr

-- Translations
translate :: Integer -> Integer -> TileGrid -> TileGrid
translate dx dy = mapIndices shiftXY shiftBounds  
    where
    shiftXY = bimap (+dx) (+dy)
    shiftBounds = bimap shiftXY shiftXY

-- Rotations
rotate90 :: TileGrid -> TileGrid
rotate90 tg = mapIndices transformation flipBounds tg
    where
    flipBounds = bimap swap swap
    (_,(maxX,maxY)) = bounds tg
    transformation = \(x,y) -> (maxY - y,x)

rotate180 :: TileGrid -> TileGrid
rotate180 tg = mapIndices transformation id tg
    where
    (_,(maxX,maxY)) = bounds tg
    transformation = \(x,y) -> (maxX - x, maxY - y)

rotate270 :: TileGrid -> TileGrid
rotate270 tg = mapIndices transformation flipXY tg
    where
    flipXY = bimap swap swap
    (_,(maxX,maxY)) = bounds tg
    transformation = \(x,y) -> (y, maxX - x)

-- Pieces to TileGrids
shapeWidth, shapeHeight :: Shape -> Integer
shapeWidth  = (1+) . fst . snd . bounds . shapeToMask
shapeHeight = (1+) . snd . snd . bounds . shapeToMask

pieceWidth, pieceHeight :: Piece -> Integer
pieceWidth  (Piece p s o) | o == Up || o == Do = shapeWidth s
                          | otherwise          = shapeHeight s
pieceHeight (Piece p s o) | o == Up || o == Do = shapeHeight s
                          | otherwise          = shapeWidth s

shapeToMask :: Shape -> MaskGrid
shapeToMask s = let t = True
                    f = False
                in case s of
                     S -> listToGrid 3 2 [f,t,t,t,t,f]
                     Z -> listToGrid 3 2 [t,f,t,t,f,t]
                     L -> listToGrid 3 2 [f,t,f,t,t,t]
                     J -> listToGrid 3 2 [t,t,f,t,f,t]
                     T -> listToGrid 3 2 [f,t,t,t,f,t]
                     O -> listToGrid 2 2 [t,t,t,t]
                     I -> listToGrid 4 1 [t,t,t,t]

shapeToTilegrid :: Shape -> TileGrid
shapeToTilegrid s = fmap f $ shapeToMask s
    where
    f True  = Full s
    f False = Empty

pieceToTilegrid :: Piece -> TileGrid
pieceToTilegrid p = translate x y $ rot $ shapeToTilegrid s   
    where
    s     = _shape p
    o     = _orientation p
    (x,y) = _position p
    rot = case o of
            Up -> id
            Ri -> rotate90
            Do -> rotate180
            Le -> rotate270
