{-# LANGUAGE TemplateHaskell #-}
module TileGrid where

import Data.Array
import Data.Bifunctor
import Data.Tuple (swap)
import Control.Lens hiding (Empty)
import System.Random
import Data.List (intersect)
import Utils

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

data Direction = GoDown | GoRight | GoLeft | GoUp
data Rotation = Rot0 | Rot90 | Rot180 | Rot270 deriving (Show, Eq, Enum, Bounded)
type Orientation = Rotation

data Mod = Mod { _x   :: Integer
               , _y   :: Integer
               , _rot :: Rotation
               }

instance Random Shape where
    random g = first toEnum $ random g
    randomR (lo,hi) g = first toEnum $ randomR (fromEnum lo, fromEnum hi) g

makeLenses ''Piece
makeLenses ''Mod

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

rotationToTransition :: Rotation -> (TileGrid -> TileGrid)
rotationToTransition r = case r of
    Rot0   -> id
    Rot90  -> rotate90
    Rot180 -> rotate180
    Rot270 -> rotate270

rotateOrientation :: Rotation -> Orientation -> Rotation
rotateOrientation rot ori = iterateN (fromEnum rot) cycleC ori

-- PieceModifications to TileGrid transformations
modToTrans :: Mod -> (TileGrid -> TileGrid)
modToTrans (Mod dx dy rot) = translate dx dy . rotationToTransition rot
    
applyMod :: Mod -> Piece -> Piece
applyMod (Mod dx dy rot) p = p & position._1 +~ dx
                               & position._2 +~ dy
                               & orientation %~ rotateOrientation rot

-- Pieces within Bounds
pieceWithin :: Piece -> Integer -> Integer -> Bool
pieceWithin p x y =    between pieceOuterX 0 x 
                    && between pieceOuterY 0 y
                    && between pieceInnerX 0 x
                    && between pieceInnerY 0 y
    where
    pieceOuterX = view (position._1) p + pieceWidth  p
    pieceOuterY = view (position._2) p + pieceHeight p
    pieceInnerX = view (position._1) p
    pieceInnerY = view (position._2) p

-- Pieces to TileGrids
shapeWidth, shapeHeight :: Shape -> Integer
shapeWidth  = (1+) . fst . snd . bounds . shapeToMask
shapeHeight = (1+) . snd . snd . bounds . shapeToMask

pieceWidth, pieceHeight :: Piece -> Integer
pieceWidth  (Piece p s o) | o == Rot0 || o == Rot180 = shapeWidth  s
                          | otherwise                = shapeHeight s
pieceHeight (Piece p s o) | o == Rot0 || o == Rot180 = shapeHeight s
                          | otherwise                = shapeWidth  s

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
pieceToTilegrid p = translate x y $ rotationToTransition o $ shapeToTilegrid s   
    where
    s     = _shape p
    o     = _orientation p
    (x,y) = _position p

-- Collisions Between TileGrids
collision :: TileGrid -> TileGrid -> Bool
collision t1 t2 = not . null $ filter f $ assocs t1 `intersect` assocs t2
    where
    f (_,x) = x /= Empty

-- Remove lowest rows of a TileGrid
levelGrid :: Integer -> TileGrid -> TileGrid
levelGrid n tg = addTopLines $ translate 0 n $ stripLastLines tg
    where
    bs = bounds tg
    maxX = fst $ snd $ bounds tg
    maxY = snd $ snd $ bounds tg
    stripLastLines = array bs . filter (\((_,y),_) -> y >= maxY - n)                  . assocs
    addTopLines    = array bs . ([((x,y),Empty) | x <- [0..maxX-1], y <- [0..n-1]]++) . assocs
