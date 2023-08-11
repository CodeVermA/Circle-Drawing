module Main(main) where

import Graphics.Gloss
import Data.List

type Function = Float -> Float -> Float

window :: Display
window = FullScreen

bgColor :: Color
bgColor = black

-- Equations
xCor :: Function
xCor k constant = cos((10*pi*k)/constant) * (1 - (1/2)*(cos((16*pi*k)/constant))^2)

yCor :: Function
yCor k constant = sin((10*pi*k)/constant) * (1 - (1/2)*(cos((16*pi*k)/constant))^2)

radius :: Function
radius k constant = (1/200) + (1/10) * (sin((52*pi*k)/constant))^4

drawPic :: Float -> Float -> Color -> [Picture]
drawPic constant scaleBy color' = [translate (scaleBy * xCor k constant) (scaleBy * yCor k constant) $ color color' $ circle (scaleBy * radius k constant) | k <- [1..constant]]


createPic :: Int -> Int -> Float -> Float -> [[Picture]]
createPic 0 _ _ _ = [[]]
createPic noOfTimes looped constnt scaleBy | looped `mod` 3 == 0 = drawPic constnt scaleBy orange : createPic (noOfTimes - 1) (looped + 1) (constnt / 2.2) (scaleBy / 2.5)
                                           | looped `mod` 3 == 1 = drawPic constnt scaleBy white : createPic (noOfTimes - 1) (looped + 1) (constnt / 2.2) (scaleBy / 2.5)
                                           | looped `mod` 3 == 2 = drawPic constnt scaleBy green : createPic (noOfTimes - 1) (looped + 1) (constnt / 2.2) (scaleBy / 2.5)

fullPic :: Picture
fullPic =  pictures [if even idx then rotate 45 pic else pic | (pic, idx) <- zip (map (pictures) (createPic 30 0 12000 1200)) [1..]]

main :: IO ()
main = display window bgColor fullPic
