module Main where

import Prelude

import Data.Array
import Data.Foldable (mconcat, foldl)
import Data.Int (toNumber)
import Data.Maybe (maybe)

import Math (cos, sin, pi)

import Flare
import Flare.Drawing

-- | Compute the prime factors of an integer. This is directly ported from
-- | Haskells `Diagrams.TwoD.Factorization`.
factorize :: Int -> Array Int
factorize 1 = []
factorize n = maybe [n] (\a -> a : factorize (n `div` a)) mf
  where
        mf = head $ filter (\x -> (n `mod` x) == 0) (2 .. (n - 1))

-- | Merge several factors of two into fours (2 * 2 * 2 * 2 * 2 = 4 * 4 * 2)
-- | to get a tighter layout for high powers of 2.
mergeTwos :: Array Int -> Array Int
mergeTwos xs = rest ++ merged num
  where rest = filter (/= 2) xs
        num = length xs - length rest
        merged 0 = []
        merged 1 = [2]
        merged n = 4 : merged (n - 2)

-- | Colors for different prime factors.
color :: Int -> Color
color 3 = rgb 255.0 102.0 190.0
color 5 = rgb 67.0 94.0 232.0
color 7 = rgb 86.0 255.0 162.0
color 11 = rgb 232.0 221.0 67.0
color 13 = rgb 255.0 135.0 73.0
color _ = white

-- | Draw a colored polygon.
polygon :: Int -> Drawing
polygon n = filled (fillColor (color n)) $ closed do
  i <- 0..n
  let theta = 2.0 * pi / (toNumber n) * toNumber i
  return { x: sin theta, y: - cos theta }

-- | Arrange `n` copies of a `Drawing` in a symmetric way.
primeLayout :: Drawing -> Int -> Drawing
primeLayout drawing n =
  polygon n
    <>
  mconcat do
    i <- 0 .. (n - 1)
    let phi = 2.0 * pi / (toNumber n) * toNumber i + offset n
        theta 4 | i == 1 || i == 2 = pi
                | otherwise        = 0.0
        theta _ = phi
        translate' = translate (sin phi) (- cos phi)
    (return <<< translate' <<< rotate (theta n) <<< scale') drawing

  where s = 1.3 / toNumber n
        scale' = scale s s
        offset 4 = pi / 4.0
        offset _ = 0.0

-- | Factor a number and draw the corresponding diagram.
factorDiagram :: Int -> Drawing
factorDiagram n = foldl primeLayout initial (factors n)
  where initial = filled (fillColor black) (circle 0.0 0.0 1.5)
        factors = factorize >>> reverse >>> mergeTwos

main =
  runFlareDrawing "controls" "canvas" $
    draw <$> intRange "Choose a number to factorize:" min max 210

  where size = 600.0
        c = size / 2.0
        s = size / 4.3

        min = 1
        max = 10000

        clamp n | n < min = min
                | n > max = max
                | otherwise = n

        draw = clamp >>> factorDiagram >>> scale s s >>> translate c c
