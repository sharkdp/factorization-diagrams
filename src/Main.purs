module Main where

import Prelude

import Data.Array (reverse, (..), (:), length, filter, head)
import Data.Foldable (fold, foldl)
import Data.Int (toNumber)
import Data.Maybe (maybe)

import Math (cos, sin, pi)

import Flare (intRange)
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
mergeTwos xs = rest <> merged num
  where rest = filter (_ /= 2) xs
        num = length xs - length rest
        merged 0 = []
        merged 1 = [2]
        merged n = 4 : merged (n - 2)

-- | Colors for different prime factors.
color :: Int -> Color
color 3 =  rgb 255 102 190
color 5 =  rgb  67  94 232
color 7 =  rgb  86 255 162
color 11 = rgb 232 221  67
color 13 = rgb 255 135  73
color _ = white

-- | Draw a colored polygon.
polygon :: Int -> Drawing
polygon n = filled (fillColor (color n)) $ closed do
  i <- 0..n
  let theta = 2.0 * pi / (toNumber n) * toNumber i
  pure { x: sin theta, y: - cos theta }

-- | Arrange `n` copies of a `Drawing` in a symmetric way.
primeLayout :: Drawing -> Int -> Drawing
primeLayout drawing n =
  polygon n
    <>
  fold do
    i <- 0 .. (n - 1)
    let phi = 2.0 * pi / (toNumber n) * toNumber i + offset n
        theta 4 | i == 1 || i == 2 = pi
                | otherwise        = 0.0
        theta _ = phi
        translate' = translate (sin phi) (- cos phi)
    (pure <<< translate' <<< rotate (theta n) <<< scale') drawing

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
    draw <$> intRange "Factorize:" 1 10000 210

  where size = 600.0
        c = size / 2.0
        s = size / 4.3

        draw = factorDiagram >>> scale s s >>> translate c c
