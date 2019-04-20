module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Random
import Linear

import Particles

type World = [Particle]

displayMode = InWindow "Window" (400, 400) (0, 0)

fct :: (Num a, Enum a) => a -> a
fct n = product [1..n]

oo :: (Integral a, Enum a) => a -> a -> a
oo m n = (fct m) `div` ((fct n) * (fct (m - n)))


shape :: Picture
shape = Circle 2

drawFunc :: World -> Picture
drawFunc = pictures . map drawParticle
  where
    drawParticle (Particle ((V2 x y), _)) = translate x y shape

updateFunc :: ViewPort -> Float -> World -> World
updateFunc _ dt = map updatePosition . integrateForces dt
  where
    updatePosition (Particle (pos, vel)) = Particle (pos + vel ^* dt, vel)

count :: Int
count = 200

main :: IO ()
main = do
  g <- getStdGen
  let rnds = take (4 * count) $ (randoms g :: [Float])
  simulate displayMode white 60 (initialWorld rnds) drawFunc updateFunc
    where
      initialWorld [] = []
      initialWorld (x:y:vx:vy:rs) = Particle ((V2 (-200 + 400 * x) (-200 + 400 * y)), (V2 vx vy)) : (initialWorld rs)
      duetWorld = [Particle ((V2 0 0), (V2 0 0)), Particle ((V2 10 0), (V2 0 10))]

  
