module Particles where

import Linear
import Data.Foldable (foldl')

type Position = V2 Float
type Velocity = V2 Float
type Acceleration = V2 Float

newtype Particle = Particle { getParticle :: (Position, Velocity) }
  deriving (Eq, Show)

force :: Position -> Position -> Acceleration
force pa pb = case compare pa pb of
  EQ        -> zero -- to prevent division by zero
  otherwise -> 100 * (1 / dist) *^ nrm
  where
    dist = distance pa pb
    nrm = normalize $ pb - pa

integrateForces :: (Functor f, Foldable f) => Float -> f Particle -> f Particle
integrateForces dt ps = (fn <$> ps)
  where
    fn :: Particle -> Particle
    fn (Particle (pos, vel)) =
      Particle (
      pos,
      vel + (foldl' (\a b -> a + dt *^ (force pos b)) zero positions))
    
    positions = (fst . getParticle) <$> ps

testParticles :: String
testParticles = "particles work"
