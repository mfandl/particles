module Particles where

import Linear

import Types

import Data.Foldable (foldl', foldMap)


force :: Position -> Position -> Acceleration
force pa pb = case compare pa pb of
  EQ        -> zero -- to prevent division by zero
  otherwise -> (1 / dist) *^ nrm
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
      vel + (foldl' (\a b -> a + dt *^ (100 * (force pos b))) zero positions))
    
    positions = (fst . getParticle) <$> ps

