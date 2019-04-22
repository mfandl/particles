module Types where

import Linear

type Position = V2 Float
type Velocity = V2 Float
type Acceleration = V2 Float

newtype Particle = Particle { getParticle :: (Position, Velocity) }
  deriving (Eq, Show)
