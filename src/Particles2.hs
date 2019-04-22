module Particles2 where

import Types
import qualified Data.Sequence as S (Seq, length)

integrateForces2 :: [(Int, Int)] -> Float -> S.Seq Particle -> S.Seq Particle
integrateForces2 = undefined

mkIntegrateForces :: S.Seq Particle -> Float -> S.Seq Particle -> S.Seq Particle
mkIntegrateForces es = integrateForces2 $ uniquePairs [0..S.length es - 1]

uniquePairs :: [a] -> [(a, a)]
uniquePairs [] = []
uniquePairs (x:xs) = map (\y -> (x, y)) xs <> (uniquePairs xs)
