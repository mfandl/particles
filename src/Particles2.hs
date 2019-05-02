module Particles2 where

import Types
import qualified Data.Sequence as S (Seq, index, update)
import  Linear

import Data.Foldable (foldl')

import Particles (force)

applySeqForces :: Float -> S.Seq Particle -> Int -> Int -> Particle
applySeqForces dt seq ia ib = Particle (posA, velA + dt *^ (force posA posB))
  where
    (Particle (posA, velA)) = S.index seq ia
    (Particle (posB, _)) = S.index seq ib

integrateForces :: [(Int, Int)] -> Float -> S.Seq Particle -> S.Seq Particle
integrateForces pairs dt particles = foldr (\(ia, ib) ps ->
  ((S.update ib (applySeqForces dt ps ib ia)) . (S.update ia (applySeqForces dt ps ia ib))) ps) particles pairs

mkIntegrateForces :: Int -> Float -> S.Seq Particle -> S.Seq Particle
mkIntegrateForces cnt = integrateForces $ uniquePairs [0..cnt-1]

uniquePairs :: [a] -> [(a, a)]
uniquePairs [] = []
uniquePairs (x:xs) = map (\y -> (x, y)) xs <> (uniquePairs xs)
