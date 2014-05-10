{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AI.Localize.ParticleFilter
  ( ParticleFilterT
  , runParticleFilterT
  , particleFilterT
  , module AI.Localize.ParticleFilter.Class
  ) where

import Control.Arrow
import Control.Monad.Random
import Data.Traversable (Traversable)
import Data.Functor.Identity
import qualified Data.Traversable as T

import AI.Localize.ParticleFilter.Class

--
-- TypeClasses
--


class (Num w) => Weight w where
  divW :: w -> w -> w


class (Traversable d) => Distrib d a where
  uniformD   :: d a -> d a
  normalizeD :: d a -> d a
  sizeD      :: Integral b => d a -> b


--
-- Definitions 
--


type Particle w a = (w, a)


newtype ParticleFilterT d w a m = ParticleFilterT
  { unParticleFilterT 
      :: (a -> a) ->
         (a -> w) ->
         d (Particle w a) ->
         m (d (Particle w a)) } 


runParticleFilterT :: (Weight w, Distrib d (Particle w a), Monad m) => 
                      ParticleFilterT d w a m ->
                      (a -> a) ->
                      (a -> w) ->
                      d (Particle w a) -> 
                      m (d (Particle w a))
runParticleFilterT = unParticleFilterT


--
-- Algorithm
--


particleFilterT :: (Weight w, Distrib d (Particle w a), Monad m) =>
                   (d (Particle w a) -> m (d (Particle w a))) ->
                   ((a -> a) -> a -> m a) ->
                   ((a -> w) -> a -> m w) ->
                   ParticleFilterT d w a m
particleFilterT resample controlError weighError =
  ParticleFilterT $ \control weigh p0 -> 
    do p1 <- T.mapM (c control) p0 -- control
       p2 <- T.mapM (w weigh) p1   -- weigh
       let p3 = normalizeD p2      -- normalize post-weigh
       p4 <- resample p3           -- resample
       return (normalizeD p4)      -- normalize post-resample
 where
  c control p =
    do a <- controlError control (snd p)
       return (fst p, a)
  w weigh p =
    do v <- weighError weigh (snd p)
       return (fst p + v, snd p)


--
-- Instances
--


instance (Weight w) => Distrib [] (Particle w a) where
  uniformD d =
    let f = first $ \_ -> divW 1 (fromIntegral $ sizeD d)
    in map f d
  normalizeD d =
    let f = first $ \w -> divW w (sum $ map fst d)
    in map f d
  sizeD = fromIntegral . length 


instance Weight Float where
  divW = (/)


