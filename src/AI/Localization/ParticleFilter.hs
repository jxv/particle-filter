{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AI.Localization.ParticleFilter
  ( particle
  , ParticleFilterT
  , runParticleFilterT
  , particleFilterT
  , module AI.Localization.ParticleFilter.Class
  ) where

import Control.Arrow
import Control.Monad.Random
import Data.Traversable (Traversable)
import Data.Functor.Identity
import qualified Data.Traversable as T

import AI.Localization.ParticleFilter.Class

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


particle :: (Weight w) => w -> a -> Particle w a
particle = (,)


newtype ParticleFilterT d w a m = ParticleFilterT
  { unParticleFilterT :: (a -> a) ->
                         (a -> w) ->
                         d (Particle w a) ->
                         m (d (Particle w a)) } 


runParticleFilterT :: (Weight w, Distrib d (Particle w a), Monad m) => 
                      ParticleFilterT d w a m -> 
                      (a -> a) -> -- | Control function
                      (a -> w) -> -- | Weigh function
                      d (Particle w a) ->  -- | Particles
                      m (d (Particle w a))
runParticleFilterT = unParticleFilterT


--
-- Algorithm
--


particleFilterT :: (Weight w, Distrib d (Particle w a), Monad m) =>
                   (d (Particle w a) -> m (Particle w a)) ->  -- | Resample
                   ((a -> a) -> a -> m a) -> -- | Control error
                   ((a -> w) -> a -> m w) -> -- | Weigh error
                   ParticleFilterT d w a m
particleFilterT resample control_error weigh_error =
  ParticleFilterT $ \control weigh p0 -> 
    do p1 <- T.mapM (c control) p0     -- control
       p2 <- T.mapM (w weigh) p1       -- weigh
       let p3 = normalizeD p2          -- normalize post-weigh
       p4 <- T.mapM (r resample p3) p3 -- resample
       return (normalizeD p4)          -- normalize post-resample
 where
  c control p =
    do a <- control_error control (snd p)
       return (fst p, a)
  w weigh p =
    do v <- weigh_error weigh (snd p)
       return (fst p + v, snd p)
  r resample ps _ = resample ps


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


