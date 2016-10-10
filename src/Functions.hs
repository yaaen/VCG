-----------------------------------------------------------------------------
--
-- Module      :  Functions
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Functions (_readDeltaRPM_,
                  _applyDeltaRPM_,
                  _readRPMSymbols_,
                  _deltaRPMSymbols_,
                  exeMain_,
                  Value (..)) where

import Data.Algorithm.Diff3
import Control.Monad
import Control.Applicative
import Data.Typeable
import System.IO.Unsafe
import Extraction
import Operations


data Value = C CoqValue
           | RPM String
           | Tuple (Value, Value)
           deriving (Show, Typeable)


readDeltaRPM_  :: IO Value
readDeltaRPM_  = Tuple <$> (readDeltaRPM  >>= \(a,b) -> return (RPM a, RPM b))

_readDeltaRPM_  :: Value
_readDeltaRPM_  = unsafePerformIO $ readDeltaRPM_

-------------------------------------------------
applyDeltaRPM_  :: Value -> IO Value
applyDeltaRPM_ a = Tuple <$> applyDeltaRPM__ a

_applyDeltaRPM_  :: Value -> Value
_applyDeltaRPM_ a = unsafePerformIO $ applyDeltaRPM_ a

applyDeltaRPM__ :: Value -> IO (Value, Value)
applyDeltaRPM__ (Tuple (RPM a, RPM b))
    = liftM id (applyDeltaRPM (a,b)) >> return (RPM a, RPM b)

--------------------------------------------------
readRPMSymbols_ :: Value -> IO Value
readRPMSymbols_ a = Tuple <$> readRPMSymbols__ a

_readRPMSymbols_ :: Value -> Value
_readRPMSymbols_ a = unsafePerformIO $ readRPMSymbols_ a

readRPMSymbols__
    :: Value ->
       IO (Value, Value)
readRPMSymbols__ (Tuple (RPM a, RPM b))
    = liftM f (readRPMSymbols (a,b))
      where
      g = \(a,b) -> Symbol a b
      f = \(ll, lr) -> (C (ListSymbols (map g ll)), C (ListSymbols (map g lr)))

--------------------------------------------------
deltaRPMSymbols_ :: Value -> IO Value
deltaRPMSymbols_ (Tuple (C (ListSymbols a), C (ListSymbols b)))
    = C <$> ListSymbols <$> liftM g (deltaRPMSymbols (map f a, map f b))
      where
      f = \(Symbol s n) -> (s,n)
      g = map (\(RightChange ((s,n):[])) -> Symbol s n)

_deltaRPMSymbols_ :: Value -> Value
_deltaRPMSymbols_ a = unsafePerformIO $ deltaRPMSymbols_ a
--------------------------------------------------


exeMain_ =
    do
    readDeltaRPM_ >>=
        \x -> applyDeltaRPM_ x >>=
            \y -> readRPMSymbols_ y >>=
                \z -> deltaRPMSymbols_ z >>=
                    \h -> -- (putStrLn . show) h >>
                        return h

    --hunks <- readDeltaRPM_ >>= applyDeltaRPM_ >>= readRPMSymbols_ >>= deltaRPMSymbols_
    --putStrLn $ show hunks
    --return ()

