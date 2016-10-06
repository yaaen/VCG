-----------------------------------------------------------------------------
--
-- Module      :  Generator5
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
{-# LANGUAGE FlexibleInstances #-}
module Generator5 ( example

) where

import Data.Map
import Control.Applicative hiding (empty)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Extraction
import Functions

-- meta-functions
type Var = String
data UninterFun = ReadDeltaRPM
                | ApplyDeltaRPM Var
                | ReadRPMSymbols Var
                | DeltaRPMSymbols Var
                | ComputeDelta Var
                | ProductCore
                deriving (Show)

data MetaExpr = Fun UninterFun
              | Assign Var MetaExpr
              | MetaExpr :>>: MetaExpr
              deriving (Show)

data Term = Var String | Val UninterFun
          deriving (Show)

data Formula  = Formula :/\: Formula
              | Formula :\/: Formula
              | Formula :=>: Formula
              | Formula :<=>: Formula
              | Not Formula
              | Exists String Formula
              | Forall String Formula
              | TRUE
              | FALSE
              | Term :<:   Term
              | Term :>:   Term
              | Term :<=:  Term
              | Term :>=:  Term
              | Term :=:   Term
              | Term :/=:  Term
              | PTy Term
              | PEx Term Term
              | PEv Term Term Term
              | Coq CoqProp

instance Show Formula where
    show (a :=>: b)     = show a ++ " -> " ++ "\n" ++ show b
    show (Coq coqprop)  = show coqprop
    show any            = "any"

type Store = Map String (Value)

newtype PreCondStoreT store m a
    = PreCondStoreT (store -> m (store, a))

instance MonadTrans (PreCondStoreT store) where
    lift m = PreCondStoreT (\s -> do a <- m
                                     return (s,a))

{-instance Monad m => Monad (PreCondStoreT store m) where
    return a = PreCondStoreT (\store -> return (store,a))
    PreCondStoreT m >>= k
        = PreCondStoreT ( \s -> do
                                (s', a) <- m s
                                let PreCondStoreT m' = k a
                                m' s' )
    fail s = PreCondStoreT (\_ -> fail s) -}

instance (Show store) => Monad (PreCondStoreT store IO) where
    return p = PreCondStoreT (\store -> return (store, p))
    PreCondStoreT m >>= k
        = PreCondStoreT ( \s -> do
                                (_, a) <- m s
                                let PreCondStoreT m' = k a
                                (s'', _) <- m' s -- real initial s, s'' contains 1st eval

                                (s''', a') <- m s'' -- repeat m with: s'' contains eval,
                                                    -- s''' contains 2nd eval
                                let PreCondStoreT m'' = k a' -- real a (resolved)
                                m'' s'''
                                )

{-instance  (Monad m) => Functor (PreCondStoreT store m) where
   fmap f x = do
              x >>= (pure . f)

instance (Monad m) => Applicative (PreCondStoreT store m) where
    pure   = return
    m *> k = m >>= \ _ -> k
    (<*>)  = ap
-}

getT :: Monad m => PreCondStoreT s m s
getT = PreCondStoreT (\s -> return (s, s))

putT :: Monad m => s -> PreCondStoreT s m ()
putT s = PreCondStoreT (\_ -> return (s, ()))

evalPreCondStoreT :: Monad m => PreCondStoreT s m a -> s -> m (s, a)
evalPreCondStoreT (PreCondStoreT m) store
    = do
      --(s', a) <- m store
      --return a
      m store


eval :: MetaExpr ->
        Store ->
        PreCondStoreT Store IO Store

eval (Assign "x" (Fun ReadDeltaRPM))
    = \s -> do
            lift $ putStrLn $ "EVAL: ReadDeltaRPM"
            --v <- lift $ readDeltaRPM_
            return $ s --insert "x" v s

eval (Assign "y" (Fun (ApplyDeltaRPM "x")))
    = \s -> do
            lift $ putStrLn $ "EVAL: ApplyDeltaRPM"
            --v <- lift $ applyDeltaRPM_ (s ! "x")
            return $ s --insert "y" v s

eval (Assign "z" (Fun (ReadRPMSymbols "y")))
    = \s -> do
            lift $ putStrLn "EVAL: ReadRPMSymbols"
            --v <- lift $ readRPMSymbols_ (s ! "y")
            return $ s --insert "z" v s

eval (Assign "h" (Fun (DeltaRPMSymbols "z")))
    = \s -> do
            lift $ putStrLn "EVAL: DeltaRPMSymbols"
            --v <- lift $ deltaRPMSymbols_ (s ! "z")
            return $ s -- insert "h" v s

--eval (a :>>: b)
--    = \s -> eval a s >>= eval b

wp :: MetaExpr ->
      Formula ->
      PreCondStoreT Store IO Formula

wp (Assign "x" (Fun ReadDeltaRPM))
    =  \p -> do
             lift $ putStrLn "WP: ReadDeltaRPM"
             s <- getT
             eval (Assign "x" (Fun ReadDeltaRPM)) s
             return p

wp (Assign "y" (Fun (ApplyDeltaRPM "x")))
    = \p -> do
            lift $ putStrLn $ "WP: ApplyDeltaRPM"
            s <- getT
            eval (Assign "y" (Fun (ApplyDeltaRPM "x"))) s
            return $ p

wp (Assign "z" (Fun (ReadRPMSymbols "y")))
    = \p -> do
            lift $ putStrLn "WP: ReadRPMSymbols"
            s <- getT
            eval (Assign "z" (Fun (ReadRPMSymbols "y"))) s
            return $ p

wp (Assign "h" (Fun (DeltaRPMSymbols "z")))
    = \p -> do
            lift $ putStrLn "WP: DeltaRPMSymbols"
            s <- getT
            eval (Assign "h" (Fun (DeltaRPMSymbols "z"))) s
            return $ p

wp (a :>>: b)
    = \p -> --getT >>= eval a >>= eval b >>= putT >> ($)
            do
            lift $ putStrLn $ "A:= " ++ show a
            wp b p >>= wp a
            lift $ putStrLn $ "B:= " ++ show b
            return p


example = do
           let a = Assign "x" (Fun ReadDeltaRPM)
               b = Assign "y" (Fun (ApplyDeltaRPM "x"))
               c = Assign "z" (Fun (ReadRPMSymbols "y"))
               d = Assign "h" (Fun (DeltaRPMSymbols "z"))
               prog =  a :>>: (b :>>: (c :>>: d))
           (s, p') <- evalPreCondStoreT (wp prog TRUE) (empty)
           mapM (putStrLn . show) $ take 3 (toList s)

