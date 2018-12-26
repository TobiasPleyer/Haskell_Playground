#!/usr/bin/env stack
{- stack
  script
  --resolver lts-11.8
  --package base
  --package transformers
-}


{-
Example use case: Chained external commands that produce output on
stdout/stderr and can possibly fail.

In such a scenario the inner WriterT instance postulates that every
external command can produce output and the outer EitherT instance
postulates that any of these commands can fail and that we do not want
to procede with other commadns if that happens.

Note that the order of stacking matters: WriterT (EitherT) does behave
differently in the way that in case a command fails (Left) all previously
logged stuff from WriterT is discarded. That is probably not the desired
behavior if we want to debug or give the user a comprehensive summary/error
log.
-}

import Control.Monad (liftM, mapM_)
import Control.Monad.Trans.Writer.Lazy
import System.IO


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
instance Monad m => Functor (EitherT e m) where
  fmap f = EitherT . liftM (fmap f) . runEitherT
  {-# INLINE fmap #-}
instance Monad m => Applicative (EitherT e m) where
  pure a  = EitherT $ return (Right a)
  {-# INLINE pure #-}
  EitherT f <*> EitherT v = EitherT $ f >>= \mf -> case mf of
    Left  e -> return (Left e)
    Right k -> v >>= \mv -> case mv of
      Left  e -> return (Left e)
      Right x -> return (Right (k x))
  {-# INLINE (<*>) #-}
instance Monad m => Monad (EitherT e m) where
  return a = EitherT $ return (Right a)
  {-# INLINE return #-}
  m >>= k  = EitherT $ do
    a <- runEitherT m
    case a of
      Left  l -> return (Left l)
      Right r -> runEitherT (k r)
  {-# INLINE (>>=) #-}
  fail = EitherT . fail
  {-# INLINE fail #-}
-- | Analogous to 'Left'. Equivalent to 'throwError'.
left :: Monad m => e -> EitherT e m a
left = EitherT . return . Left
{-# INLINE left #-}

-- | Analogous to 'Right'. Equivalent to 'return'.
right :: Monad m => a -> EitherT e m a
right = return
{-# INLINE right #-}


type MyStack a = EitherT () (WriterT [String] IO) a


main = do
  let
    ewr1,ewr2,ewr3 :: MyStack ()
    ewl1 :: MyStack ()
    ewr1 = right ()
    ewr2 = EitherT $ writer (Right (), ["Test"])
    ewr3 = EitherT $ writer (Right (), ["END"])
    ewl1 = EitherT $ writer (Left (), ["BAD"])
    output :: MyStack Int
    output = right 2
    decide x = if x <= 0 then ewl1 else ewr3
  (res,logs) <- runWriterT $ runEitherT $ (ewr1 >> ewr2 >> output >>= decide)
  case res of
    Right _ -> putStrLn "SUCCESS"
    Left _ -> putStrLn "FAILURE"
  putStrLn "== LOGS =="
  mapM_ putStrLn logs
