module Main where

import Control.Monad
import Conduit
import Data.Conduit
import Data.Conduit.Internal.Pipe (Pipe(..))
import qualified Data.Conduit.Internal.Conduit as C
import qualified Data.Conduit.Internal.Pipe as P


main :: IO ()
main = runConduit $ (ConduitT h) .| loop
  where
    f = \rest  -> HaveOutput (rest  ()) 1
    g = (\_ -> ConduitT (\rest2 -> HaveOutput (rest2 ()) 2))
    h = \r -> f (\a -> unConduitT (g a) r)
    loop = (ConduitT (\f -> NeedInput (f . Just) (const $ f Nothing)) >>= (\mi -> handle mi))

    handle mi =
      case mi of
        Just i -> do
          liftIO $ print(i+1)
          loop
        Nothing -> return ()