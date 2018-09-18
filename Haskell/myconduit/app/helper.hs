
yieldManyS :: (Monad m, MonoFoldable mono)
            => mono
            -> StreamProducer m (Element mono)
yieldManyS mono _ =
    Stream (return . step) (return (otoList mono))
  where
    step [] = Stop ()
    step (x:xs) = Emit xs x
yieldManyC = ofoldMap yield
yieldMany x = unstream (streamConduit (yieldManyC x) (yieldManyS x))
type StreamProducer m o = forall i. StreamConduitT i o m ()
streamConduit :: ConduitT i o m r
              -> (Stream m i () -> Stream m o r)
              -> ConduitWithStream i o m r
streamConduit = ConduitWithStream

