module Utils where

import ClassyPrelude.Yesod

orElse :: Monad m => Maybe a -> m a -> m a
Nothing `orElse` e = e
Just x `orElse` _ = return x

orElseM :: Monad m => m (Maybe a) -> m a -> m a
m `orElseM` e = do
    val <- m
    case val of
      Just x -> return x
      Nothing -> e
