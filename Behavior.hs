module Behavior where


data Iteratee a m b
  = Done b
  | Cont (a -> Behavior a m b)


newtype Behavior a m b
  = Behavior { runBehavior :: m (Iteratee a m b) }


instance Monad m => Functor (Behavior a m) where
  fmap f b =
    Behavior (runBehavior b >>= processIteratee)
      where
        processIteratee (Done result) =
          return $ Done (f result)
        processIteratee (Cont feed) =
          return $ Cont (fmap f . feed)


instance Monad m => Applicative (Behavior a m) where
  pure =
    Behavior . return . Done

  b1 <*> b2 =
    b1 >>= (\f -> b2 >>= (\x -> return (f x)))


instance Monad m => Monad (Behavior a m) where
  b >>= f =
    Behavior (runBehavior b >>= processIteratee)
      where
        processIteratee (Done result) =
          runBehavior (f result)
        processIteratee (Cont feed) =
          return $ Cont (\x -> feed x >>= f)


onceB :: Monad m => Behavior a m a
onceB =
  Behavior (return $ Cont return)


repeatB :: Monad m => Behavior a m b -> Behavior a m c
repeatB b =
  b >> repeatB b


firstB :: Monad m => Behavior a m b -> Behavior a m b -> Behavior a m b
firstB b1 b2 =
  Behavior mIter
    where
      next :: Monad m => Iteratee a m b -> Iteratee a m b -> Iteratee a m b
      next (Done result) _ =
        Done result
      next _ (Done result) =
        Done result
      next (Cont feed1) (Cont feed2) =
        Cont (\x -> firstB (feed1 x) (feed2 x))

      mIter = do
        iter1 <- runBehavior b1
        iter2 <- runBehavior b2
        return $ next iter1 iter2


firstBL :: Monad m => [Behavior a m b] -> Behavior a m b
-- firstBL [] =
--   error "firstBL should be called on non-empty lists"
firstBL =
  foldr1 firstB


effectB :: Monad m => m a -> Behavior b m a
effectB effect =
  Behavior (effect >>= return . Done)


waitForB :: Monad m => (a -> Bool) -> Behavior a m a
waitForB test = do
  x <- onceB
  if test x then return x else waitForB test


whileB :: Monad m => Behavior a m Bool -> Behavior a m b
whileB behavior =
  behavior >>= (\result -> if result then whileB behavior else return undefined)