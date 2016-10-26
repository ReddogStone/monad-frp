module EventSignal where

import Control.Category
import Control.Arrow
import Prelude hiding (id, (.))

import Signal

data HeadlessFiniteSignal i o r
  = HeadlessFiniteSignal (i -> FiniteSignal i o r)

data FiniteSignal i o r
  = Running o (HeadlessFiniteSignal i o r)
  | Finished r
  deriving (Show)


instance Show r => Show (HeadlessFiniteSignal i o r) where
  show _ = "Headless"


instance Functor (FiniteSignal i o) where
  fmap f =
    (flip (>>=)) (pure . f)


instance Applicative (FiniteSignal i o) where
  pure =
    Finished
  sf <*> sx =
    sf >>= (\f -> sx >>= (\x -> pure $ f x))


instance Monad (FiniteSignal i o) where
  (Finished r) >>= f =
    f r
  (Running value (HeadlessFiniteSignal feed)) >>= f =
    Running value (HeadlessFiniteSignal (\x -> feed x >>= f))


type HeadlessEventSignal e i o = HeadlessFiniteSignal (Either e i) o (o, e)
type EventSignal e i o = FiniteSignal (Either e i) o (o, e)


alwaysHS :: Signal i o -> HeadlessEventSignal e i o
alwaysHS s@ (Signal _ (Headless feed)) =
  HeadlessFiniteSignal feed'
    where
      feed' (Left _) =
        alwaysS s
      feed' (Right x) =
        alwaysS (feed x)


alwaysS :: Signal i o -> EventSignal e i o
alwaysS s@ (Signal value headless) =
  Running value (alwaysHS s)


untilHS :: (e -> Bool) -> Signal i o -> HeadlessEventSignal e i o
untilHS testEvent s@ (Signal value (Headless feed)) =
  HeadlessFiniteSignal feed'
    where
      feed' (Left event) =
        if testEvent event then Finished (value, event) else untilS testEvent s
      feed' (Right x) =
        untilS testEvent (feed x)


untilS :: (e -> Bool) -> Signal i o -> EventSignal e i o
untilS testEvent s@ (Signal value headless) =
  Running value (untilHS testEvent s)


feedInputHS :: HeadlessEventSignal e i o -> i -> EventSignal e i o
feedInputHS (HeadlessFiniteSignal feed) =
  feed . Right


feedEventHS :: HeadlessEventSignal e i o -> e -> EventSignal e i o
feedEventHS (HeadlessFiniteSignal feed) =
  feed . Left


feedInputS :: EventSignal e i o -> i -> EventSignal e i o
feedInputS (Running _ headless) =
  feedInputHS headless


feedEventS :: EventSignal e i o -> e -> EventSignal e i o
feedEventS (Running _ headless) =
  feedEventHS headless
