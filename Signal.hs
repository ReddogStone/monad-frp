module Signal where

import Control.Category
import Control.Arrow
import Prelude hiding (id, (.))

newtype HeadlessSignal a b = Headless (a -> Signal a b)
data Signal a b = Signal b (HeadlessSignal a b)


instance Show b => Show (Signal a b) where
  show (Signal value _) =
    "Signal " ++ (show value)


instance Category HeadlessSignal where
  id =
    arr id

  (Headless feed2) . (Headless feed1) =
    Headless ( \x ->
      let
        (Signal y next1) = feed1 x
        (Signal z next2) = feed2 y
      in
        Signal z (next2 . next1) )


instance Arrow HeadlessSignal where
  arr f =
    Headless (\x -> Signal (f x) (arr f))

  first (Headless feed) =
    Headless ( \(x, y) ->
      let
        (Signal x' next) = feed x
      in
        Signal (x', y) (first next) )


valueS :: Signal a b -> b
valueS (Signal value _) =
  value


constS :: b -> Signal a b
constS value =
  Signal value (constHS value)


constHS :: b -> HeadlessSignal a b
constHS value =
  arr (const value)


filterS :: (a -> Bool) -> Signal a (Maybe a)
filterS test =
  Signal Nothing (filterHS test)


filterHS :: (a -> Bool) -> HeadlessSignal a (Maybe a)
filterHS test =
  arr (\x -> if test x then Just x else Nothing)


accumulateS :: (b -> a -> b) -> b -> Signal a b
accumulateS f start =
  Signal start (accumulateHS f start)


accumulateHS :: (b -> a -> b) -> b -> HeadlessSignal a b
accumulateHS f start =
  Headless (\x -> accumulateS f (f start x))


feedListS :: HeadlessSignal a b -> [a] -> [b]
feedListS _ [] =
  []
feedListS (Headless feed) (x:xs) =
  value : (feedListS next xs)
    where
      (Signal value next) = feed x


class HasSignal a where
  toSignal :: a -> HeadlessSignal i o
