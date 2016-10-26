import Behavior
import Signal
import Control.Monad.State.Lazy
import Data.Maybe

import Control.Category
import Control.Arrow
import Prelude hiding (id, (.))


data FiniteHeadlessSignal i o r
  = Finish r
  | Run (i -> FiniteSignal i o r)

data FiniteSignal i o r
  = FiniteSignal o (FiniteHeadlessSignal i o r)

toFiniteHeadlessSignal :: (HeadlessSignal i o) -> (Iteratee i (State (Signal i o)) r) -> FiniteHeadlessSignal i o r
toFiniteHeadlessSignal _ (Done result) =
  Finish result
toFiniteHeadlessSignal (Headless next) (Cont feed) =
  Run (\x -> toFiniteSignal (next x) (feed x))


toFiniteSignal :: (Signal i o) -> (Behavior i (State (Signal i o)) r)-> FiniteSignal i o r
toFiniteSignal lastSignal behavior =
  FiniteSignal output headless
    where
      (iteratee, (Signal output hs)) = runState (runBehavior behavior) lastSignal
      headless = toFiniteHeadlessSignal hs iteratee


data Event
  = Down
  | Up
  | Update Double
  deriving (Show, Read)

isDown :: Event -> Bool
isDown event = case event of Down -> True; _ -> False 

isUp :: Event -> Bool
isUp event = case event of Up -> True; _ -> False

isUpdate :: Event -> Bool
isUpdate event = case event of (Update _) -> True; _ -> False


putB = effectB . put
getB = effectB get


timeS :: Double -> Signal Event Double
timeS start =
  Signal start (filtered >>> acc)
    where
      filtered =
        filterHS isUpdate >>> arr (>>= (\(Update dt) -> return dt))
      acc =
        accumulateHS (\t maybeDt -> t + (fromMaybe 0 maybeDt)) start


test = toFiniteSignal (constS 10) $ do
  waitForB isDown
  putB $ timeS 20
  waitForB isUp
  s <- getB
  return (valueS s)


main :: IO ()
main = main' test
  where
    main' (FiniteSignal value headless) = do
      putStrLn $ "Current " ++ (show value)
      case headless of
        Finish result ->
          putStrLn $ "Finished " ++ (show result)
        Run feed -> do
          s <- getLine
          main' (feed (read s :: Event))
