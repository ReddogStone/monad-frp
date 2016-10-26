import Control.Category
import Control.Arrow
import Prelude hiding (id, (.))

import Control.Monad.State.Lazy
import Data.Maybe

import Behavior
import Signal
import qualified EventSignal as ES


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


data Message
  = Down
  | Up
  | Update Double
  deriving (Show, Read)

isDown :: Message -> Bool
isDown message = case message of (Down {}) -> True; _ -> False 

isUp :: Message -> Bool
isUp message = case message of (Up {}) -> True; _ -> False

isUpdate :: Message -> Bool
isUpdate message = case message of (Update {}) -> True; _ -> False


data Event
  = StartPowerUp
  | FinishPowerUp
  deriving (Show, Eq)

isStartPowerUp :: Event -> Bool
isStartPowerUp event = case event of (StartPowerUp {}) -> True; _ -> False 

isFinishPowerUp :: Event -> Bool
isFinishPowerUp event = case event of (FinishPowerUp {}) -> True; _ -> False


putB = effectB . put
sampleB = effectB get >>= (return . valueS)


timeS :: Double -> Signal Message Double
timeS start =
  Signal start (filtered >>> acc)
    where
      filtered =
        filterHS isUpdate >>> arr (>>= (\(Update dt) -> return dt))
      acc =
        accumulateHS (\t maybeDt -> t + (fromMaybe 0 maybeDt)) start


update f =
  whileB $ do
    (Update dt) <- waitForB isUpdate
    s <- sampleB
    return $ f s dt


test = toFiniteSignal (constS 10) $ do
  waitForB isDown
  putB $ timeS 20
  firstBL
    [
      update (\s _ -> s < 30),
      waitForB isUp
    ]
  sampleB


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
          main' (feed (read s :: Message))
