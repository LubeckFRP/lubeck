
import Lubeck.FRP
import BasePrelude hiding (Signal, Const)

data Expr = Input | Const Int | Add Expr Expr
  deriving (Eq, Ord, Read, Show)

interpret :: Signal Int -> Expr -> Signal Int
interpret i x = case x of
  Input    -> i
  Const n  -> pure n
  Add a b  -> liftA2 (+) (interpret i a) (interpret i b)

withInterp :: Signal Int -> Signal Expr -> Signal Int
withInterp is vs = vs >>= interpret is



testFRP2 :: (Read a, Read b, Show a, Show b, Show c) => a -> b -> (Signal a -> Signal b -> Signal c) -> IO ()
testFRP2 initA initB nw = do
  putStrLn "Enter values to send to A or B, '' for nothing, or 'Ctrl-C' to exit loop"
  (sinkA, eventA) <- newEvent
  (sinkB, eventB) <- newEvent
  sA <- stepperS initA $ fmap read eventA
  sB <- stepperS initB $ fmap read eventB
  forever $ loop sA sB sinkA sinkB (nw sA sB)
  where
    loop sA sB sinkA sinkB s = do
      a' <- pollBehavior (current sA)
      b' <- pollBehavior (current sB)
      v <- pollBehavior (current s)
      putStrLn $ "a: " <> show a' <> ", b: " <> show b' <> ", result: " <> show v
      putStr "A> "
      a <- getLine
      putStr "B> "
      b <- getLine
      case (a, b) of
        ("", "") -> return ()
        ("", b)  -> sinkB b
        (a, "")  -> sinkA a
        (a, b)   -> sinkA a >> sinkB b


main = testFRP2 0 Input withInterp
