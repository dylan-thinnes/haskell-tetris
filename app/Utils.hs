module Utils where

between :: Integral a => a -> a -> a -> Bool
between x min max = min <= x && x <= max

cycleC, cycleCC :: (Eq a, Bounded a, Enum a) => a -> a
cycleC a = if a == maxBound then minBound else succ a
cycleCC a = if a == minBound then maxBound else pred a

doWhile :: (Monad m) => [a] -> (a -> m Bool) -> m Bool
doWhile []     action = return True
doWhile (b:bs) action = do
    result <- action b
    case result of
      True  -> doWhile bs action
      False -> return False

test = doWhile [1..11] $ \i -> do
    print i
    return (i < 10)
