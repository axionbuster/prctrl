-- Exceptions in the ST monad using prompt# and control0#

import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Cont
import Data.Functor
import Debug.Trace

-- | Exception effect (strict ST monad).
newtype H s e a = H (PromptTag s (Either e a))

-- | Run the exception effect.
exceptionally :: (H s e a -> ST s a) -> ST s a
exceptionally = (H <$> newPromptTag >>=)

-- | Try out.
try :: ST s a -> H s e a -> ST s (Either e a)
try action (H tag) = prompt tag do Right <$> action

-- | Throw.
throw :: e -> H s e a -> ST s b
throw e (H tag) = control0 tag \_ -> pure (Left e)

-- | Catch.
catch :: ST s a -> (e -> ST s a) -> H s e a -> ST s a
catch action handler h =
  try action h >>= \case
    Left e -> handler e
    Right a -> pure a

-- example

data DivError = DivByZero | Overflow
  deriving (Show)

divide :: Int -> Int -> H s DivError a -> ST s Int
divide n d h = do
  unless (d /= 0) do throw DivByZero h
  when (n == minBound && d == (-1)) do throw Overflow h
  pure (n `div` d)

main :: IO ()
main = do
  let handlediv DivByZero = traceM "division by zero" $> 0
      handlediv Overflow = traceM "integer division overflow" $> 0
      case1 = runST (exceptionally (\h -> catch (divide 1 0 h) handlediv h))
      case2 =
        runST (exceptionally (\h -> catch (divide minBound (-1) h) handlediv h))
      case3 = runST (exceptionally (\h -> catch (divide 19 5 h) handlediv h))
  traceShowM case1
  traceShowM case2
  traceShowM case3

{-
division by zero
0
integer division overflow
0
3
-}
