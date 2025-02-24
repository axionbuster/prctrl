import Control.Monad
import Control.Monad.Fix
import Data.IORef
import System.IO.Cont
import Text.Printf

main :: IO ()
main = do
  ref <- newIORef (0 :: Int)
  tag <- newPromptTag
  prompt tag do
    printf "hi\n"
    x <- control0 tag \k -> do
      fix \r -> do
        n <- readIORef ref
        when (n < 5) do
          printf "n was %d\n" n
          k (pure n) *> k (pure (n * 100))
          r
    printf "x was %d\n" x
    modifyIORef ref succ
  printf "fin\n"
