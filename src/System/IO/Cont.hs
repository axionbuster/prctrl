module System.IO.Cont (PromptTag, newPromptTag, prompt, control0) where

import GHC.Exts
import GHC.IO

data PromptTag a = PromptTag (PromptTag# a)

newPromptTag :: IO (PromptTag a)
newPromptTag = IO \s -> case newPromptTag# s of
  (# s1, a #) -> (# s1, PromptTag a #)

prompt :: PromptTag a -> IO a -> IO a
prompt (PromptTag t) a = IO (prompt# t (unIO a))

control0 :: PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
control0 (PromptTag t) a = IO (control0# t \b -> unIO (a \p -> IO (b (unIO p))))
