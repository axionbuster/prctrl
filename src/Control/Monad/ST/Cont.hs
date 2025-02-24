module Control.Monad.ST.Cont (PromptTag, newPromptTag, prompt, control0) where

import GHC.Exts
import GHC.ST
import Unsafe.Coerce

data PromptTag s a = PromptTag (PromptTag# a)

newPromptTag :: ST s (PromptTag s a)
newPromptTag = ST $ unsafeCoerce \s -> case newPromptTag# s of
  (# s1, a #) -> (# s1, PromptTag a #)

prompt :: PromptTag s a -> ST s a -> ST s a
prompt (PromptTag t) (ST a) = ST (unsafeCoerce# (prompt# t (unsafeCoerce# a)))

unST :: ST s a -> STRep t a
unST (ST a) = unsafeCoerce# a

toST :: STRep s a -> ST t a
toST f = ST (unsafeCoerce# f)

control0 :: PromptTag s a -> ((ST s b -> ST s a) -> ST s a) -> ST s b
control0 (PromptTag t) a = toST do
  control0# t \b s -> unST (a \p -> toST (b (unST p))) (unsafeCoerce# s)
