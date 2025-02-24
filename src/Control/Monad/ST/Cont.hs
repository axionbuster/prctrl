-- | Delimited control primitives for the strict ST monad.
--
-- This module provides first-class, multi-shot delimited control operators
-- that are natively supported by GHC. These primitives allow capturing and
-- manipulating continuations within the strict ST monad.
--
-- = Safety Notes
-- These primitives are unsafe if misused. To maintain safety:
--
-- * Every 'control0' must have a matching 'prompt' with the same tag
-- * Continuations cannot cross between different state threads (the @s@
-- type in @'ST' s a@)
--
-- = Example Usage
-- @
-- do tag <- newPromptTag
--    prompt tag $ do
--      -- Continuation can be captured in this scope
--      control0 tag $ \k -> do
--        -- k is the captured continuation
--        k (pure someValue)
-- @
--
-- = References
-- See GHC documentation for prompt#/control0# primitives.
module Control.Monad.ST.Cont (PromptTag, newPromptTag, prompt, control0) where

import GHC.Exts
import GHC.ST
import Unsafe.Coerce

-- | Tags are used to distinguish different control flows.
data PromptTag s a = PromptTag (PromptTag# a)

-- | Create a new unique prompt tag for delimiting control effects.
-- Tags are specific to a particular state thread @s@.
newPromptTag :: ST s (PromptTag s a)
newPromptTag = ST $ unsafeCoerce \s -> case newPromptTag# s of
  (# s1, a #) -> (# s1, PromptTag a #)

-- | Establish a prompt (delimiter) for continuation capture.
-- Any matching 'control0' will capture up to this point.
prompt :: PromptTag s a -> ST s a -> ST s a
prompt (PromptTag t) (ST a) = ST (unsafeCoerce# (prompt# t (unsafeCoerce# a)))

-- | Capture the current continuation up to a matching prompt.
--
-- The continuation can be invoked multiple times or discarded.
--
-- === Safety
--
-- * Must have matching prompt with same tag in scope
-- * Cannot cross state thread boundaries
control0 :: PromptTag s a -> ((ST s b -> ST s a) -> ST s a) -> ST s b
control0 (PromptTag t) a = toST do
  control0# t \b s -> unST (a \p -> toST (b (unST p))) (unsafeCoerce# s)

unST :: ST s a -> STRep t a
unST (ST a) = unsafeCoerce# a

toST :: STRep s a -> ST t a
toST f = ST (unsafeCoerce# f)
