-- | Delimited control primitives for the IO monad.
--
-- This module provides first-class, multi-shot delimited control operators
-- that are natively supported by GHC. These primitives allow capturing and
-- manipulating continuations within the IO monad.
--
-- = Safety Notes
-- These primitives are unsafe if misused. To maintain safety:
--
-- * Every 'control0' must have a matching 'prompt' with the same tag
-- * Cannot be used within STM transactions
-- * Avoid capturing continuations that contain resource management operations
--   (e.g., bracket, withFile) until consequences of reentrancy are understood.
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
module System.IO.Cont (PromptTag, newPromptTag, prompt, control0) where

import GHC.Exts
import GHC.IO

-- | Tags are used to distinguish different control flows.
data PromptTag a = PromptTag (PromptTag# a)

-- | Create a new unique prompt tag for delimiting control effects.
newPromptTag :: IO (PromptTag a)
newPromptTag = IO \s -> case newPromptTag# s of
  (# s1, a #) -> (# s1, PromptTag a #)

-- | Establish a prompt (delimiter) for continuation capture.
-- Any matching 'control0' will capture up to this point.
prompt :: PromptTag a -> IO a -> IO a
prompt (PromptTag t) a = IO (prompt# t (unIO a))

-- | Capture the current continuation up to a matching prompt.
--
-- The continuation can be invoked multiple times or discarded.
--
-- === Safety
--
-- * Must have matching prompt with same tag in scope
-- * Avoid capturing continuations containing resource management
-- * Cannot be used in STM
control0 :: PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
control0 (PromptTag t) a = IO (control0# t \b -> unIO (a \p -> IO (b (unIO p))))
