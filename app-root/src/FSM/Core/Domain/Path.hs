module FSM.Core.Domain.Path (Path, isAbsolute, join) where

import           Data.Text (Text)
import qualified Data.Text as T

type Path = Text

-- | Check if a path is absolute (starts with '/')
{-# NOINLINE isAbsolute #-}
isAbsolute :: Path -> Bool
isAbsolute = T.isPrefixOf "/"

-- | Join a base path with a relative path
--   Ensures only one '/' separator between path components
{-# NOINLINE join #-}
join :: Path -> Path -> Path
join base rel = T.intercalate "/" 
              . filter (not . T.null) 
              $ [base, rel]
