module FSM.Core.Domain.FileSystem
    ( Entries
    , Entry (..)
    , FileSystem (..)
    , Registry
    ) where

import qualified Data.Map              as M
import           FSM.Core.Domain.Types (Content, Filename, Path)

type Registry = M.Map [Path] Entries
type Entries  = [Entry]

data Entry
    = File      Filename (Maybe Content)
    | Directory Path
    deriving (Show)

data FileSystem
    = FileSystem { reg  :: Registry, path :: Path }
    deriving (Show)
