module FSM.Core.Domain.FileSystem
    ( Entries
    , Entry (..)
    , FileSystem (..)
    , Registry
    , newFileSystem
    , pathEntries
    ) where

import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import           FSM.Core.Domain.Types (Content, Filename, Path)

type Registry = M.Map [Path] Entries
type Entries  = [Entry]

data Entry
    = File      Filename (Maybe Content)
    -- ^ File abstraction composed of a name and nullable content
    | Directory Path
    -- ^ Directory that just has a path
    deriving (Show)

data FileSystem
    = FileSystem
          { reg  :: Registry -- ^ The file system registry
          , path :: Path     -- ^ The current path of the registry
          }
    deriving (Show)

newFileSystem :: Path -> FileSystem
newFileSystem basePath = 
    FileSystem { reg  = M.fromList [([basePath], mempty)]
               , path = basePath
               }

pathEntries :: [Path] -> Registry -> Entries
pathEntries paths = fromMaybe [] . M.lookup paths
