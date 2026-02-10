module FSM.Core.Domain.FileSystem
    ( Entry (..)
    , FileSystem (..)
    , FileSystemError (..)
    , Registry
    , entryId
    , newFileSystem
    ) where

import qualified Data.Map              as M
import           Data.Text             (Text)
import           FSM.Core.Domain.Types (Content, Filename, Path, Registry)

data Entry
    = File      Filename (Maybe Content)
    -- ^ File abstraction composed of a name and nullable content
    | Directory Path
    -- ^ Directory that just has a path
    deriving (Show)

data FileSystem
    = FileSystem
          { reg  :: Registry Entry -- ^ The file system registry
          , path :: Path           -- ^ The current path of the registry
          }
    deriving (Show)

data FileSystemError 
    = OperationNotPermittedError Text
    -- ^ The operation is not permitted due to business constraints
    | EntryNotFoundError         Text
    -- ^ The required entry was not found in the File System
    deriving (Show)

instance Eq Entry where
    entry == entry' = entryId entry == entryId entry'

newFileSystem :: Path -> FileSystem
newFileSystem basePath = 
    FileSystem { reg  = M.fromList [([basePath], mempty)]
               , path = basePath
               }

entryId :: Entry -> Text
entryId (File name _)    = name
entryId (Directory path) = path
