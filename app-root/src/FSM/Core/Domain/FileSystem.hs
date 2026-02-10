module FSM.Core.Domain.FileSystem
    ( Entry (..)
    , FileSystem (..)
    , FileSystemError (..)
    , Registry
    , cpath
    , dpath
    , fname
    , mcontent
    , reg
    , newFileSystem
    ) where

import           Control.Lens.TH       (makeLenses)
import qualified Data.Map              as M
import           Data.Text             (Text)
import           FSM.Core.Domain.Types (Content, Filename, Path, Registry)

data Entry
    = File      { _fname :: Filename, _mcontent :: Maybe Content }
    -- ^ File abstraction composed of a name and nullable content
    | Directory { _dpath :: Path}
    -- ^ Directory that just has a path
    deriving (Show)

makeLenses ''Entry

data FileSystem
    = FileSystem
          { _reg   :: Registry Entry -- ^ The file system registry
          , _cpath :: Path           -- ^ The current path of the registry
          }
    deriving (Show)

makeLenses ''FileSystem

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
    let registry = M.fromList [([basePath], mempty)]
     in FileSystem registry basePath

entryId :: Entry -> Text
entryId (File name _)    = name
entryId (Directory path) = path
