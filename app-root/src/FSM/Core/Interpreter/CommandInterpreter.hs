module FSM.Core.Interpreter.CommandInterpreter (interpret) where

import           Control.Lens                 (At (at), _Just, filtered, non,
                                               traversed, (%~), (&), (^.),
                                               (^..))
import           Control.Monad.Error.Class    (MonadError)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           FSM.Core.Domain.Command      (Command (..))
import           FSM.Core.Domain.FileSystem   (Entry (File), FileSystemError, cpath, entryId,
                                               formatEntry, joinCont, reg)
import           FSM.Core.Domain.Path         (Path, isAbsolute, join)
import           FSM.Core.Domain.Types        (Filename)
import           FSM.Core.Effect.MonadConsole (MonadConsole (..))
import           FSM.Core.Effect.MonadFS      (MonadFS (getFS, modifyFS))

type Interpreter m = (MonadFS m, MonadConsole m, MonadError FileSystemError m)

interpret :: Interpreter m => Command -> m ()
interpret Exit             = return ()
interpret Help             = help
interpret (Ls mPath)       = ls mPath
interpret Pwd              = pwd
interpret (Echo msg mfile) = echo msg mfile

echo :: Interpreter m => Text -> Maybe Filename -> m ()
echo msg Nothing      = sendLine msg
echo msg (Just fname) = modifyFS $ \fs ->
    let currentPath = fs ^. cpath
        entries     = fs ^.. reg . at currentPath . _Just . traversed
    in case filter ((== fname) . entryId) entries of
        [] -> fs & reg
                . at currentPath
                . non []
                %~ (++ [File fname (Just msg)])
        _ -> fs & reg
                . at currentPath
                . non []
                . traversed
                . filtered ((== fname) . entryId)
                %~ (`joinCont` msg)

pwd :: Interpreter m => m ()
pwd = getFS >>= sendLine . (^. cpath)

ls :: Interpreter m => Maybe Path -> m ()
ls mPath = do
    fs <- getFS
    let currentPath       = fs ^. cpath
        whenAbsolute path = if isAbsolute path then path else join currentPath path
        targetPath        = maybe currentPath whenAbsolute mPath
        entries           = fs ^.. reg . at targetPath . _Just . traversed
    if null entries
        then sendLine "(empty directory)"
        else mapM_ (sendLine . formatEntry) entries

help :: Interpreter m => m ()
help = sendLine $ T.unlines
    [ "Available commands:"
    , "  help               - Show this help message"
    , "  pwd                - Print current working directory"
    , "  ls                 - List directory contents (current directory)"
    , "  ls <path>          - List directory contents at specified path"
    , "  echo <msg>         - Print message to console"
    , "  echo <msg> >> <f>  - Append message to file (creates file if not exists)"
    , "  exit               - Exit the program"
    ]
