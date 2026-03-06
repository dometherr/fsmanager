module FSM.Core.Interpreter.CommandInterpreter (interpret) where

import           Control.Lens                 (At (at), filtered, non,
                                               traversed, (%~), (&), (^..), (^.), _Just)
import           Control.Monad.Error.Class    (MonadError)
import qualified Data.Text                   as T
import           Data.Text                    (Text)
import           FSM.Core.Domain.Command      (Command (..))
import           FSM.Core.Domain.FileSystem   (Entry (..), FileSystemError, cpath, entryId,
                                               joinCont, reg)
import           FSM.Core.Domain.Types        (Filename)
import           FSM.Core.Effect.MonadConsole (MonadConsole (..))
import           FSM.Core.Effect.MonadFS      (MonadFS (getFS, modifyFS))

type Interpreter m = (MonadFS m, MonadConsole m, MonadError FileSystemError m)

interpret :: Interpreter m => Command -> m ()
interpret Exit             = return ()
interpret Help             = help
interpret Ls               = ls
interpret Pwd              = pwd
interpret (Echo msg mfile) = echo msg mfile

echo :: Interpreter m => Text -> Maybe Filename -> m ()
echo msg Nothing      = sendLine msg
echo msg (Just fname) = modifyFS $ \fs ->
    fs & reg
        . at (fs ^. cpath)
        . non []
        . traversed
        . filtered ((== fname) . entryId)
        %~ (`joinCont` msg)

pwd :: Interpreter m => m ()
pwd = getFS >>= sendLine . (^. cpath)

ls :: Interpreter m => m ()
ls = do
    fs <- getFS
    let entries = fs ^.. reg . at (fs ^. cpath) . _Just . traversed
    if null entries
        then sendLine "(empty directory)"
        else mapM_ (sendLine . formatEntry) entries

formatEntry :: Entry -> Text
formatEntry (File name _)     = name <> " (file)"
formatEntry (Directory path)  = path <> " (dir)"

help :: Interpreter m => m ()
help = sendLine $ T.unlines
    [ "Available commands:"
    , "  help               - Show this help message"
    , "  pwd                - Print current working directory"
    , "  ls                 - List directory contents"
    , "  echo <msg>         - Print message to console"
    , "  echo <msg> >> <f>  - Append message to file"
    , "  exit               - Exit the program"
    ]
