module FSM.Core.Interpreter.CommandInterpreter (interpret) where

import           Control.Lens                 (At (at), filtered, non,
                                               traversed, (%~), (&), (^.))
import           Control.Monad.Error.Class    (MonadError)
import qualified Data.Text                   as T
import           Data.Text                    (Text)
import           FSM.Core.Domain.Command      (Command (..))
import           FSM.Core.Domain.FileSystem   (FileSystemError, cpath, entryId,
                                               joinCont, reg)
import           FSM.Core.Domain.Types        (Filename)
import           FSM.Core.Effect.MonadConsole (MonadConsole (..))
import           FSM.Core.Effect.MonadFS      (MonadFS (getFS, modifyFS))

type Interpreter m = (MonadFS m, MonadConsole m, MonadError FileSystemError m)

interpret :: Interpreter m => Command -> m ()
interpret Exit             = return ()
interpret Help             = help
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

help :: Interpreter m => m ()
help = sendLine $ T.unlines
    [ "Available commands:"
    , "  help               - Show this help message"
    , "  pwd                - Print current working directory"
    , "  echo <msg>         - Print message to console"
    , "  echo <msg> >> <f>  - Append message to file"
    , "  exit               - Exit the program"
    ]
