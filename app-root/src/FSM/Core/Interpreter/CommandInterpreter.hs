module FSM.Core.Interpreter.CommandInterpreter (interpret) where

import           Control.Lens                 (At (at), filtered, non,
                                               traversed, (%~), (&), (^.))
import           Control.Monad.Error.Class    (MonadError)
import           Data.Text                    (Text)
import           FSM.Core.Domain.Command      (Command (..))
import           FSM.Core.Domain.FileSystem   (Entry (..), FileSystemError,
                                               cpath, joinCont, reg)
import           FSM.Core.Domain.Types        (Filename)
import           FSM.Core.Effect.MonadConsole (MonadConsole (..))
import           FSM.Core.Effect.MonadFS      (MonadFS (getFS, modifyFS))

type Interpreter m = (MonadFS m, MonadConsole m, MonadError FileSystemError m)

interpret :: Interpreter m => Command -> m ()
interpret Exit             = return ()
interpret Pwd              = pwd
interpret (Echo msg mfile) = echo msg mfile

echo :: Interpreter m => Text -> Maybe Filename -> m ()
echo msg Nothing      = sendLine msg
echo msg (Just fname) = modifyFS $ \fs ->
    fs & reg
        . at [fs ^. cpath]
        . non []
        . traversed
        . filtered (\case (File n _) -> n == fname
                          _          -> False)
        %~ (`joinCont` msg)

pwd :: Interpreter m => m ()
pwd = getFS >>= sendLine . (^. cpath)
