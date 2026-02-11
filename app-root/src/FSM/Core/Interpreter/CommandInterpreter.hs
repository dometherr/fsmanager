module FSM.Core.Interpreter.CommandInterpreter (interpret) where

import           Control.Lens               (At (at), filtered, non, traversed,
                                             (%~), (&), (^.))
import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           FSM.Core.Domain.Command    (Command (..))
import           FSM.Core.Domain.FileSystem (Entry (..), FileSystemError, cpath,
                                             joinCont, reg)
import           FSM.Core.Domain.Types      (Filename)
import           FSM.Core.Effect.MonadFS    (MonadFS (modifyFS))

type Interpreter m = (MonadFS m, MonadIO m, MonadError FileSystemError m)

interpret :: Interpreter m => Command -> m ()
interpret Exit             = return ()
interpret (Echo msg mfile) = echo msg mfile

echo :: Interpreter m => Text -> Maybe Filename -> m ()
echo msg Nothing      = liftIO $ TIO.putStrLn msg
echo msg (Just fname) = modifyFS $ \fs ->
    fs & reg
        . at [fs ^. cpath]
        . non []
        . traversed
        . filtered (\case (File n _) -> n == fname
                          _          -> False)
        %~ (`joinCont` msg)
