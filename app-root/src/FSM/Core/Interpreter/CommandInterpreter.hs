module FSM.Core.Interpreter.CommandInterpreter (interpret) where

import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           FSM.Core.Domain.Command    (Command (..))
import           FSM.Core.Domain.FileSystem (FileSystemError)
import           FSM.Core.Domain.Types      (Filename)
import           FSM.Core.Effect.MonadFS    (MonadFS)

type Interpreter m = (MonadFS m, MonadIO m, MonadError FileSystemError m)

interpret :: Interpreter m => Command -> m ()
interpret Exit             = return ()
interpret (Echo msg mfile) = echo msg mfile

echo :: Interpreter m => Text -> Maybe Filename -> m ()
echo msg Nothing = liftIO $ TIO.putStrLn msg
echo _ (Just _)  = pure ()
