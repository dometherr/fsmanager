module FSM.Core.Effect.MonadConsole (MonadConsole (..)) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (Text)

class MonadIO m => MonadConsole m where
    sendFlush :: Text -> m ()
    sendLine  :: Text -> m ()
    readLine  :: m Text
