module FSM.Core.Effect.MonadConsole (MonadConsole (..)) where

import           Data.Text              (Text)

class Monad m => MonadConsole m where
    sendFlush :: Text -> m ()
    sendLine  :: Text -> m ()
    readLine  :: m Text
