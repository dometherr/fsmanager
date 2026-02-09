module FSM.Core.Effect.MonadFS (MonadFS (..)) where

import           FSM.Core.Domain.FileSystem (FileSystem)

class Monad m => MonadFS m where
    getFS    :: m FileSystem
    putFS    :: FileSystem -> m ()
    modifyFS :: (FileSystem -> FileSystem) -> m ()
    modifyFS f = getFS >>= putFS . f
