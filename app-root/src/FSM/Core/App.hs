module FSM.Core.App (AppT (..), runAppT) where

import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.Except       (ExceptT, runExceptT)
import           Control.Monad.State        (MonadIO, MonadState (get, put),
                                             MonadTrans (lift),
                                             StateT (runStateT))
import           FSM.Core.Domain.FileSystem (FileSystem, FileSystemError)
import           FSM.Core.Effect.MonadFS    (MonadFS (..))

newtype AppT m a
    = AppT { unAppT :: ExceptT FileSystemError (StateT FileSystem m) a }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadError FileSystemError
        , MonadState FileSystem
        , MonadIO
        )

instance Monad m => MonadFS (AppT m) where
    getFS = AppT $ lift get
    putFS = AppT . lift . put

runAppT :: AppT m a -> FileSystem -> m (Either FileSystemError a, FileSystem)
runAppT = runStateT . runExceptT . unAppT
