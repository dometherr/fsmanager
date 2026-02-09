module FSM.Core.App (AppT (..), runAppT) where

import           Control.Monad.Error.Class  (MonadError)
import           Control.Monad.Except       (ExceptT, runExceptT)
import           Control.Monad.State        (MonadIO, MonadState (get, put),
                                             MonadTrans (lift),
                                             StateT (runStateT))
import           FSM.Core.Domain.Command    (CommandError)
import           FSM.Core.Domain.FileSystem (FileSystem)
import           FSM.Core.Effect.MonadFS    (MonadFS (..))

newtype AppT m a
    = AppT { unAppT :: ExceptT CommandError (StateT FileSystem m) a }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadError CommandError
        , MonadState FileSystem
        , MonadIO
        )

instance Monad m => MonadFS (AppT m) where
    getFS = AppT $ lift get
    putFS = AppT . lift . put

runAppT :: AppT m a -> FileSystem -> m (Either CommandError a, FileSystem)
runAppT = runStateT . runExceptT . unAppT
