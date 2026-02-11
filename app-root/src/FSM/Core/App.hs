module FSM.Core.App (AppT (..), runAppT) where

import           Control.Monad.Error.Class    (MonadError)
import           Control.Monad.Except         (ExceptT, runExceptT)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.State          (MonadState (get, put),
                                               StateT (runStateT))
import           Control.Monad.Trans          (MonadTrans (lift))
import qualified Data.Text.IO                 as TIO
import           FSM.Core.Domain.FileSystem   (FileSystem, FileSystemError)
import           FSM.Core.Effect.MonadConsole (MonadConsole (..))
import           FSM.Core.Effect.MonadFS      (MonadFS (..))
import           GHC.IO.Handle                (hFlush)
import           System.IO                    (stdout)

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

instance MonadIO m => MonadConsole (AppT m) where
    sendFlush t = liftIO (TIO.putStr t >> hFlush stdout)
    sendLine    = liftIO . TIO.putStrLn
    readLine    = liftIO TIO.getLine

runAppT :: AppT m a -> FileSystem -> m (Either FileSystemError a, FileSystem)
runAppT = runStateT . runExceptT . unAppT
