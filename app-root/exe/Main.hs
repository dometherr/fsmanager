module Main (main) where

import           Control.Lens                            ((^.))
import           Control.Monad                           (void)
import           Control.Monad.IO.Class                  (MonadIO (liftIO))
import qualified Data.Text                               as T
import qualified Data.Text.IO                            as TIO
import           FSM.Core.App                            (AppT, runAppT)
import           FSM.Core.Domain.Command                 (Command (Exit))
import           FSM.Core.Domain.FileSystem              (cpath, newFileSystem)
import           FSM.Core.Effect.MonadFS                 (MonadFS (getFS))
import           FSM.Core.Interpreter.CommandInterpreter (interpret)
import           FSM.Core.Parser.CommandParser           (parseCommand)
import           GHC.IO.Handle                           (hFlush)
import           System.IO                               (stdout)

main :: IO ()
main = do
    TIO.putStrLn "Welcome to the FileSystem Manager. Use help for usage details, or exit to quit the program!"
    void $ runAppT repl $ newFileSystem "/"

repl :: MonadIO m => AppT m ()
repl = do
    getFS >>= \fs -> liftIO $ putStrUnbuffered ("$" <> fs ^. cpath <> "> ")
    liftIO (parseCommand <$> TIO.getLine) >>= \case
        Right Exit    -> liftIO (TIO.putStrLn "Bye!")
        Right command -> interpret command                  >> repl
        Left err      -> liftIO (TIO.putStrLn $ T.show err) >> repl

putStrUnbuffered :: T.Text -> IO ()
putStrUnbuffered txt = TIO.putStr txt
                    >> hFlush stdout
