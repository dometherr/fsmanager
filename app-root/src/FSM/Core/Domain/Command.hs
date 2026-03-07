module FSM.Core.Domain.Command (Command (..), CommandError (..)) where

import           Data.Text             (Text)
import           FSM.Core.Domain.Types (Argument, Filename, Path)

data Command where
    Echo :: Text -> Maybe Filename -> Command
    -- ^ Echo a message or append to a file
    Pwd  :: Command
    -- ^ Print the current working directory
    Ls   :: Maybe Path -> Command
    -- ^ List directory contents (optionally at a specific path)
    Help :: Command
    -- ^ Show available commands
    Exit :: Command
    -- ^ Terminates the program
    deriving (Eq, Show)

data CommandError
    = MissingArgumentError [Argument]
    -- ^ The command failed due to missing arguments
    | CommandNotFoundError Text
    -- ^ The command is not supported
    | NoCommandError
    -- ^ No command was passed
    deriving (Show)
