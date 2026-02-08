module FSM.Core.Domain.Command (Command (..), CommandError (..)) where

import           Data.Text (Text)

type Filename = Text
type Argument = Text

data Command where
    Echo :: Text -> Maybe Filename -> Command
    -- ^ Echo a message or append to a file
    deriving (Show)

data CommandError
    = MissingArgumentError [Argument]
    -- ^ The command failed due to missing arguments
    | CommandNotFoundError Text
    -- ^ The command is not supported
    | NoCommandError
    -- ^ No command was passed
    deriving (Show)
