module FSM.Core.Parser.CommandParser (Parser, parseCommand) where

import           Data.Text               (Text)
import qualified Data.Text               as T
import           FSM.Core.Domain.Command (Command (..), CommandError (..))

type Parser = Either CommandError Command

parseCommand :: Text -> Parser
parseCommand rawCmd =
    case T.words (T.strip rawCmd) of
        []             -> Left  NoCommandError
        ("echo":parts) -> parseEcho parts
        (command:_)    -> Left $ CommandNotFoundError
                               $ "command: " <> command <> " not implemented"

parseEcho :: [Text] -> Parser
parseEcho parts =
    let (msg, fparts) = break (== ">>") parts
        outputMsg     = T.unwords msg
    in case fparts of
        []             -> Right $ Echo outputMsg Nothing
        (_:filename:_) -> Right $ Echo outputMsg (Just filename)
        [_]            -> Left $ MissingArgumentError ["filename"]
