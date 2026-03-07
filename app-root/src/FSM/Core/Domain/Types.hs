module FSM.Core.Domain.Types
    ( Argument
    , Content
    , Entries
    , module FSM.Core.Domain.Path
    , Filename
    , Registry
    ) where

import qualified Data.Map             as M
import           Data.Text            (Text)
import           FSM.Core.Domain.Path (Path)

type Argument   = Text
type Content    = Text
type Entries a  = [a]
type Filename   = Text
type Registry a = M.Map Path (Entries a)
