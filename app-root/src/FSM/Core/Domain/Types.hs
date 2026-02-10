module FSM.Core.Domain.Types
    ( Argument
    , Content
    , Entries
    , Filename
    , Path
    , Registry
    ) where

import qualified Data.Map  as M
import           Data.Text (Text)

type Argument   = Text
type Content    = Text
type Entries a  = [a]
type Filename   = Text
type Registry a = M.Map [Path] (Entries a)
type Path       = Text
