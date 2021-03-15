module Matterhorn.Command where

import Data.Text ( Text )

import Matterhorn.Types ( MH, Cmd )

commandList :: [Cmd]
dispatchCommand :: Text -> MH ()
