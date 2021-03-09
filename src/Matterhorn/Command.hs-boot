module Matterhorn.Command where

import Data.Text ( Text )

import Matterhorn.Types ( MH, Cmd )

commandList :: [Cmd]
-- printArgSpec :: CmdArgs a -> Text
dispatchCommand :: Text -> MH ()
