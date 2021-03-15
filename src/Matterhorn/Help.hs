{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Matterhorn.Help
  ( helpPandoc
  , printArgSpec
  )
where

import Prelude ()
import Matterhorn.Prelude

import Matterhorn.Events.Keybindings
import Matterhorn.Types
import Matterhorn.Types.KeyEvents

import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Builder

helpPandoc :: [Cmd] -> [(Text, [KeyEventHandler])] -> KeyConfig -> Pandoc
helpPandoc commandList kbds kc = setTitle "Matterhorn Help" $ doc $
  header 1 "Matterhorn Help" <>
  header 2 "General Usage" <>
  header 3 "Commands" <> commands commandList <>
  header 3 "Keybindings" <> keybindings kc kbds <>
  header 2 "Using Scripts" <> scriptHelp <>
  header 2 "Using Themes" <>
  header 2 "Configurable Keybindings" <>
  header 2 "Syntax Highlighting"

commands :: [Cmd] -> Blocks
commands commandList = bulletList
  [ para info <>
    para (text desc)
  | Cmd c desc args _ <- commandList
  , let argSpec = printArgSpec args
        spc = if T.null argSpec then "" else " "
        info = text $ "/" <> c <> spc <> argSpec
  ]

keybindings :: KeyConfig -> [(Text, [KeyEventHandler])] -> Blocks
keybindings kc kbds = mconcat (keybindingSection kc <$> kbds)

keybindingSection :: KeyConfig -> (Text, [KeyEventHandler]) -> Blocks
keybindingSection kc (sectionName, kbs) =
  header 4 (text sectionName) <>
  bulletList (snd <$> sortWith fst (keybindingHelp kc <$> kbs))

keybindingHelp :: KeyConfig -> KeyEventHandler -> (Text, Blocks)
keybindingHelp kc h =
  let unbound = ["(unbound)"]
      label = case kehEventTrigger h of
          Static k -> ppBinding $ eventToBinding k
          ByEvent ev ->
              let bindings = case M.lookup ev kc of
                      Nothing ->
                          let bs = defaultBindings ev
                          in if not $ null bs
                             then ppBinding <$> defaultBindings ev
                             else unbound
                      Just Unbound -> unbound
                      Just (BindingList bs) | not (null bs) -> ppBinding <$> bs
                                            | otherwise -> unbound
              in T.intercalate ", " bindings

      rendering = para (text label) <>
                  para (text $ ehDescription $ kehHandler h)
  in (label, rendering)

scriptHelp :: Blocks
scriptHelp = scriptHelpText
  where scriptHelpText = mconcat $ para <$>
          [ text $ T.concat
            [ "Matterhorn has a special feature that allows you to use "
             , "prewritten shell scripts to preprocess messages. "
             , "For example, this can allow you to run various filters over "
             , "your written text, do certain kinds of automated formatting, "
             , "or just automatically cowsay-ify a message." ]
           , text $ T.concat
             [ "These scripts can be any kind of executable file, "
             , "as long as the file lives in "
             , "*~/.config/matterhorn/scripts* (unless you've explicitly "
             , "moved your XDG configuration directory elsewhere). "
             , "Those executables are given no arguments "
             , "on the command line and are passed your typed message on "
             , "*stdin*; whatever they produce on *stdout* is sent "
             , "as a message. If the script exits successfully, then everything "
             , "that appeared on *stderr* is discarded; if it instead exits with "
             , "a failing exit code, your message is *not* sent, and you are "
             , "presented with whatever was printed on stderr as a "
             , "local error message." ]
           , text $ T.concat [ "To run a script, simply type" ]
           , code "> > /sh [script-name] [my-message]*"
           , text $ T.concat [ "And the script named *[script-name]* will be invoked with "
             , "the text of *[my-message]*. If the script does not exist, "
             , "or if it exists but is not marked as executable, you'll be "
             , "presented with an appropriate error message." ]
           , text $ T.concat [ "For example, if you want to use a basic script to "
             , "automatically ROT13 your message, you can write a shell "
             , "script using the standard Unix *tr* utility, like this:" ]
           , code "> #!/bin/bash -e> tr '[A-Za-z]' '[N-ZA-Mn-za-m]'*"
           , text $ T.concat
             [ "Move this script to *~/.config/matterhorn/scripts/rot13* "
             , "and be sure it's executable with" ]
           , code "> $ chmod u+x ~/.config/matterhorn/scripts/rot13*"
           , text $ T.concat
             [ "after which you can send ROT13 messages with the "
             , "Matterhorn command " ]
           , code "> > /sh rot13 Hello, world!*" 
           ]
