{-# LANGUAGE LambdaCase #-}

module Matterhorn.State.Help
  ( showHelpScreen
  , showHelpBrowser
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( viewportScroll, vScrollToBeginning )

import {-# SOURCE #-} Matterhorn.Command ( commandList )
import           Matterhorn.Events ( keybindSections )
import           Matterhorn.FilePaths
import           Matterhorn.Help
import           Matterhorn.State.Links
import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.Types.RichText

import           Data.Default
import           Data.Text ( pack, unpack )
import           Data.Version ( showVersion )
import           Paths_matterhorn ( version )
import           System.Directory ( doesFileExist )
import           System.Environment.XDG.BaseDir ( getUserConfigFile )
import           Text.Pandoc ( runIOorExplode
                             , writeHtml4String
                             , WriterOptions(..)
                             , Pandoc
                             )
import           Text.Pandoc.SelfContained ( makeSelfContained )

showHelpScreen :: HelpTopic -> MH ()
showHelpScreen topic = do
    curMode <- use (csCurrentTeam.tsMode)
    case curMode of
        ShowHelp {} -> return ()
        _ -> do
            mh $ vScrollToBeginning (viewportScroll HelpViewport)
            setMode $ ShowHelp topic curMode

showHelpBrowser' :: (KeyConfig -> Pandoc) -> MH ()
showHelpBrowser' mkPandoc = do
  docsPath <- liftIO docsHtmlFilePath
  docsExist <- liftIO $ doesFileExist docsPath
  kc <- use (csResources.crConfiguration.configUserKeysL)
  when (not docsExist) $ do
    html' <- liftIO $ runIOorExplode $ writeHtml4String opts (mkPandoc kc)
    html <- liftIO $ runIOorExplode $ makeSelfContained html'
    liftIO $ writeFile docsPath (unpack html)

  _ <- openLinkTarget (LinkURL (URL (pack docsPath)))
  return ()

  where opts = def { writerTableOfContents = True
                   , writerNumberSections = True
                   }

showHelpBrowser :: MH ()
showHelpBrowser = showHelpBrowser' (helpPandoc commandList keybindSections)

docsHtmlFileName :: FilePath
docsHtmlFileName = "matterhorn_docs_" ++ showVersion version ++ ".html"

docsHtmlFilePath :: IO FilePath
docsHtmlFilePath = getUserConfigFile xdgName docsHtmlFileName
