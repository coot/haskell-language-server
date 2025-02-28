-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}

module Development.IDE.LSP.Notifications
    ( whenUriFile
    , descriptor
    , Log(..)
    , ghcideNotificationsPluginPriority
    ) where

import qualified Language.LSP.Protocol.Message         as LSP
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types           as LSP

import           Control.Concurrent.STM.Stats          (atomically)
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict                   as HM
import qualified Data.HashSet                          as S
import qualified Data.Text                             as Text
import           Development.IDE.Core.FileExists       (modifyFileExists,
                                                        watchedGlobs)
import           Development.IDE.Core.FileStore        (registerFileWatches,
                                                        resetFileStore,
                                                        setFileModified,
                                                        setSomethingModified)
import qualified Development.IDE.Core.FileStore        as FileStore
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.OfInterest       hiding (Log, LogShake)
import           Development.IDE.Core.RuleTypes        (GetClientSettings (..))
import           Development.IDE.Core.Service          hiding (Log, LogShake)
import           Development.IDE.Core.Shake            hiding (Log, Priority)
import qualified Development.IDE.Core.Shake            as Shake
import           Development.IDE.Types.Location
import           Development.IDE.Types.Shake           (toKey)
import           Ide.Logger
import           Ide.Types
import           Numeric.Natural

data Log
  = LogShake Shake.Log
  | LogFileStore FileStore.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogShake log     -> pretty log
    LogFileStore log -> pretty log

whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . toNormalizedFilePath'

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId) { pluginNotificationHandlers = mconcat
  [ mkPluginNotificationHandler LSP.SMethod_TextDocumentDidOpen $
      \ide vfs _ (DidOpenTextDocumentParams TextDocumentItem{_uri,_version}) -> liftIO $ do
      atomically $ updatePositionMapping ide (VersionedTextDocumentIdentifier _uri _version) []
      whenUriFile _uri $ \file -> do
          -- We don't know if the file actually exists, or if the contents match those on disk
          -- For example, vscode restores previously unsaved contents on open
          addFileOfInterest ide file Modified{firstOpen=True}
          setFileModified (cmapWithPrio LogFileStore recorder) (VFSModified vfs) ide False file
          logDebug (ideLogger ide) $ "Opened text document: " <> getUri _uri

  , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidChange $
      \ide vfs _ (DidChangeTextDocumentParams identifier@VersionedTextDocumentIdentifier{_uri} changes) -> liftIO $ do
        atomically $ updatePositionMapping ide identifier changes
        whenUriFile _uri $ \file -> do
          addFileOfInterest ide file Modified{firstOpen=False}
          setFileModified (cmapWithPrio LogFileStore recorder) (VFSModified vfs) ide False file
        logDebug (ideLogger ide) $ "Modified text document: " <> getUri _uri

  , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidSave $
      \ide vfs _ (DidSaveTextDocumentParams TextDocumentIdentifier{_uri} _) -> liftIO $ do
        whenUriFile _uri $ \file -> do
            addFileOfInterest ide file OnDisk
            setFileModified (cmapWithPrio LogFileStore recorder) (VFSModified vfs) ide True file
        logDebug (ideLogger ide) $ "Saved text document: " <> getUri _uri

  , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidClose $
        \ide vfs _ (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> liftIO $ do
          whenUriFile _uri $ \file -> do
              deleteFileOfInterest ide file
              let msg = "Closed text document: " <> getUri _uri
              scheduleGarbageCollection ide
              setSomethingModified (VFSModified vfs) ide [] $ Text.unpack msg
              logDebug (ideLogger ide) msg

  , mkPluginNotificationHandler LSP.SMethod_WorkspaceDidChangeWatchedFiles $
      \ide vfs _ (DidChangeWatchedFilesParams fileEvents) -> liftIO $ do
        -- See Note [File existence cache and LSP file watchers] which explains why we get these notifications and
        -- what we do with them
        -- filter out files of interest, since we already know all about those
        -- filter also uris that do not map to filenames, since we cannot handle them
        filesOfInterest <- getFilesOfInterest ide
        let fileEvents' =
                [ (nfp, event) | (FileEvent uri event) <- fileEvents
                , Just fp <- [uriToFilePath uri]
                , let nfp = toNormalizedFilePath fp
                , not $ HM.member nfp filesOfInterest
                ]
        unless (null fileEvents') $ do
            let msg = show fileEvents'
            logDebug (ideLogger ide) $ "Watched file events: " <> Text.pack msg
            modifyFileExists ide fileEvents'
            resetFileStore ide fileEvents'
            setSomethingModified (VFSModified vfs) ide [] msg

  , mkPluginNotificationHandler LSP.SMethod_WorkspaceDidChangeWorkspaceFolders $
      \ide _ _ (DidChangeWorkspaceFoldersParams events) -> liftIO $ do
        let add       = S.union
            substract = flip S.difference
        modifyWorkspaceFolders ide
          $ add       (foldMap (S.singleton . parseWorkspaceFolder) (_added   events))
          . substract (foldMap (S.singleton . parseWorkspaceFolder) (_removed events))

  , mkPluginNotificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $
      \ide vfs _ (DidChangeConfigurationParams cfg) -> liftIO $ do
        let msg = Text.pack $ show cfg
        logDebug (ideLogger ide) $ "Configuration changed: " <> msg
        modifyClientSettings ide (const $ Just cfg)
        setSomethingModified (VFSModified vfs) ide [toKey GetClientSettings emptyFilePath] "config change"

  , mkPluginNotificationHandler LSP.SMethod_Initialized $ \ide _ _ _ -> do
      --------- Initialize Shake session --------------------------------------------------------------------
      liftIO $ shakeSessionInit (cmapWithPrio LogShake recorder) ide

      --------- Set up file watchers ------------------------------------------------------------------------
      opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
        -- See Note [Which files should we watch?] for an explanation of why the pattern is the way that it is
        -- The patterns will be something like "**/.hs", i.e. "any number of directory segments,
        -- followed by a file with an extension 'hs'.
        -- We use multiple watchers instead of one using '{}' because lsp-test doesn't
        -- support that: https://github.com/bubba/lsp-test/issues/77
      let globs = watchedGlobs opts
      success <- registerFileWatches globs
      unless success $
        liftIO $ logDebug (ideLogger ide) "Warning: Client does not support watched files. Falling back to OS polling"
  ],

    -- The ghcide descriptors should come last'ish so that the notification handlers
    -- (which restart the Shake build) run after everything else
        pluginPriority = ghcideNotificationsPluginPriority
    }

ghcideNotificationsPluginPriority :: Natural
ghcideNotificationsPluginPriority = defaultPluginPriority - 900
