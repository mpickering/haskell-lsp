{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.LSP.Core (
    handleMessage
  , LanguageContextData(..)
  , Handler
  , InitializeCallbacks(..)
  , LspFuncs(..)
  , Progress(..)
  , ProgressCancellable(..)
  , ProgressCancelledException
  , SendFunc
  , Handlers(..)
  , Options(..)
  , defaultLanguageContextData
  , makeResponseMessage
  , makeResponseError
  , setupLogger
  , sendErrorResponseS
  , sendErrorLogS
  , sendErrorShowS
  , reverseSortEdit
  , handlerMap
  , getVirtualFile
  , persistVirtualFile
  ) where

import           Control.Concurrent.Classy.Async
import           Control.Concurrent.Classy.STM
import           Control.Concurrent.Classy (MonadConc(..))
import qualified Control.Monad.Catch as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Lens ( (<&>), (^.), (^?), _Just )
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text ( Text )
import           Language.Haskell.LSP.Capture
import           Language.Haskell.LSP.Constant
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types.Capabilities    as C
import qualified Language.Haskell.LSP.Types                 as J
import qualified Language.Haskell.LSP.Types.Lens            as J
import           Language.Haskell.LSP.Utility
import           Language.Haskell.LSP.VFS
import           Language.Haskell.LSP.Diagnostics
import           System.Directory
import           System.Exit
import           System.IO
import qualified System.Log.Formatter as L
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LHS
import           System.Log.Logger
import qualified System.Log.Logger as L
import Debug.Trace

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------

-- | A function to send a message to the client
type SendFunc m = FromServerMessage -> m ()

-- | state used by the LSP dispatcher to manage the message loop
data LanguageContextData stm m config =
  LanguageContextData {
    resSeqDebugContextData :: !Int
  , resHandlers            :: !(Handlers m)
  , resOptions             :: !Options
  , resSendResponse        :: !(SendFunc m)
  , resVFS                 :: !VFS
  , reverseMap             :: !(Map.Map FilePath FilePath)
  , resDiagnostics         :: !DiagnosticStore
  , resConfig              :: !(Maybe config)
  , resLspId               :: !(TVar stm Int)
  , resLspFuncs            :: LspFuncs m config -- NOTE: Cannot be strict, lazy initialization
  , resCaptureFile         :: !(Maybe FilePath)
  , resWorkspaceFolders    :: ![J.WorkspaceFolder]
  , resProgressData        :: !(ProgressData m)
  }

data ProgressData m = ProgressData { progressNextId :: !Int
                                 , progressCancel :: !(Map.Map J.ProgressToken (m ())) }



-- ---------------------------------------------------------------------

-- | Language Server Protocol options that the server may configure.
-- If you set handlers for some requests, you may need to set some of these options.
data Options =
  Options
    { textDocumentSync                 :: Maybe J.TextDocumentSyncOptions
    -- |  The characters that trigger completion automatically.
    , completionTriggerCharacters      :: Maybe [Char]
    -- | The list of all possible characters that commit a completion. This field can be used
    -- if clients don't support individual commmit characters per completion item. See
    -- `_commitCharactersSupport`.
    , completionAllCommitCharacters    :: Maybe [Char]
    -- | The characters that trigger signature help automatically.
    , signatureHelpTriggerCharacters   :: Maybe [Char]
    -- | List of characters that re-trigger signature help.
    -- These trigger characters are only active when signature help is already showing. All trigger characters
    -- are also counted as re-trigger characters.
    , signatureHelpRetriggerCharacters :: Maybe [Char]
    -- | CodeActionKinds that this server may return.
    -- The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
    -- may list out every specific kind they provide.
    , codeActionKinds                  :: Maybe [J.CodeActionKind]
    -- | The list of characters that triggers on type formatting.
    -- If you set `documentOnTypeFormattingHandler`, you **must** set this.
    , documentOnTypeFormattingTriggerCharacters :: Maybe (NonEmpty Char)
    -- | The commands to be executed on the server.
    -- If you set `executeCommandHandler`, you **must** set this.
    , executeCommandCommands           :: Maybe [Text]
    }

instance Default Options where
  def = Options Nothing Nothing Nothing Nothing Nothing
                Nothing Nothing Nothing

-- | A function to publish diagnostics. It aggregates all diagnostics pertaining
-- to a particular version of a document, by source, and sends a
-- 'textDocument/publishDiagnostics' notification with the total (limited by the
-- first parameter) whenever it is updated.
type PublishDiagnosticsFunc m = Int -- Max number of diagnostics to send
                            -> J.NormalizedUri -> J.TextDocumentVersion -> DiagnosticsBySource -> m ()

-- | A function to remove all diagnostics from a particular source, and send the updates to the client.
type FlushDiagnosticsBySourceFunc m = Int -- Max number of diagnostics to send
                                  -> Maybe J.DiagnosticSource -> m ()

-- | A package indicating the perecentage of progress complete and a
-- an optional message to go with it during a 'withProgress'
--
-- @since 0.10.0.0
data Progress = Progress (Maybe Double) (Maybe Text)

-- | Thrown if the user cancels a 'Cancellable' 'withProgress'/'withIndefiniteProgress'/ session
--
-- @since 0.11.0.0
data ProgressCancelledException = ProgressCancelledException
  deriving Show
instance E.Exception ProgressCancelledException

-- | Whether or not the user should be able to cancel a 'withProgress'/'withIndefiniteProgress'
-- session
--
-- @since 0.11.0.0
data ProgressCancellable = Cancellable | NotCancellable

-- | Returned to the server on startup, providing ways to interact with the client.
data LspFuncs m c =
  LspFuncs
    { clientCapabilities           :: !C.ClientCapabilities
    , config                       :: !(m (Maybe c))
      -- ^ Derived from the DidChangeConfigurationNotification message via a
      -- server-provided function.
    , sendFunc                     :: !(SendFunc m)
    , getVirtualFileFunc           :: !(J.NormalizedUri -> m (Maybe VirtualFile))
    , persistVirtualFileFunc       :: !(J.NormalizedUri -> m FilePath)
    , reverseFileMapFunc           :: !(m (FilePath -> FilePath))
    , publishDiagnosticsFunc       :: !(PublishDiagnosticsFunc m)
    , flushDiagnosticsBySourceFunc :: !(FlushDiagnosticsBySourceFunc m)
    , getNextReqId                 :: !(m J.LspId)
    , rootPath                     :: !(Maybe FilePath)
    , getWorkspaceFolders          :: !(m (Maybe [J.WorkspaceFolder]))
    , withProgress                 :: !(forall a . Text -> ProgressCancellable
                                        -> ((Progress -> m ()) -> m a) -> m a)
      -- ^ Wrapper for reporting progress to the client during a long running
      -- task.
      -- 'withProgress' @title cancellable f@ starts a new progress reporting
      -- session, and finishes it once f is completed.
      -- f is provided with an update function that allows it to report on
      -- the progress during the session.
      -- If @cancellable@ is 'Cancellable', @f@ will be thrown a
      -- 'ProgressCancelledException' if the user cancels the action in
      -- progress.
      --
      -- @since 0.10.0.0
    , withIndefiniteProgress       :: !(forall a . Text -> ProgressCancellable
                                        -> m a -> m a)
    -- ^ Same as 'withProgress', but for processes that do not report the
    -- precentage complete.
    --
    -- @since 0.10.0.0
    }

-- | Contains all the callbacks to use for initialized the language server.
-- it is parameterized over a config type variable representing the type for the
-- specific configuration data the language server needs to use.
data InitializeCallbacks m config =
  InitializeCallbacks
    { onInitialConfiguration :: J.InitializeRequest -> Either T.Text config
      -- ^ Invoked on the first message from the language client, containg the client configuration
      -- This callback should return either the parsed configuration data or an error indicating
      -- what went wrong. The parsed configuration object will be stored internally and passed to
      -- hanlder functions as context.
    , onConfigurationChange :: J.DidChangeConfigurationNotification-> Either T.Text config
      -- ^ Invoked whenever the clients sends a message with a changed client configuration.
      -- This callback should return either the parsed configuration data or an error indicating
      -- what went wrong. The parsed configuration object will be stored internally and passed to
      -- hanlder functions as context.
    , onStartup :: LspFuncs m config -> m (Maybe J.ResponseError)
      -- ^ Once the initial configuration has been received, this callback will be invoked to offer
      -- the language server implementation the chance to create any processes or start new threads
      -- that may be necesary for the server lifecycle.
    }

-- | The Handler type captures a function that receives local read-only state
-- 'a', a function to send a reply message once encoded as a ByteString, and a
-- received message of type 'b'
type Handler m b =  b -> m ()

-- | Callbacks from the language server to the language handler
data Handlers m =
  Handlers
    {
    -- Capability-advertised handlers
      hoverHandler                   :: !(Maybe (Handler m J.HoverRequest))
    , completionHandler              :: !(Maybe (Handler m J.CompletionRequest))
    , completionResolveHandler       :: !(Maybe (Handler m J.CompletionItemResolveRequest))
    , signatureHelpHandler           :: !(Maybe (Handler m J.SignatureHelpRequest))
    , definitionHandler              :: !(Maybe (Handler m J.DefinitionRequest))
    , typeDefinitionHandler          :: !(Maybe (Handler m J.TypeDefinitionRequest))
    , implementationHandler          :: !(Maybe (Handler m J.ImplementationRequest))
    , referencesHandler              :: !(Maybe (Handler m J.ReferencesRequest))
    , documentHighlightHandler       :: !(Maybe (Handler m J.DocumentHighlightRequest))
    , documentSymbolHandler          :: !(Maybe (Handler m J.DocumentSymbolRequest))
    , workspaceSymbolHandler         :: !(Maybe (Handler m J.WorkspaceSymbolRequest))
    , codeActionHandler              :: !(Maybe (Handler m J.CodeActionRequest))
    , codeLensHandler                :: !(Maybe (Handler m J.CodeLensRequest))
    , codeLensResolveHandler         :: !(Maybe (Handler m J.CodeLensResolveRequest))
    , documentColorHandler           :: !(Maybe (Handler m J.DocumentColorRequest))
    , colorPresentationHandler       :: !(Maybe (Handler m J.ColorPresentationRequest))
    , documentFormattingHandler      :: !(Maybe (Handler m J.DocumentFormattingRequest))
    , documentRangeFormattingHandler :: !(Maybe (Handler m J.DocumentRangeFormattingRequest))
    , documentOnTypeFormattingHandler :: !(Maybe (Handler m J.DocumentOnTypeFormattingRequest))
    , renameHandler                  :: !(Maybe (Handler m J.RenameRequest))
    , prepareRenameHandler           :: !(Maybe (Handler m J.PrepareRenameRequest))
    , foldingRangeHandler            :: !(Maybe (Handler m J.FoldingRangeRequest))
    -- new in 3.0
    , documentLinkHandler            :: !(Maybe (Handler m J.DocumentLinkRequest))
    , documentLinkResolveHandler     :: !(Maybe (Handler m J.DocumentLinkResolveRequest))
    , executeCommandHandler          :: !(Maybe (Handler m J.ExecuteCommandRequest))
    -- Next 2 go from server -> client
    -- , registerCapabilityHandler      :: !(Maybe (Handler m J.RegisterCapabilityRequest))
    -- , unregisterCapabilityHandler    :: !(Maybe (Handler m J.UnregisterCapabilityRequest))
    , willSaveWaitUntilTextDocHandler:: !(Maybe (Handler m J.WillSaveWaitUntilTextDocumentRequest))

    -- Notifications from the client
    , didChangeConfigurationParamsHandler      :: !(Maybe (Handler m J.DidChangeConfigurationNotification))
    , didOpenTextDocumentNotificationHandler   :: !(Maybe (Handler m J.DidOpenTextDocumentNotification))
    , didChangeTextDocumentNotificationHandler :: !(Maybe (Handler m J.DidChangeTextDocumentNotification))
    -- ^ Note: If you need to keep track of document changes,
    -- "Language.Haskell.LSP.VFS" will take care of these messages for you!
    , didCloseTextDocumentNotificationHandler  :: !(Maybe (Handler m J.DidCloseTextDocumentNotification))
    , didSaveTextDocumentNotificationHandler   :: !(Maybe (Handler m J.DidSaveTextDocumentNotification))
    , didChangeWatchedFilesNotificationHandler :: !(Maybe (Handler m J.DidChangeWatchedFilesNotification))
    , didChangeWorkspaceFoldersNotificationHandler :: !(Maybe (Handler m J.DidChangeWorkspaceFoldersNotification))
    -- new in 3.0
    , initializedHandler                       :: !(Maybe (Handler m J.InitializedNotification))
    , willSaveTextDocumentNotificationHandler  :: !(Maybe (Handler m J.WillSaveTextDocumentNotification))
    , cancelNotificationHandler                :: !(Maybe (Handler m J.CancelNotification))

    -- Responses to Request messages originated from the server
    -- TODO: Properly decode response types and replace them with actual handlers
    , responseHandler                    :: !(Maybe (Handler m J.BareResponseMessage))
    -- , registerCapabilityHandler                :: !(Maybe (Handler m J.RegisterCapabilityResponse))
    -- , unregisterCapabilityHandler              :: !(Maybe (Handler m J.RegisterCapabilityResponse))
    -- , showMessageHandler                       :: !(Maybe (Handler m J.ShowMessageResponse))

    -- Initialization request on startup
    , initializeRequestHandler                 :: !(Maybe (Handler m J.InitializeRequest))
    -- Will default to terminating `exitMessage` if Nothing
    , exitNotificationHandler                  :: !(Maybe (Handler m J.ExitNotification))

    , customRequestHandler                     :: !(Maybe (Handler m J.CustomClientRequest))
    , customNotificationHandler                :: !(Maybe (Handler m J.CustomClientNotification))

    }

instance MonadConc m => Default (Handlers m) where
  -- These already implicitly do stuff to the VFS, so silence warnings about no handler
  def = nothings { didChangeTextDocumentNotificationHandler = Just ignore
                 , didOpenTextDocumentNotificationHandler   = Just ignore
                 , didCloseTextDocumentNotificationHandler  = Just ignore
                 }
    where ignore = const (pure ())
          nothings = Handlers Nothing Nothing Nothing Nothing Nothing Nothing
                              Nothing Nothing Nothing Nothing Nothing Nothing
                              Nothing Nothing Nothing Nothing Nothing Nothing
                              Nothing Nothing Nothing Nothing Nothing Nothing
                              Nothing Nothing Nothing Nothing Nothing Nothing
                              Nothing Nothing Nothing Nothing Nothing Nothing
                              Nothing Nothing Nothing Nothing Nothing

-- ---------------------------------------------------------------------
nop :: MonadConc m => a -> b -> m a
nop = const . return


helper :: (MonadConc m,  J.FromJSON a) => (TVar (STM m) (LanguageContextData (STM m) m config) -> a -> m ()) -> (TVar (STM m) (LanguageContextData (STM m) m config) -> J.Value -> m ())
helper requestHandler tvarDat json =
  case J.fromJSON json of
    J.Success req -> requestHandler tvarDat req
    J.Error err -> do
      let msg = T.pack . unwords $ ["haskell-lsp:parse error.", show json, show err] ++ _ERR_MSG_URL
          failLog = sendErrorLog tvarDat msg
      case json of
        (J.Object o) -> case HM.lookup "id" o of
          Just olid -> case J.fromJSON olid of
            J.Success lid -> sendErrorResponse tvarDat lid msg
            _ -> failLog
          _ -> failLog
        _ -> failLog

handlerMap :: (Show config, MonadConc m, MonadIO m) => InitializeCallbacks m config
           -> Handlers m -> J.ClientMethod -> (TVar (STM m) (LanguageContextData (STM m) m config) -> J.Value -> m ())
-- General
handlerMap i h J.Initialize                      = handleInitialConfig i (initializeRequestHandler h)
handlerMap _ h J.Initialized                     = hh nop NotInitialized $ initializedHandler h
handlerMap _ _ J.Shutdown                        = helper shutdownRequestHandler
handlerMap _ h J.Exit                            =
  case exitNotificationHandler h of
    Just _ -> hh nop NotExit $ exitNotificationHandler h
    Nothing -> \ctxVar v -> do
      ctx <- readTVarConc ctxVar
      -- Capture exit notification
      case J.fromJSON v :: J.Result J.ExitNotification of
        J.Success n -> liftIO $ captureFromClient (NotExit n) (resCaptureFile ctx)
        J.Error _ -> return ()
      liftIO $ logm $ B.pack "haskell-lsp:Got exit, exiting"
      liftIO $ exitSuccess
handlerMap _ h J.CancelRequest                   = hh nop NotCancelRequestFromClient $ cancelNotificationHandler h
-- Workspace
handlerMap _ h J.WorkspaceDidChangeWorkspaceFolders = hwf $ didChangeWorkspaceFoldersNotificationHandler h
handlerMap i h J.WorkspaceDidChangeConfiguration = hc i $ didChangeConfigurationParamsHandler h
handlerMap _ h J.WorkspaceDidChangeWatchedFiles  = hh nop NotDidChangeWatchedFiles $ didChangeWatchedFilesNotificationHandler h
handlerMap _ h J.WorkspaceSymbol                 = hh nop ReqWorkspaceSymbols $ workspaceSymbolHandler h
handlerMap _ h J.WorkspaceExecuteCommand         = hh nop ReqExecuteCommand $ executeCommandHandler h
-- Document
handlerMap _ h J.TextDocumentDidOpen             = hh openVFS NotDidOpenTextDocument $ didOpenTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentDidChange           = hh changeFromClientVFS NotDidChangeTextDocument $ didChangeTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentWillSave            = hh nop NotWillSaveTextDocument $ willSaveTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentWillSaveWaitUntil   = hh nop ReqWillSaveWaitUntil $ willSaveWaitUntilTextDocHandler h
handlerMap _ h J.TextDocumentDidSave             = hh nop NotDidSaveTextDocument $ didSaveTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentDidClose            = hh closeVFS NotDidCloseTextDocument $ didCloseTextDocumentNotificationHandler h
handlerMap _ h J.TextDocumentCompletion          = hh nop ReqCompletion $ completionHandler h
handlerMap _ h J.CompletionItemResolve           = hh nop ReqCompletionItemResolve $ completionResolveHandler h
handlerMap _ h J.TextDocumentHover               = hh nop ReqHover $ hoverHandler h
handlerMap _ h J.TextDocumentSignatureHelp       = hh nop ReqSignatureHelp $ signatureHelpHandler h
handlerMap _ h J.TextDocumentDefinition          = hh nop ReqDefinition $ definitionHandler h
handlerMap _ h J.TextDocumentTypeDefinition      = hh nop ReqTypeDefinition $ typeDefinitionHandler h
handlerMap _ h J.TextDocumentImplementation      = hh nop ReqImplementation $ implementationHandler h
handlerMap _ h J.TextDocumentReferences          = hh nop ReqFindReferences $ referencesHandler h
handlerMap _ h J.TextDocumentDocumentHighlight   = hh nop ReqDocumentHighlights $ documentHighlightHandler h
handlerMap _ h J.TextDocumentDocumentSymbol      = hh nop ReqDocumentSymbols $ documentSymbolHandler h
handlerMap _ h J.TextDocumentFormatting          = hh nop ReqDocumentFormatting $ documentFormattingHandler h
handlerMap _ h J.TextDocumentRangeFormatting     = hh nop ReqDocumentRangeFormatting $ documentRangeFormattingHandler h
handlerMap _ h J.TextDocumentOnTypeFormatting    = hh nop ReqDocumentOnTypeFormatting $ documentOnTypeFormattingHandler h
handlerMap _ h J.TextDocumentCodeAction          = hh nop ReqCodeAction $ codeActionHandler h
handlerMap _ h J.TextDocumentCodeLens            = hh nop ReqCodeLens $ codeLensHandler h
handlerMap _ h J.CodeLensResolve                 = hh nop ReqCodeLensResolve $ codeLensResolveHandler h
handlerMap _ h J.TextDocumentDocumentColor       = hh nop ReqDocumentColor $ documentColorHandler h
handlerMap _ h J.TextDocumentColorPresentation   = hh nop ReqColorPresentation $ colorPresentationHandler h
handlerMap _ h J.TextDocumentDocumentLink        = hh nop ReqDocumentLink $ documentLinkHandler h
handlerMap _ h J.DocumentLinkResolve             = hh nop ReqDocumentLinkResolve $ documentLinkResolveHandler h
handlerMap _ h J.TextDocumentRename              = hh nop ReqRename $ renameHandler h
handlerMap _ h J.TextDocumentPrepareRename       = hh nop ReqPrepareRename $ prepareRenameHandler h
handlerMap _ h J.TextDocumentFoldingRange        = hh nop ReqFoldingRange $ foldingRangeHandler h
handlerMap _ _ J.WorkDoneProgressCancel          = helper progressCancelHandler
handlerMap _ h (J.CustomClientMethod _)          = \ctxData val ->
    case val of
        J.Object o | "id" `HM.member` o ->
            -- Custom request
            hh nop ReqCustomClient (customRequestHandler h) ctxData val
        _ -> -- Custom notification
            hh nop NotCustomClient (customNotificationHandler h) ctxData val

-- ---------------------------------------------------------------------

-- | Adapter from the normal handlers exposed to the library users and the
-- internal message loop
hh :: forall b config m . (J.FromJSON b, MonadConc m, MonadIO m)
   => (VFS -> b -> m VFS) -> (b -> FromClientMessage) -> Maybe (Handler m b) -> TVar (STM m) (LanguageContextData (STM m) m config) -> J.Value -> m ()
hh getVfs wrapper mh tvarDat json = do
      case J.fromJSON json of
        J.Success req -> do
          ctx <- readTVarConc tvarDat
          vfs' <- getVfs (resVFS ctx) req
          atomically $ modifyTVar' tvarDat (\c -> c {resVFS = vfs'})

          liftIO $ captureFromClient (wrapper req) (resCaptureFile ctx)

          case mh of
            Just h -> h req
            Nothing -> do
              let msg = T.pack $ unwords ["haskell-lsp:no handler for.", show json]
              sendErrorLog tvarDat msg
        J.Error  err -> do
          let msg = T.pack $ unwords $ ["haskell-lsp:parse error.", show json, show err] ++ _ERR_MSG_URL
          sendErrorLog tvarDat msg

handleInitialConfig
  :: (Show config, MonadConc m, MonadIO m)
  => InitializeCallbacks m config
  -> Maybe (Handler m J.InitializeRequest)
  -> TVar (STM m) (LanguageContextData (STM m) m config)
  -> J.Value
  -> m ()
handleInitialConfig (InitializeCallbacks { onInitialConfiguration, onStartup }) mh tvarDat json
  = handleMessageWithConfigChange ReqInitialize
                                  onInitialConfiguration
                                  (Just $ initializeRequestHandler' onStartup mh tvarDat)
                                  tvarDat
                                  json


hc
  :: (Show config, MonadConc m, MonadIO m)
  => InitializeCallbacks m config
  -> Maybe (Handler m J.DidChangeConfigurationNotification)
  -> TVar (STM m) (LanguageContextData (STM m) m config)
  -> J.Value
  -> m ()
hc (InitializeCallbacks { onConfigurationChange }) mh tvarDat json =
  handleMessageWithConfigChange NotDidChangeConfiguration
                                onConfigurationChange
                                mh
                                tvarDat
                                json

handleMessageWithConfigChange
  :: (J.FromJSON reqParams, Show reqParams, Show err, MonadConc m, MonadIO m)
  => (reqParams -> FromClientMessage) -- ^ The notification message from the client to expect
  -> (reqParams -> Either err config) -- ^ A function to parse the config out of the request
  -> Maybe (reqParams -> m ()) -- ^ The upstream handler for the client request
  -> TVar (STM m) (LanguageContextData (STM m) m config) -- ^ The context data containing the current configuration
  -> J.Value -- ^ The raw reqeust data
  -> m ()
handleMessageWithConfigChange notification parseConfig mh tvarDat json =
  -- logs $ "haskell-lsp:hc DidChangeConfigurationNotification entered"
  case J.fromJSON json of
    J.Success req -> do
      ctx <- readTVarConc tvarDat

      liftIO $ captureFromClient (notification req) (resCaptureFile ctx)

      case parseConfig req of
        Left err -> do
          let
            msg =
              T.pack $ unwords
                ["haskell-lsp:configuration parse error.", show req, show err]
          sendErrorLog tvarDat msg
        Right newConfig -> do
          -- logs $ "haskell-lsp:hc DidChangeConfigurationNotification got newConfig:" ++ show newConfig
          let ctx' = ctx { resConfig = Just newConfig }
          atomically $ modifyTVar' tvarDat (const ctx')
      case mh of
        Just h  -> h req
        Nothing -> return ()
    J.Error err -> do
      let msg =
            T.pack
              $  unwords
              $  ["haskell-lsp:parse error.", show json, show err]
              ++ _ERR_MSG_URL
      sendErrorLog tvarDat msg

-- | Updates the list of workspace folders and then delegates back to 'hh'
hwf :: (MonadConc m, MonadIO m) => Maybe (Handler m J.DidChangeWorkspaceFoldersNotification) -> TVar (STM m) (LanguageContextData (STM m) m config) -> J.Value -> m ()
hwf h tvarDat json = do
  case J.fromJSON json :: J.Result J.DidChangeWorkspaceFoldersNotification of
    J.Success (J.NotificationMessage _ _ params) -> atomically $ do

      oldWfs <- resWorkspaceFolders <$> readTVar tvarDat
      let J.List toRemove = params ^. J.event . J.removed
          wfs0 = foldr L.delete oldWfs toRemove
          J.List toAdd = params ^. J.event . J.added
          wfs1 = wfs0 <> toAdd

      modifyTVar' tvarDat (\c -> c {resWorkspaceFolders = wfs1})
    _ -> return ()
  hh nop NotDidChangeWorkspaceFolders h tvarDat json

-- ---------------------------------------------------------------------

getVirtualFile :: (MonadConc m) => TVar (STM m) (LanguageContextData (STM m) m config) -> J.NormalizedUri -> m (Maybe VirtualFile)
getVirtualFile tvarDat uri = Map.lookup uri . resVFS <$> readTVarConc tvarDat

-- | Dump the current text for a given VFS file to a temporary file,
-- and return the path to the file.
persistVirtualFile :: (MonadConc m, MonadIO m) => TVar (STM m) (LanguageContextData (STM m) m config) -> J.NormalizedUri -> m FilePath
persistVirtualFile tvarDat uri = do
  st <- readTVarConc tvarDat
  let vfs = resVFS st
      revMap = reverseMap st

  (fn, new_vfs) <- liftIO $ persistFileVFS vfs uri
  let revMap' =
        -- TODO: Does the VFS make sense for URIs which are not files?
        -- The reverse map should perhaps be (FilePath -> URI)
        case J.uriToFilePath (J.fromNormalizedUri uri) of
          Just uri_fp -> Map.insert fn uri_fp revMap
          Nothing -> revMap

  atomically $ modifyTVar' tvarDat (\d -> d { resVFS = new_vfs
                                            , reverseMap = revMap' })
  return fn

-- TODO: should this function return a URI?
-- | If the contents of a VFS has been dumped to a temporary file, map
-- the temporary file name back to the original one.
reverseFileMap :: (MonadConc m)
               => TVar (STM m) (LanguageContextData (STM m) m config)
               -> m (FilePath -> FilePath)
reverseFileMap tvarDat = do
  revMap <- reverseMap <$> readTVarConc tvarDat
  let f fp = fromMaybe fp $ Map.lookup fp revMap
  return f

-- ---------------------------------------------------------------------

getConfig :: (MonadConc m, MonadIO m) => TVar (STM m) (LanguageContextData (STM m) m config) -> m (Maybe config)
getConfig tvar = resConfig <$> readTVarConc tvar

-- ---------------------------------------------------------------------
-- |
--
--
_INITIAL_RESPONSE_SEQUENCE :: Int
_INITIAL_RESPONSE_SEQUENCE = 0


-- |
--
--
_SEP_WIN :: Char
_SEP_WIN = '\\'

-- |
--
--
_SEP_UNIX :: Char
_SEP_UNIX = '/'

-- |
--
--
_ERR_MSG_URL :: [String]
_ERR_MSG_URL = [ "`stack update` and install new haskell-lsp."
               , "Or check information on https://marketplace.visualstudio.com/items?itemName=xxxxxxxxxxxxxxx"
               ]


-- |
--
--
defaultLanguageContextData :: (MonadSTM stm, MonadConc m) => Handlers m -> Options -> LspFuncs m config -> TVar stm Int -> SendFunc m -> Maybe FilePath -> LanguageContextData stm m config
defaultLanguageContextData h o lf tv sf cf =
  LanguageContextData _INITIAL_RESPONSE_SEQUENCE h o sf mempty mempty mempty
                      Nothing tv lf cf mempty defaultProgressData

defaultProgressData :: ProgressData m
defaultProgressData = ProgressData 0 Map.empty

-- ---------------------------------------------------------------------

handleMessage :: (Show config, MonadConc m, MonadIO m) => InitializeCallbacks m config
              -> TVar (STM m) (LanguageContextData (STM m) m config) -> BSL.ByteString -> m ()
handleMessage dispatcherProc tvarDat jsonStr = do
  {-
  Message Types we must handle are the following

  Request      | jsonrpc | id | method | params?
  Response     | jsonrpc | id |        |         | response? | error?
  Notification | jsonrpc |    | method | params?

  -}

  case J.eitherDecode jsonStr :: Either String J.Object of
    Left  err -> do
      let msg =  T.pack $ unwords [ "haskell-lsp:incoming message parse error.", lbs2str jsonStr, show err]
              ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL)
              ++ "\n"
      sendErrorLog tvarDat msg

    Right o -> do

      case HM.lookup "method" o of
        Just cmd@(J.String s) -> case J.fromJSON cmd of
                                   J.Success m -> handle (J.Object o) m
                                   J.Error _ -> do
                                     let msg = T.pack $ unwords ["haskell-lsp:unknown message received:method='"
                                                                 ++ T.unpack s ++ "',", lbs2str jsonStr]
                                     sendErrorLog tvarDat msg
        Just oops -> liftIO $ logs $ "haskell-lsp:got strange method param, ignoring:" ++ show oops
        Nothing -> do
          liftIO $ logs $ "haskell-lsp:Got reply message:" ++ show jsonStr
          handleResponse (J.Object o)

  where
    handleResponse json = do
      ctx <- readTVarConc tvarDat
      case responseHandler $ resHandlers ctx of
        Nothing -> sendErrorLog tvarDat $ T.pack $ "haskell-lsp: responseHandler is not defined, ignoring response " ++ lbs2str jsonStr
        Just h -> case J.fromJSON json of
          J.Success res -> h res
          J.Error err -> let msg = T.pack $ unwords $ ["haskell-lsp:response parse error.", lbs2str jsonStr, show err] ++ _ERR_MSG_URL
                           in sendErrorLog tvarDat msg
    -- capability based handlers
    handle json cmd = do
      ctx <- readTVarConc tvarDat
      let h = resHandlers ctx
      handlerMap dispatcherProc h cmd tvarDat json

-- ---------------------------------------------------------------------

makeResponseMessage :: J.RequestMessage J.ClientMethod req resp -> resp -> J.ResponseMessage resp
makeResponseMessage req result = J.ResponseMessage "2.0" (J.responseId $ req ^. J.id) (Just result) Nothing

makeResponseError :: J.LspIdRsp -> J.ResponseError -> J.ResponseMessage ()
makeResponseError origId err = J.ResponseMessage "2.0" origId Nothing (Just err)

-- ---------------------------------------------------------------------
-- |
--
sendEvent :: (MonadConc m) => TVar (STM m) (LanguageContextData (STM m) m config) -> FromServerMessage -> m ()
sendEvent tvarCtx msg = sendResponse tvarCtx msg

-- |
--
sendResponse :: (MonadConc m) => TVar (STM m) (LanguageContextData (STM m) m config) -> FromServerMessage -> m ()
sendResponse tvarCtx msg = do
  ctx <- readTVarConc tvarCtx
  resSendResponse ctx msg


-- ---------------------------------------------------------------------
-- |
--
--
sendErrorResponse :: (MonadConc m) => TVar (STM m) (LanguageContextData (STM m) m config) -> J.LspIdRsp -> Text -> m ()
sendErrorResponse tv origId msg = sendErrorResponseS (sendEvent tv) origId J.InternalError msg

sendErrorResponseS ::  (MonadConc m) => SendFunc m -> J.LspIdRsp -> J.ErrorCode -> Text -> m ()
sendErrorResponseS sf origId err msg = do
  sf $ RspError (J.ResponseMessage "2.0" origId Nothing
                  (Just $ J.ResponseError err msg Nothing) :: J.ErrorResponse)

sendErrorLog :: (MonadConc m) => TVar (STM m) (LanguageContextData (STM m) m config) -> Text -> m ()
sendErrorLog tv msg = sendErrorLogS (sendEvent tv) msg

sendErrorLogS :: MonadConc m => SendFunc m -> Text -> m ()
sendErrorLogS sf msg =
  sf $ NotLogMessage $ fmServerLogMessageNotification J.MtError msg

-- sendErrorShow :: String -> IO ()
-- sendErrorShow msg = sendErrorShowS sendEvent msg

sendErrorShowS :: SendFunc m -> Text -> m ()
sendErrorShowS sf msg =
  sf $ NotShowMessage $ fmServerShowMessageNotification J.MtError msg

-- ---------------------------------------------------------------------

defaultErrorHandlers :: (Show a, MonadConc m) => TVar (STM m) (LanguageContextData (STM m) m config) -> J.LspIdRsp -> a -> [E.Handler m ()]
defaultErrorHandlers tvarDat origId req = [ E.Handler someExcept ]
  where
    someExcept (e :: E.SomeException) = do
      let msg = T.pack $ unwords ["request error.", show req, show e]
      sendErrorResponse tvarDat origId msg
      sendErrorLog tvarDat msg


-- |=====================================================================
--
-- Handlers

-- |
--
initializeRequestHandler'
  :: forall config m . (Show config, MonadConc m, MonadIO m)
  => (LspFuncs m config -> m (Maybe J.ResponseError))
  -> Maybe (Handler m J.InitializeRequest)
  -> TVar (STM  m) (LanguageContextData (STM m) m config)
  -> J.InitializeRequest
  -> m ()
initializeRequestHandler' onStartup mHandler tvarCtx req@(J.RequestMessage _ origId _ params) =
  flip E.catches (defaultErrorHandlers tvarCtx (J.responseId origId) req) $ do

    case mHandler of
      Just handler -> handler req
      Nothing -> return ()

    let wfs = case params ^. J.workspaceFolders of
                Just (J.List xs) -> xs
                Nothing -> []

    atomically $ modifyTVar' tvarCtx (\c -> c { resWorkspaceFolders = wfs })

    ctx0 <- readTVarConc tvarCtx
    let rootDir = getFirst $ foldMap First [ params ^. J.rootUri  >>= J.uriToFilePath
                                           , params ^. J.rootPath <&> T.unpack ]

    case rootDir of
      Nothing -> return ()
      Just dir -> do
        liftIO $ logs $ "haskell-lsp:initializeRequestHandler: setting current dir to project root:" ++ dir
        liftIO $ unless (null dir) $ setCurrentDirectory dir

    let
      getCapabilities :: J.InitializeParams -> C.ClientCapabilities
      getCapabilities (J.InitializeParams _ _ _ _ c _ _) = c
      getLspId tvId = atomically $ do
        cid <- readTVar tvId
        modifyTVar' tvId (+1)
        return $ J.IdInt cid

      clientSupportsWfs = fromMaybe False $ do
        let (C.ClientCapabilities mw _ _ _) = params ^. J.capabilities
        (C.WorkspaceClientCapabilities _ _ _ _ _ _ mwf _) <- mw
        mwf
      getWfs tvc
        | clientSupportsWfs = atomically $ Just . resWorkspaceFolders <$> readTVar tvc
        | otherwise = return Nothing

      clientSupportsProgress = fromMaybe False $ do
        let (C.ClientCapabilities _ _ wc _) = params ^. J.capabilities
        (C.WindowClientCapabilities mProgress) <- wc
        mProgress

      storeProgress :: J.ProgressToken -> Async m a -> m ()
      storeProgress n a = atomically $ do
        pd <- resProgressData <$> readTVar tvarCtx
        let pc = progressCancel pd
            pc' = Map.insert n (cancelWith a ProgressCancelledException) pc
        modifyTVar tvarCtx (\ctx -> ctx { resProgressData = pd { progressCancel = pc' }})

      deleteProgress :: MonadConc m => J.ProgressToken -> m ()
      deleteProgress n = atomically $ do
        pd <- resProgressData <$> readTVar tvarCtx
        let x = progressCancel pd
            x' = Map.delete n x
        modifyTVar tvarCtx (\ctx -> ctx { resProgressData = pd { progressCancel = x' }})

      -- Get a new id for the progress session and make a new one
      getNewProgressId :: MonadConc m => m J.ProgressToken
      getNewProgressId = atomically $ do
        pd <- resProgressData <$> readTVar tvarCtx
        let x = progressNextId pd
        modifyTVar tvarCtx (\ctx -> ctx { resProgressData = pd { progressNextId = x + 1 }})
        return $ J.ProgressNumericToken x

      withProgressBase :: MonadConc m => Bool -> (Text -> ProgressCancellable
                    -> ((Progress -> m ()) -> m a) -> m a)
      withProgressBase indefinite title cancellable f
        | clientSupportsProgress = do
          sf <- resSendResponse <$> readTVarConc tvarCtx

          progId <- getNewProgressId

          let initialPercentage
                | indefinite = Nothing
                | otherwise = Just 0
              cancellable' = case cancellable of
                              Cancellable -> True
                              NotCancellable -> False

          rId <- getLspId $ resLspId ctx0

          -- Create progress token
          sf $ ReqWorkDoneProgressCreate $
            fmServerWorkDoneProgressCreateRequest rId $ J.WorkDoneProgressCreateParams progId

          -- Send initial notification
          sf $ NotWorkDoneProgressBegin $ fmServerWorkDoneProgressBeginNotification $
            J.ProgressParams progId $
            J.WorkDoneProgressBeginParams title (Just cancellable') Nothing initialPercentage

          aid <- async $ f (updater progId sf)
          storeProgress progId aid
          res <- wait aid

          -- Send done notification
          sf $ NotWorkDoneProgressEnd $ fmServerWorkDoneProgressEndNotification $
            J.ProgressParams progId $
            J.WorkDoneProgressEndParams Nothing
          -- Delete the progress cancellation from the map
          -- If we don't do this then it's easy to leak things as the map contains any IO action.
          deleteProgress progId


          return res
        | otherwise = f (const $ return ())
          where updater progId sf (Progress percentage msg) =
                  sf $ NotWorkDoneProgressReport $ fmServerWorkDoneProgressReportNotification $
                    J.ProgressParams progId $
                    J.WorkDoneProgressReportParams Nothing msg percentage

      withProgress' :: MonadConc m => Text -> ProgressCancellable -> ((Progress -> m ()) -> m a) -> m a
      withProgress' = withProgressBase False

      withIndefiniteProgress' :: MonadConc m => Text -> ProgressCancellable -> m a -> m a
      withIndefiniteProgress' title cancellable f =
        withProgressBase True title cancellable (const f)

    -- Launch the given process once the project root directory has been set
    let lspFuncs = LspFuncs (getCapabilities params)
                            (getConfig tvarCtx)
                            (resSendResponse ctx0)
                            (getVirtualFile tvarCtx)
                            (persistVirtualFile tvarCtx)
                            (reverseFileMap tvarCtx)
                            (publishDiagnostics tvarCtx)
                            (flushDiagnosticsBySource tvarCtx)
                            (getLspId $ resLspId ctx0)
                            rootDir
                            (getWfs tvarCtx)
                            withProgress'
                            withIndefiniteProgress'
    let ctx = ctx0 { resLspFuncs = lspFuncs }
    atomically $ writeTVar tvarCtx ctx

    initializationResult <- onStartup lspFuncs

    case initializationResult of
      Just errResp -> do
        sendResponse tvarCtx $ RspError $ makeResponseError (J.responseId origId) errResp

      Nothing -> do
        let capa = serverCapabilities (getCapabilities params) (resOptions ctx) (resHandlers ctx)
            -- TODO: wrap this up into a fn to create a response message
            res  = J.ResponseMessage "2.0" (J.responseId origId) (Just $ J.InitializeResponseCapabilities capa) Nothing

        sendResponse tvarCtx $ RspInitialize res

-- | Infers the capabilities based on registered handlers, and sets the appropriate options.
-- A provider should be set to Nothing if the server does not support it, unless it is a
-- static option.
serverCapabilities :: C.ClientCapabilities -> Options -> Handlers m -> J.InitializeResponseCapabilitiesInner
serverCapabilities clientCaps o h =
  J.InitializeResponseCapabilitiesInner
    { J._textDocumentSync                 = sync
    , J._hoverProvider                    = supported (hoverHandler h)
    , J._completionProvider               = completionProvider
    , J._signatureHelpProvider            = signatureHelpProvider
    , J._definitionProvider               = supported (definitionHandler h)
    , J._typeDefinitionProvider           = Just $ J.GotoOptionsStatic $ isJust $ typeDefinitionHandler h
    , J._implementationProvider           = Just $ J.GotoOptionsStatic $ isJust $ typeDefinitionHandler h
    , J._referencesProvider               = supported (referencesHandler h)
    , J._documentHighlightProvider        = supported (documentHighlightHandler h)
    , J._documentSymbolProvider           = supported (documentSymbolHandler h)
    , J._workspaceSymbolProvider          = supported (workspaceSymbolHandler h)
    , J._codeActionProvider               = codeActionProvider
    , J._codeLensProvider                 = supported' (codeLensHandler h) $ J.CodeLensOptions $
                                              supported (codeLensResolveHandler h)
    , J._documentFormattingProvider       = supported (documentFormattingHandler h)
    , J._documentRangeFormattingProvider  = supported (documentRangeFormattingHandler h)
    , J._documentOnTypeFormattingProvider = documentOnTypeFormattingProvider
    , J._renameProvider                   = Just $ J.RenameOptionsStatic $ isJust $ renameHandler h
    , J._documentLinkProvider             = supported' (documentLinkHandler h) $ J.DocumentLinkOptions $
                                              Just $ isJust $ documentLinkResolveHandler h
    , J._colorProvider                    = Just $ J.ColorOptionsStatic $ isJust $ documentColorHandler h
    , J._foldingRangeProvider             = Just $ J.FoldingRangeOptionsStatic $ isJust $ foldingRangeHandler h
    , J._executeCommandProvider           = executeCommandProvider
    , J._workspace                        = Just workspace
    -- TODO: Add something for experimental
    , J._experimental                     = Nothing :: Maybe J.Value
    }
  where
    supported x = supported' x True

    supported' (Just _) = Just
    supported' Nothing = const Nothing

    singleton :: a -> [a]
    singleton x = [x]

    completionProvider
      | isJust $ completionHandler h = Just $
          J.CompletionOptions
            (Just $ isJust $ completionResolveHandler h)
            (map singleton <$> completionTriggerCharacters o)
            (map singleton <$> completionAllCommitCharacters o)
      | otherwise = Nothing

    clientSupportsCodeActionKinds = isJust $
      clientCaps ^? J.textDocument . _Just . J.codeAction . _Just . J.codeActionLiteralSupport

    codeActionProvider
      | clientSupportsCodeActionKinds
      , isJust $ codeActionHandler h = Just $ maybe (J.CodeActionOptionsStatic True) (J.CodeActionOptions . Just) (codeActionKinds o)
      | isJust $ codeActionHandler h = Just (J.CodeActionOptionsStatic True)
      | otherwise = Just (J.CodeActionOptionsStatic False)

    signatureHelpProvider
      | isJust $ signatureHelpHandler h = Just $
          J.SignatureHelpOptions
            (map singleton <$> signatureHelpTriggerCharacters o)
            (map singleton <$> signatureHelpRetriggerCharacters o)
      | otherwise = Nothing

    documentOnTypeFormattingProvider
      | isJust $ documentOnTypeFormattingHandler h
      , Just (first :| rest) <- documentOnTypeFormattingTriggerCharacters o = Just $
          J.DocumentOnTypeFormattingOptions (T.pack [first]) (Just (map (T.pack . singleton) rest))
      | isJust $ documentOnTypeFormattingHandler h
      , Nothing <- documentOnTypeFormattingTriggerCharacters o =
          error "documentOnTypeFormattingTriggerCharacters needs to be set if a documentOnTypeFormattingHandler is set"
      | otherwise = Nothing

    executeCommandProvider
      | isJust $ executeCommandHandler h
      , Just cmds <- executeCommandCommands o = Just (J.ExecuteCommandOptions (J.List cmds))
      | isJust $ executeCommandHandler h
      , Nothing <- executeCommandCommands o =
          error "executeCommandCommands needs to be set if a executeCommandHandler is set"
      | otherwise = Nothing

    sync = case textDocumentSync o of
            Just x -> Just (J.TDSOptions x)
            Nothing -> Nothing

    workspace = J.WorkspaceOptions workspaceFolder
    workspaceFolder = case didChangeWorkspaceFoldersNotificationHandler h of
      Just _ -> Just $
        -- sign up to receive notifications
        J.WorkspaceFolderOptions (Just True) (Just (J.WorkspaceFolderChangeNotificationsBool True))
      Nothing -> Nothing

progressCancelHandler :: (MonadConc m) => TVar (STM m) (LanguageContextData (STM m) m config) -> J.WorkDoneProgressCancelNotification -> m ()
progressCancelHandler tvarCtx (J.NotificationMessage _ _ (J.WorkDoneProgressCancelParams tid)) = do
  mact <- Map.lookup tid . progressCancel . resProgressData <$> readTVarConc tvarCtx
  case mact of
    Nothing -> return ()
    Just cancelAction -> cancelAction


-- |
--
shutdownRequestHandler :: (MonadConc m) => TVar (STM m) (LanguageContextData (STM m) m config) -> J.ShutdownRequest -> m ()
shutdownRequestHandler tvarCtx req@(J.RequestMessage _ origId _ _) =
  flip E.catches (defaultErrorHandlers tvarCtx (J.responseId origId) req) $ do
  let res  = makeResponseMessage req Nothing

  sendResponse tvarCtx $ RspShutdown res

-- ---------------------------------------------------------------------

-- | Take the new diagnostics, update the stored diagnostics for the given file
-- and version, and publish the total to the client.
publishDiagnostics :: (MonadConc m) => TVar (STM m) (LanguageContextData (STM m) m config) -> PublishDiagnosticsFunc m
publishDiagnostics tvarDat maxDiagnosticCount uri version diags = do
  ctx <- readTVarConc tvarDat
  let ds = updateDiagnostics (resDiagnostics ctx) uri version diags
  atomically $ writeTVar tvarDat $ ctx{resDiagnostics = ds}
  let mdp = getDiagnosticParamsFor maxDiagnosticCount ds uri
  case mdp of
    Nothing -> return ()
    Just params -> do
      resSendResponse ctx $ NotPublishDiagnostics
        $ J.NotificationMessage "2.0" J.TextDocumentPublishDiagnostics params

-- ---------------------------------------------------------------------

-- | Take the new diagnostics, update the stored diagnostics for the given file
-- and version, and publish the total to the client.
flushDiagnosticsBySource :: (MonadConc m) => TVar (STM m) (LanguageContextData (STM m) m config) -> FlushDiagnosticsBySourceFunc m
flushDiagnosticsBySource tvarDat maxDiagnosticCount msource = do
  -- logs $ "haskell-lsp:flushDiagnosticsBySource:source=" ++ show source
  ctx <- readTVarConc tvarDat
  let ds = flushBySource (resDiagnostics ctx) msource
  atomically $ writeTVar tvarDat $ ctx {resDiagnostics = ds}
  -- Send the updated diagnostics to the client
  forM_ (Map.keys ds) $ \uri -> do
    -- logs $ "haskell-lsp:flushDiagnosticsBySource:uri=" ++ show uri
    let mdp = getDiagnosticParamsFor maxDiagnosticCount ds uri
    case mdp of
      Nothing -> return ()
      Just params -> do
        resSendResponse ctx $ NotPublishDiagnostics
          $ J.NotificationMessage "2.0" J.TextDocumentPublishDiagnostics params

-- |=====================================================================
--
--  utility


-- |
--  Logger
--
setupLogger :: Maybe FilePath -> [String] -> Priority -> IO ()
setupLogger mLogFile extraLogNames level = do

  logStream <- case mLogFile of
    Just logFile -> openFile logFile AppendMode
    Nothing      -> return stderr
  hSetEncoding logStream utf8

  logH <- LHS.streamHandler logStream level

  let logHandle  = logH {LHS.closeFunc = hClose}
      logFormat  = L.tfLogFormatter _LOG_FORMAT_DATE _LOG_FORMAT
      logHandler = LH.setFormatter logHandle logFormat

  L.updateGlobalLogger L.rootLoggerName $ L.setHandlers ([] :: [LHS.GenericHandler Handle])
  L.updateGlobalLogger _LOG_NAME $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_NAME $ L.setLevel level

  -- Also route the additional log names to the same log
  forM_ extraLogNames $ \logName -> do
    L.updateGlobalLogger logName $ L.setHandlers [logHandler]
    L.updateGlobalLogger logName $ L.setLevel level


-- ---------------------------------------------------------------------

-- | The changes in a workspace edit should be applied from the end of the file
-- toward the start. Sort them into this order.
reverseSortEdit :: J.WorkspaceEdit -> J.WorkspaceEdit
reverseSortEdit (J.WorkspaceEdit cs dcs) = J.WorkspaceEdit cs' dcs'
  where
    cs' :: Maybe J.WorkspaceEditMap
    cs' = (fmap . fmap ) sortTextEdits cs

    dcs' :: Maybe (J.List J.TextDocumentEdit)
    dcs' = (fmap . fmap ) sortTextDocumentEdits dcs

    sortTextEdits :: J.List J.TextEdit -> J.List J.TextEdit
    sortTextEdits (J.List edits) = J.List (L.sortBy down edits)

    sortTextDocumentEdits :: J.TextDocumentEdit -> J.TextDocumentEdit
    sortTextDocumentEdits (J.TextDocumentEdit td (J.List edits)) = J.TextDocumentEdit td (J.List edits')
      where
        edits' = L.sortBy down edits

    down (J.TextEdit r1 _) (J.TextEdit r2 _) = r2 `compare` r1
