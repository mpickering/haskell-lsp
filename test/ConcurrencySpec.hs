{-# LANGUAGE OverloadedStrings #-}
module ConcurrencySpec where

import Test.HUnit.DejaFu
import           Control.Concurrent.Classy.MVar
import           Control.Concurrent.Classy.Async
import           Control.Concurrent.Classy.STM
import Control.Concurrent.Classy
import           Data.Aeson
import           Data.Default
import           Language.Haskell.LSP.Core
import           Language.Haskell.LSP.VFS
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Capabilities
import           Test.Hspec
import qualified Test.DejaFu as D
import Data.Maybe
import Test.Hspec.Contrib.HUnit
import Control.Monad.IO.Class
import Data.Text (Text, pack)
import Control.Monad

initSession :: MonadConc m => m (TVar (STM m) (LanguageContextData (STM m) m ()))
initSession = do
    let
        initialConfigHandler (RequestMessage _ _ Initialize InitializeParams{_initializationOptions = Just opts}) =
          case (fromJSON opts :: Result String) of
                Success s -> Right s
                _         -> Left "Could not decode configuration"
        initialConfigHandler _ =
          error "Got the wrong request for the onInitialConfiguration callback"

        initCb :: InitializeCallbacks IO String
        initCb = InitializeCallbacks
          initialConfigHandler
          (const $ Left "")
          (\lf -> return Nothing)
          --(\lf -> putMVar lfVar lf >> return Nothing)

        handlers = def

    tvarLspId <- atomically $ newTVar 0
    tvarCtx   <- atomically $ newTVar $ defaultLanguageContextData handlers
                                                        def
                                                        undefined
                                                        tvarLspId
                                                        (const $ return ())
                                                        Nothing
    return tvarCtx

    {-
    let putMsg msg =
          let jsonStr = encode msg in handleMessage initCb tvarCtx jsonStr

    let
        initParams        = InitializeParams
          Nothing
          Nothing
          (Just (Uri "/foo"))
          (Just (Data.Aeson.String "configuration"))
          fullCaps
          Nothing
          Nothing

        initMsg :: InitializeRequest
        initMsg = RequestMessage "2.0" (IdInt 0) Initialize initParams
-}
--    putMsg initMsg
--    contents <- readTVarIO tvarCtx
--    resConfig contents  `shouldBe` Just "configuration"

sendRequest :: (MonadIO m, MonadConc m) => TVar (STM m) (LanguageContextData (STM m) m ()) -> ClientMethod -> Value -> m ()
sendRequest tvarDat t json = do
  ctx <- readTVarConc tvarDat
  let h = resHandlers ctx
  handlerMap undefined h t tvarDat json


fake_path = Uri "file://fake_path"
fake_npath = toNormalizedUri fake_path

program :: (MonadIO m, MonadConc m) => TVar (STM m) (LanguageContextData (STM m) m ()) -> m (Maybe VirtualFile)
program tvarCtx = do
  sendRequest tvarCtx TextDocumentDidOpen (wrap 0 TextDocumentDidOpen (DidOpenTextDocumentParams (TextDocumentItem fake_path "hs" 0 "")))
  let changeReq n = sendRequest tvarCtx TextDocumentDidChange (mkChange n)
  changes <- asyncN "changes" $ forM_ [0..1] changeReq
  persist <- asyncN "persist" $ persistVirtualFile tvarCtx fake_npath
  void $ waitBoth changes persist
  res <- getVirtualFile tvarCtx fake_npath
  return res

wrap :: ToJSON a => Int -> ClientMethod -> a -> Value
wrap n method params = object ["jsonrpc" .= ("2.0" :: String), "params" .= params, "method" .= method, "id" .= n]

mkChange :: Int -> Value
mkChange n =
  let versioned_doc = VersionedTextDocumentIdentifier fake_path (Just n)
      change = TextDocumentContentChangeEvent (Just (Range (Position 1 1) (Position 1 1))) (Just 1) (pack $ show n)
      changes = List [change]

  in wrap (n + 1) TextDocumentDidChange $ DidChangeTextDocumentParams versioned_doc changes


spec :: Spec
spec = do
  let pred :: Maybe VirtualFile -> Maybe Text
      pred x = virtualFileText <$> x
  fromHUnitTest $ testDejafu "Test that changes to VFS are not lost" (D.alwaysSameOn pred) (withSetup initSession program)


