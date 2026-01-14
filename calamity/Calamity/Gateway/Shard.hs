{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

-- | The shard logic
module Calamity.Gateway.Shard (
  Shard (..),
  newShard,
) where

import Calamity.Gateway.DispatchEvents (
  CalamityEvent (Dispatch),
  DispatchData (Ready),
 )
import Calamity.Gateway.Intents (Intents)
import Calamity.Gateway.Types (
  ControlMessage (..),
  IdentifyData (
    IdentifyData,
    compress,
    intents,
    largeThreshold,
    presence,
    properties,
    shard,
    token
  ),
  IdentifyProps (IdentifyProps, browser, device),
  ReceivedDiscordMessage (
    EvtDispatch,
    HeartBeatAck,
    HeartBeatReq,
    Hello,
    InvalidSession,
    Reconnect
  ),
  ResumeData (ResumeData, seq, sessionID, token),
  SentDiscordMessage (HeartBeat, Identify, Resume, StatusUpdate),
  Shard (..),
  ShardC,
  ShardFlowControl (..),
  ShardMsg (..),
  ShardState (ShardState, wsConn),
  StatusUpdateData,
 )
import Calamity.Internal.RunIntoIO (bindSemToIO)
import Calamity.Internal.Utils (
  debug,
  error,
  info,
  leftToMaybe,
  swap,
  unlessM,
  untilJustFinalIO,
  whenJust,
  whileMFinalIO,
 )
import Calamity.Metrics.Eff (
  MetricEff,
  modifyGauge,
  registerGauge,
 )
import Calamity.Types.LogEff (LogEff)
import Calamity.Types.Token (Token, rawToken)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, cancel)
import Control.Concurrent.Chan.Unagi qualified as UC
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TBMQueue (
  TBMQueue,
  closeTBMQueue,
  newTBMQueueIO,
  readTBMQueue,
  tryWriteTBMQueue,
  writeTBMQueue,
 )
import Control.Exception (
  Exception (fromException),
  SomeException,
  throwIO,
 )
import Control.Exception.Safe qualified as Ex
import Control.Monad (unless, void, when)
import Control.Monad.State.Lazy (runState)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as LBS
import Data.Default.Class (def)
import Data.IORef (newIORef)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import DiPolysemy (attr, push)
import Network.Connection qualified as NC
import Network.HTTP.Client (Manager, Proxy (..))
import Network.HTTP.Client.Internal (connectionClose, connectionRead, connectionWrite, makeConnection)
import Network.HTTP.Client.Internal qualified as HTTP.Internal
import Network.TLS qualified as NT
import Network.TLS.Extra qualified as NT
import Network.WebSockets (ConnectionException (..), receiveData, sendCloseCode, sendTextData)
import Network.WebSockets qualified as NW
import Network.WebSockets.Stream qualified as NW
import Optics
import Optics.State.Operators
import Polysemy (Sem)
import Polysemy qualified as P
import Polysemy.Async qualified as P
import Polysemy.AtomicState qualified as P
import Polysemy.Error qualified as P
import Polysemy.Resource qualified as P
import System.X509 qualified as X509
import TextShow (showt)
import Prelude hiding (error)

{- | Establishes a WebSocket connection, respecting HTTP proxy settings from the Manager.

This function automatically handles:
- Direct connections when no proxy is configured
- HTTP CONNECT tunneling through HTTP proxies (configured via HTTP_PROXY/HTTPS_PROXY environment variables)
- TLS encryption after proxy tunneling
-}
runWebsocket ::
  (P.Members '[LogEff, P.Final IO, P.Embed IO] r) =>
  Manager ->
  T.Text ->
  T.Text ->
  (NW.Connection -> P.Sem r a) ->
  P.Sem r (Maybe a)
runWebsocket mgr host path ma = do
  inner <- bindSemToIO ma

  let logExc e = debug $ "runWebsocket raised with " <> (T.pack . show $ e)
  logExc' <- bindSemToIO logExc
  let handler e = do
        void $ logExc' e
        pure Nothing

  P.embed . Ex.handleAny handler $ do
    certStore <- X509.getSystemCertificateStore
    let clientParams =
          (NT.defaultParamsClient (T.unpack host) "443")
            { NT.clientSupported = def {NT.supportedCiphers = NT.ciphersuite_default}
            , NT.clientShared =
                def
                  { NT.sharedCAStore = certStore
                  }
            }

    -- Use Manager to create connection - automatically handles proxy
    rawConn <- openTlsConnectionThroughManager mgr (T.unpack host) 443 clientParams

    -- Wrap raw connection in WebSocket stream
    stream <-
      NW.makeStream
        (Just <$> connectionRead rawConn)
        (maybe (pure ()) (connectionWrite rawConn . LBS.toStrict))

    Ex.bracket
      (pure rawConn)
      connectionClose
      (const $ NW.runClientWithStream stream (T.unpack host) (T.unpack path) NW.defaultConnectionOptions [] inner)

{- | Opens a TLS connection using the Manager's proxy settings.

Extracts proxy configuration from the Manager and either:
- Connects directly if no proxy is configured
- Tunnels through HTTP proxy using HTTP CONNECT method
-}
openTlsConnectionThroughManager ::
  Manager ->
  String ->
  Int ->
  NT.ClientParams ->
  IO HTTP.Internal.Connection
openTlsConnectionThroughManager mgr host port clientParams = do
  -- Extract proxy settings from Manager
  -- Create a dummy request to get proxy settings from Manager
  let dummyReq =
        HTTP.Internal.defaultRequest
          { HTTP.Internal.host = BS8.pack host
          , HTTP.Internal.port = port
          , HTTP.Internal.secure = True
          }
  -- Apply Manager's proxy override to the request
  let modifiedReq = HTTP.Internal.mSetProxy mgr dummyReq
      maybeProxySettings = HTTP.Internal.proxy modifiedReq

  case maybeProxySettings of
    Just (Proxy proxyHost proxyPort) -> do
      -- Connect through HTTP proxy using HTTP CONNECT
      connectThroughHttpProxy proxyHost proxyPort host port clientParams
    Nothing -> do
      -- Direct connection with TLS
      ctx <- NC.initConnectionContext
      let tlsSettings = NC.TLSSettings clientParams
          connParams = NC.ConnectionParams host (fromIntegral port) (Just tlsSettings) Nothing

      conn <- NC.connectTo ctx connParams

      -- Convert NC.Connection to http-client's Connection
      makeConnection
        (NC.connectionGetChunk conn)
        (NC.connectionPut conn)
        (NC.connectionClose conn)

{- | Establishes a connection through an HTTP proxy using the CONNECT method.

Steps:
1. Connects to the proxy server (plain TCP)
2. Sends HTTP CONNECT request for the target host:port
3. Waits for 200 OK response from proxy
4. Upgrades the connection to TLS
5. Returns a Connection ready for use
-}
connectThroughHttpProxy ::
  BS.ByteString ->
  Int ->
  String ->
  Int ->
  NT.ClientParams ->
  IO HTTP.Internal.Connection
connectThroughHttpProxy proxyHost proxyPort targetHost targetPort clientParams = do
  ctx <- NC.initConnectionContext

  -- 1. Connect to proxy (plain TCP)
  let proxyParams =
        NC.ConnectionParams
          { NC.connectionHostname = BS8.unpack proxyHost
          , NC.connectionPort = fromIntegral proxyPort
          , NC.connectionUseSecure = Nothing
          , NC.connectionUseSocks = Nothing
          }

  proxyConn <- NC.connectTo ctx proxyParams

  -- 2. Send HTTP CONNECT request
  let connectRequest =
        "CONNECT "
          <> targetHost
          <> ":"
          <> show targetPort
          <> " HTTP/1.1\r\n"
          <> "Host: "
          <> targetHost
          <> ":"
          <> show targetPort
          <> "\r\n"
          <> "\r\n"
  NC.connectionPut proxyConn (BS8.pack connectRequest)

  -- 3. Read proxy response
  responseLine <- NC.connectionGetLine 1024 proxyConn
  unless (BS.isPrefixOf "HTTP/1.1 200" responseLine || BS.isPrefixOf "HTTP/1.0 200" responseLine) $
    throwIO $
      userError $
        "Proxy CONNECT to " <> targetHost <> ":" <> show targetPort <> " failed. "
          <> "Proxy response: "
          <> BS8.unpack responseLine

  -- 4. Skip remaining headers (until empty line)
  skipProxyHeaders 100 proxyConn

  -- 5. Upgrade connection to TLS by creating a Backend from NC.Connection
  let backend =
        NT.Backend
          { NT.backendFlush = pure ()
          , NT.backendClose = NC.connectionClose proxyConn
          , NT.backendSend = NC.connectionPut proxyConn
          , NT.backendRecv = NC.connectionGet proxyConn
          }
  tlsCtx <- NT.contextNew backend clientParams
  NT.handshake tlsCtx

  -- 6. Convert to http-client Connection
  makeConnection
    (NT.recvData tlsCtx)
    (NT.sendData tlsCtx . LBS.fromStrict)
    (NT.bye tlsCtx >> NC.connectionClose proxyConn)

{- | Skips HTTP headers by reading lines until an empty line is encountered.

HTTP headers are terminated by an empty line (just CRLF).
Takes a maximum number of headers to skip as a safety limit.
-}
skipProxyHeaders :: Int -> NC.Connection -> IO ()
skipProxyHeaders maxHeaders conn = go maxHeaders
  where
    go 0 = throwIO $ userError "Too many proxy headers (limit exceeded)"
    go n = do
      line <- NC.connectionGetLine 1024 conn
      unless (BS.null line || line == "\r" || line == "\r\n") $
        go (n - 1)

newShardState :: Shard -> Manager -> ShardState
newShardState shard mgr = ShardState shard Nothing Nothing False Nothing Nothing Nothing mgr

-- | Creates and launches a shard
newShard ::
  (P.Members '[LogEff, MetricEff, P.Embed IO, P.Final IO, P.Async] r) =>
  Manager ->
  T.Text ->
  Int ->
  Int ->
  Token ->
  Maybe StatusUpdateData ->
  Intents ->
  UC.InChan CalamityEvent ->
  Sem r (UC.InChan ControlMessage, Async (Maybe ()))
newShard mgr gateway id count token presence intents evtIn = do
  (cmdIn, cmdOut) <- P.embed UC.newChan
  let shard = Shard id count gateway evtIn cmdOut (rawToken token) presence intents
  stateVar <- P.embed . newIORef $ newShardState shard mgr

  let runShard = P.runAtomicStateIORef stateVar shardLoop
  let action = push "calamity-shard" . attr "shard-id" id $ runShard

  thread' <- P.async action

  pure (cmdIn, thread')

sendToWs :: (ShardC r) => SentDiscordMessage -> Sem r ()
sendToWs data' = do
  wsConn' <- P.atomicGets wsConn
  case wsConn' of
    Just wsConn -> do
      let encodedData = A.encode data'
      debug . T.pack $ "sending " <> show data' <> " encoded to " <> show encodedData <> " to gateway"
      P.embed . sendTextData wsConn $ encodedData
    Nothing -> debug "tried to send to closed WS"

tryWriteTBMQueue' :: TBMQueue a -> a -> STM Bool
tryWriteTBMQueue' q v = do
  v' <- tryWriteTBMQueue q v
  case v' of
    Just False -> retry
    Just True -> pure True
    Nothing -> pure False

restartUnless :: (P.Members '[LogEff, P.Error ShardFlowControl] r) => T.Text -> Maybe a -> P.Sem r a
restartUnless _ (Just a) = pure a
restartUnless msg Nothing = do
  error msg
  P.throw ShardFlowRestart

-- | The loop a shard will run on
shardLoop :: (ShardC r) => Sem r ()
shardLoop = do
  activeShards <- registerGauge "active_shards" mempty
  void $ modifyGauge (+ 1) activeShards
  void outerloop
  void $ modifyGauge (subtract 1) activeShards
  debug "Shard shut down"
  where
    controlStream :: Shard -> TBMQueue ShardMsg -> IO ()
    controlStream shard outqueue = inner
      where
        q = shard ^. #cmdOut
        inner = do
          v <- UC.readChan q
          r <- atomically $ tryWriteTBMQueue' outqueue (Control v)
          when r inner

    handleWSException :: SomeException -> IO (Either (ControlMessage, Maybe T.Text) a)
    handleWSException e = pure $ case fromException e of
      Just (CloseRequest code _)
        | code `elem` [4004, 4010, 4011, 4012, 4013, 4014] ->
            Left (ShutDownShard, Just . showt $ code)
      e -> Left (RestartShard, Just . T.pack . show $ e)

    discordStream :: (P.Members '[LogEff, MetricEff, P.Embed IO, P.Final IO] r) => NW.Connection -> TBMQueue ShardMsg -> Sem r ()
    discordStream ws outqueue = inner
      where
        inner = do
          msg <- P.embed $ Ex.catchAny (Right <$> receiveData ws) handleWSException

          case msg of
            Left (c, reason) -> do
              whenJust reason (\r -> error . T.pack $ "Shard closed with reason: " <> show r)
              P.embed . atomically $ writeTBMQueue outqueue (Control c)
            Right msg' -> do
              -- debug [fmt|Got msg: {msg'}|]
              let decoded = A.eitherDecode msg'
              r <- case decoded of
                Right a ->
                  P.embed . atomically $ tryWriteTBMQueue' outqueue (Discord a)
                Left e -> do
                  error . T.pack $ "Failed to decode " <> e <> ": " <> show msg'
                  pure True
              when r inner
    outerloop :: (ShardC r) => Sem r ()
    outerloop = whileMFinalIO $ do
      shard :: Shard <- P.atomicGets (^. #shardS)
      mgr <- P.atomicGets (^. #httpManager)
      let host = shard ^. #gateway
      let host' = fromMaybe host $ T.stripPrefix "wss://" host
      info . T.pack $ "starting up shard " <> show (shardID shard) <> " of " <> show (shardCount shard)

      innerLoopVal <- runWebsocket mgr host' "/?v=9&encoding=json" innerloop

      case innerLoopVal of
        Just ShardFlowShutDown -> do
          info "Shutting down shard"
          pure False
        Just ShardFlowRestart -> do
          info "Restaring shard"
          pure True
        -- we restart normally when we loop

        Nothing -> do
          -- won't happen unless innerloop starts using a non-deterministic effect or connecting to the ws dies
          info "Restarting shard (abnormal reasons?)"
          pure True

    innerloop :: (ShardC r) => NW.Connection -> Sem r ShardFlowControl
    innerloop ws = do
      debug "Entering inner loop of shard"

      shard <- P.atomicGets (^. #shardS)
      P.atomicModify' (#wsConn ?~ ws)

      seqNum' <- P.atomicGets (^. #seqNum)
      sessionID' <- P.atomicGets (^. #sessionID)

      case (seqNum', sessionID') of
        (Just n, Just s) -> do
          debug $ "Resuming shard (sessionID: " <> s <> ", seq: " <> T.pack (show n)
          sendToWs
            ( Resume
                ResumeData
                  { token = shard ^. #token
                  , sessionID = s
                  , seq = n
                  }
            )
        _noActiveSession -> do
          debug "Identifying shard"
          sendToWs
            ( Identify
                IdentifyData
                  { token = shard ^. #token
                  , properties =
                      IdentifyProps
                        { browser = "Calamity: https://github.com/simmsb/calamity"
                        , device = "Calamity: https://github.com/simmsb/calamity"
                        }
                  , compress = False
                  , largeThreshold = Nothing
                  , shard =
                      Just (shard ^. #shardID, shard ^. #shardCount)
                  , presence = shard ^. #initialStatus
                  , intents = shard ^. #intents
                  }
            )

      result <-
        P.resourceToIOFinal $
          P.bracket
            (P.embed $ newTBMQueueIO 1)
            (P.embed . atomically . closeTBMQueue)
            ( \q -> do
                debug "handling events now"
                _controlThread <- P.async . P.embed $ controlStream shard q
                _discordThread <- P.async $ discordStream ws q
                P.raise . untilJustFinalIO . (leftToMaybe <$>) . P.runError $ do
                  -- only we close the queue
                  msg <- P.embed . atomically $ readTBMQueue q
                  handleMsg =<< restartUnless "shard message stream closed by someone other than the sink" msg
            )

      debug "Exiting inner loop of shard"

      P.atomicModify' (#wsConn .~ Nothing)
      haltHeartBeat
      pure result
    handleMsg :: (ShardC r, P.Member (P.Error ShardFlowControl) r) => ShardMsg -> Sem r ()
    handleMsg (Discord msg) = case msg of
      EvtDispatch sn data' -> do
        -- trace $ "Handling event: ("+||data'||+")"
        P.atomicModify' (#seqNum ?~ sn)

        case data' of
          Ready rdata' ->
            P.atomicModify' (#sessionID ?~ (rdata' ^. #sessionID))
          _NotReady -> pure ()

        shard <- P.atomicGets (^. #shardS)
        P.embed $ UC.writeChan (shard ^. #evtIn) (Dispatch (shard ^. #shardID) data')
      HeartBeatReq -> do
        debug "Received heartbeat request"
        sendHeartBeat
      Reconnect -> do
        debug "Being asked to restart by Discord"
        P.throw ShardFlowRestart
      InvalidSession resumable -> do
        if resumable
          then info "Received resumable invalid session"
          else do
            info "Received non-resumable invalid session, sleeping for 15 seconds then retrying"
            P.atomicModify' (#sessionID .~ Nothing)
            P.atomicModify' (#seqNum .~ Nothing)
            P.embed $ threadDelay (15 * 1000 * 1000)
        P.throw ShardFlowRestart
      Hello interval -> do
        info . T.pack $ "Received hello, beginning to heartbeat at an interval of " <> show interval <> "ms"
        startHeartBeatLoop interval
      HeartBeatAck -> do
        debug "Received heartbeat ack"
        P.atomicModify' (#hbResponse .~ True)
    handleMsg (Control msg) = case msg of
      SendPresence data' -> do
        debug . T.pack $ "Sending presence: (" <> show data' <> ")"
        sendToWs $ StatusUpdate data'
      RestartShard -> P.throw ShardFlowRestart
      ShutDownShard -> P.throw ShardFlowShutDown

startHeartBeatLoop :: (ShardC r) => Int -> Sem r ()
startHeartBeatLoop interval = do
  haltHeartBeat -- cancel any currently running hb thread
  thread <- P.async $ heartBeatLoop interval
  P.atomicModify' (#hbThread ?~ thread)

haltHeartBeat :: (ShardC r) => Sem r ()
haltHeartBeat = do
  thread <- P.atomicState @ShardState . (swap .) . runState $ do
    thread <- use #hbThread
    #hbThread .= Nothing
    pure thread
  case thread of
    Just t -> do
      debug "Stopping heartbeat thread"
      P.embed (void $ cancel t)
    Nothing -> pure ()

sendHeartBeat :: (ShardC r) => Sem r ()
sendHeartBeat = do
  sn <- P.atomicGets (^. #seqNum)
  debug . T.pack $ "Sending heartbeat (seq: " <> show sn <> ")"
  sendToWs $ HeartBeat sn
  P.atomicModify' (#hbResponse .~ False)

heartBeatLoop :: (ShardC r) => Int -> Sem r ()
heartBeatLoop interval = untilJustFinalIO . (leftToMaybe <$>) . P.runError $ do
  sendHeartBeat
  P.embed . threadDelay $ interval * 1000
  unlessM (P.atomicGets (^. #hbResponse)) $ do
    debug "No heartbeat response, restarting shard"
    wsConn <- P.note () =<< P.atomicGets (^. #wsConn)
    P.embed $ sendCloseCode wsConn 4000 ("No heartbeat in time" :: T.Text)
    P.throw ()
