{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Posix.Signals
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception          (SomeException, handle)
import System.Exit (exitFailure)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.Aeson.Types           (fieldLabelModifier)
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Text
import Data.Time.Clock
import           GHC.Generics
import           GitHub.Data.Name
import qualified GitHub.Endpoints.Repos     as R
import qualified Network.AMQP               as A
import qualified Network.AMQP.Worker        as W
import qualified Network.HTTP.Simple        as H
import           System.Environment
import           System.IO
import           System.Process             (createProcess, proc, std_in, std_out, waitForProcess, std_err, StdStream(CreatePipe), env, getProcessExitCode, ProcessHandle, interruptProcessGroupOf)
import           System.Process.Internals
import           Text.Regex.TDFA



data Input = Input
  { in_from   :: String
  , in_body   :: String
  , in_sender :: String
  } deriving (Show, Generic)

data Output = Output
  { out_target :: String
  , out_body   :: String
  } deriving (Show, Generic)

data Issue = Issue
  { i_title  :: String
  , i_state  :: String
  , i_url    :: String
  , i_author :: String
  } deriving (Show, Generic)

instance FromJSON Issue where
  parseJSON (Object v) = do
    title <- v .: "title"
    state <- v .: "state"
    url <- v .: "html_url"
    user <- v .: "user"
    case user of
      Object o -> do
        login <- o .: "login"
        return $ Issue title state url login
      _ -> mzero

  parseJSON _ = mzero

instance FromJSON Input where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \s -> fromMaybe s (L.stripPrefix "in_" s) }
instance ToJSON Output where
  toEncoding = genericToEncoding defaultOptions
    { fieldLabelModifier = \s -> fromMaybe s (L.stripPrefix "out_" s) }

exchange :: A.ExchangeOpts
exchange = A.newExchange
  { A.exchangeName = "exchange-messages"
  , A.exchangeType = "fanout"
  , A.exchangePassive = True
  , A.exchangeDurable = True
  , A.exchangeAutoDelete = False
  }

--input :: W.Queue W.Direct Message
--input = W.queue exchange ""


opts :: A.SASLMechanism -> A.ConnectionOpts
opts auth = A.defaultConnectionOpts
  { A.coVHost = "ircbot"
  , A.coTLSSettings = Just A.TLSTrusted
  , A.coServers = [("events.nix.gsc.io", 5671)]
  , A.coAuth = [auth]
  }


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  cmdArgs <- getArgs
  putStrLn $ "Command line arguments are " ++ show cmdArgs
  args <- if L.length cmdArgs >= 2 then
    return cmdArgs else
    fmap L.words $ readFile "./auth"

  let auth = A.amqplain (pack $ args !! 0) (pack $ args !! 1)
  putStrLn $ "Using authentication: " ++ show args
  server auth

writeDone :: TMVar () -> IO ()
writeDone var = atomically $ putTMVar var ()

server :: A.SASLMechanism -> IO ()
server auth = do
    conn <- A.openConnection'' (opts auth)
    var <- newEmptyTMVarIO
    A.addConnectionClosedHandler conn True $ writeDone var
    handle (onInterrupt conn) $ do
      putStrLn "Got connection"
      chan <- A.openChannel conn
      putStrLn "Got channel"

      (publishQueueName, _, _) <- A.declareQueue chan $ A.newQueue
        { A.queueName = "queue-publish"
        , A.queuePassive = True
        }
      putStrLn $ "Declared queue with name " ++ show publishQueueName

      (myQueue, _, _) <- A.declareQueue chan $ A.newQueue
        { A.queueName = ""
        , A.queueAutoDelete = True
        , A.queueExclusive = True
        }
      putStrLn $ "Declared my queue with name " ++ show myQueue

      A.declareExchange chan exchange
      putStrLn "Declared exchange"

      A.bindQueue chan myQueue "exchange-messages" "queue-publish"
      putStrLn "Bound queue"

      A.confirmSelect chan False
      putStrLn "disabled nowait"

      stateVar <- atomically $ newTVar $ initialState chan
      tag <- A.consumeMsgs chan myQueue A.Ack (onMessage chan stateVar)
      putStrLn $ "Started consumer with tag " ++ show tag

      putStrLn $ "terminating if enter is pressed..."
      atomically $ takeTMVar var
      A.closeConnection conn

onInterrupt :: A.Connection -> SomeException -> IO ()
onInterrupt conn e = do
  putStrLn "Interrupted, closing connection"
  A.closeConnection conn

publishMessage :: A.Channel -> Output -> IO (Maybe Int)
publishMessage chan msg = do
    putStrLn $ "Sending the message " ++ show msg
    -- publish a message to our new exchange
    intMb <- A.publishMsg chan "" "queue-publish" A.newMsg
      { A.msgBody = encode msg
      , A.msgDeliveryMode = Just A.Persistent}

    putStrLn $ "Published Message" ++ maybe "" (\s -> ", got sequence number " ++ show s) intMb
    return intMb

onMessage :: A.Channel -> TVar ServerState -> (A.Message, A.Envelope) -> IO ()
onMessage chan stateVar (m, e) = do
  case decode $ A.msgBody m :: Maybe Input of
    Nothing -> do
      putStrLn $ "Message body invalid: " ++ show (A.msgBody m)
      A.ackEnv e
    Just msg -> do
      putStrLn $ "Valid message " ++ show msg
      forkIO $ do
        state <- readTVarIO stateVar
        (replyBodies, newState) <- runStateT (reply msg) state
        atomically $ writeTVar stateVar newState
        sequence_ $ flip fmap replyBodies (\b -> publishMessage chan (Output (in_from msg) b))
      A.ackEnv e


data ServerState = ServerState
           { commands   :: M.Map String String
           , chanStdin :: M.Map String (ProcessHandle, Handle)
           , channel :: A.Channel
           }

initialState = ServerState M.empty M.empty

parse :: String -> [(String, Int)]
parse s = fmap (\xs -> (xs !! 1, read $ xs !! 2)) matches where
  matches :: [[String]]
  matches = s =~ ("([[:alpha:]]+)#([[:digit:]]+)" :: String)

parseNixpkgs :: String -> [String]
parseNixpkgs s = fmap (\xs -> xs !! 1) matches where
  matches :: [[String]]
  matches = s =~ ("nixpkgs/([^[:space:]]+)+" :: String)

getNixpkgs :: MonadIO m => String -> m (Maybe String)
getNixpkgs s = do
  lg $ "Trying to get contents for " ++ s
  contents <- liftIO $ R.contentsFor "NixOS" "nixpkgs" (pack s) (Just "heads/master")
  case contents of
    Left error -> do
      lg $ show error
      return Nothing
    Right contents -> do
      return $ Just $ "https://github.com/NixOS/nixpkgs/tree/master/" ++ s

nixpkgs :: MonadIO m => String -> m [String]
nixpkgs s = fmap catMaybes . mapM getNixpkgs $ parseNixpkgs s

prToInfo :: (String, Int) -> IO (Maybe String)
prToInfo (p, n) = do
  putStrLn $ "Sending request to github for NixOS/" ++ p ++ "#" ++ show n

  req <- fmap (H.setRequestHeader "user-agent" ["haskell"]) $ H.parseRequest $ "https://api.github.com/repos/NixOS/" ++ p ++ "/issues/" ++ show n
  response <- H.httpJSON req :: IO (H.Response Issue)
  let body = H.getResponseBody response

  return $ Just $ i_url body ++ " (by " ++ i_author body ++ ", " ++ i_state body ++ "): " ++ i_title body

evalNixForChannel :: (MonadIO m, MonadState ServerState m) => String -> String -> m ()
evalNixForChannel chan nix = do
  state <- get
  (process, stdin) <- liftIO $ case M.lookup chan $ chanStdin state of
    Nothing -> do
      putStrLn "No process running for this channel"
      newHandle (channel state)
    Just (process', stdin') -> do
      exitCode <- getProcessExitCode process'
      case exitCode of
        Nothing -> do
          putStrLn "reusing existing process"
          return (process', stdin')
        Just _ -> do
          putStrLn "Process was running but now exited"
          newHandle (channel state)
    
  put state { chanStdin = M.insert chan (process, stdin) (chanStdin state) }
  lg $ "Putting line " ++ nix ++ " to nix process for channel " ++ chan
  liftIO $ hPutStrLn stdin nix
  liftIO $ hFlush stdin
  return ()
    where
      newHandle :: A.Channel -> IO (ProcessHandle, Handle)
      newHandle channel = do
        nixPath <- getEnv "NIX_PATH"
        putStrLn "Starting process"
        (Just stdin, Just stdout, Just stderr, handle) <- createProcess (proc "/run/current-system/sw/bin/nix-repl" [])
          { std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          , env = Just [ ("NIX_REMOTE", "daemon")
                       , ("NIX_PATH", nixPath)
                       ]
          }
        hSetBuffering stdout LineBuffering
        hSetBuffering stderr LineBuffering
        time <- fmap (addUTCTime (-1)) getCurrentTime
        _ <- hGetLine stdout
        hPutStrLn stdin ":a import <nixpkgs> {}"
        hPutStrLn stdin ":a lib"
        hPutStrLn stdin ":a builtins"
        hFlush stdin
        _ <- hGetLine stdout
        _ <- hGetLine stdout
        _ <- hGetLine stdout
        _ <- hGetLine stdout
        _ <- hGetLine stdout
        _ <- hGetLine stdout
        _ <- hGetLine stdout
        _ <- hGetLine stdout
        _ <- hGetLine stdout
        forkIO $ outputToChannel handle chan stderr channel time
        forkIO $ outputToChannel handle chan stdout channel time
        return (handle, stdin)

lg :: MonadIO m => String -> m ()
lg = liftIO . putStrLn 

outputToChannel :: MonadIO m => ProcessHandle -> String -> Handle -> A.Channel -> UTCTime-> m ()
outputToChannel process chan handle channel lastTime = do
  lg "Receiving stdout"
  line <- liftIO $ hGetLine handle
  time <- liftIO getCurrentTime
  lg $ "Got \"" ++ line ++ "\""
  if line == "" || L.isPrefixOf "nix-repl>" line || time `diffUTCTime` lastTime < 1 || line == "error: interrupted by the user" then do
    lg "Not doing anything"
    outputToChannel process chan handle channel lastTime else do
      let actualMessage = L.take 200 line
      liftIO $ publishMessage channel $ Output { out_target = chan, out_body = actualMessage }
      lg "Sent message, waiting for confirmation..."
      liftIO $ A.waitForConfirms channel
      lg "Published line"
      lg "Sending interrupt"
      liftIO $ interrupt process
      lg "Sent interrupt"
      outputToChannel process chan handle channel time

interrupt :: ProcessHandle -> IO ()
interrupt ph = do
  let (ProcessHandle pmvar _) = ph
  ph_ <- takeMVar pmvar
  case ph_ of
    OpenHandle pid -> do  -- pid is a POSIX pid
      signalProcess sigINT pid
      putMVar pmvar ph_

reply :: (MonadIO m, MonadState ServerState m) => Input -> m [String]
reply (Input { in_from = "#bottest", in_sender = user, in_body = "hello!" }) = return ["Hello, " ++ user]
--reply (Input { in_from = chan, in_body = ('>':' ':nix) }) = evalNixForChannel chan nix >> return []
reply (Input { in_body = body}) = do
  a <- nixpkgs body
  b <- fmap catMaybes . mapM (fmap liftIO prToInfo) $ parse body
  return $ a ++ b
