module Main where

import           System.Environment(getArgs)
import           System.Time
import           Text.Printf
import           Text.Regex.Posix
import           Data.Maybe
import           Data.Either
import           Control.Monad
import           Control.Concurrent
import Text.JSON(decode, encode, Result(..))
import qualified Network.Shed.Httpd as Httpd
import qualified Network.URI as URI
import Data.Task.QueueSet(QueueSet)
import qualified Data.Task.QueueSet as QS

import Request

maybeLogRequest req = do
  putStrLn $ printf "%s %s" 
             (Httpd.reqMethod req) 
             (URI.uriPath (Httpd.reqURI req))

main :: IO ()
main = do
  getArgs >>= run >> return ()
    where run [port] = realMain ((read port)::Int)
          run []     = realMain 8080
          run _      = error "ezq [port]"

realMain port = do
  putStrLn $ printf "serving on port %d" port

  -- Create a new MVar, curry it onto the dispatcher, then initialize
  -- the server.
  mq <- newMVar QS.empty

  Httpd.initServer port (dispatch mq)

resp code = Httpd.Response code []

dispatch :: MVar (QueueSet String) -> Httpd.Request -> IO Httpd.Response
dispatch mq req = do
  -- maybeLogRequest req
  if path /= "/"
    then return $ Httpd.Response 404 [] "invalid path"
    else dispatch' mq req
  where
    path = URI.uriPath $ Httpd.reqURI req

dispatch' mq req =
  case decode body of
    Ok decoded -> execute decoded
    Error _    -> return $ resp 400 "invalid request"

  where 
    body = Httpd.reqBody req

    execute (GetRequest queues) = do
      now     <- getClockTime
      modifyMVar mq $ \qs ->
        case QS.getAny now queues qs of
          Just (which, qs') -> return (qs', resp 200 (encode which))
          Nothing           -> return (qs,  resp 200 "none")

    execute (EditRequest ops) = do
      modifyMVar mq $ \qs ->
        case foldM applyOp qs ops of
          Just qs' -> return (qs', resp 200 "ok")
          Nothing  -> return (qs, resp 400 "fail")

    applyOp qs (Add queue task)     = Just $ QS.add task queue qs
    applyOp qs (Remove queue ident) = QS.done (queue, ident) qs
