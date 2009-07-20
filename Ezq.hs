module Main where

import           System.Environment(getArgs)
import           Text.Printf
import           Text.Regex.Posix
import           Control.Monad
import           Control.Monad.Trans(liftIO)
import           Control.Concurrent
import Text.JSON(decode, Result(..))
import qualified Network.Shed.Httpd as Httpd
import qualified Network.URI as URI

import Queue
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
  mq <- newMVar emptyQueueState

  Httpd.initServer port (dispatch mq)

dispatch :: MVar QueueState -> Httpd.Request -> IO Httpd.Response
dispatch mq req = do
  maybeLogRequest req

  -- Run the dispatcher in the other thread.

  case path =~ "^/([^/]+)/(.*)$" :: (String, String, String, [String])  of
    ("", path, "", [a, o]) -> dispatchAction a o body mq
    _                      -> return $ Httpd.Response 404 [] "no such path"
    where path = URI.uriPath $ Httpd.reqURI req
          body = Httpd.reqBody req

-- TODO: helpers for withMVar, modifyMVar (withMQueue, modifyMQueue)

dispatchAction :: String            -- ^ action name
               -> String            -- ^ object
               -> String            -- ^ HTTP body
               -> MVar QueueState   -- ^ the current queue state mvar
               -> IO Httpd.Response
dispatchAction "get" _ body mq = do
  case (decode body) :: Result GetRequest of
    Ok req ->
      return $ Httpd.Response 200 [] $ printf "req! %s" (show req)
    Error what -> 
      return $ Httpd.Response 400 [] $ printf "bad request %s" what
dispatchAction "edit" _ body mq = do
  case (decode body) :: Result EditRequest of
    Ok req ->
      return $ Httpd.Response 200 [] $ printf "req! %s" (show req)
    Error what ->
      return $ Httpd.Response 400 [] $ printf "bad request %s" what

dispatchAction action _ _ _ =
  return $ Httpd.Response 404 [] $ printf "invalid action %s!" action
