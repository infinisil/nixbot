module IRC where

import           Control.Monad.Logger
import           Control.Monad.Reader

type User = String
type Channel = String
type Message = String

class Monad m => IRCMonad m where
  privMsg :: User -> Message -> m ()
  chanMsg :: Channel -> Message -> m ()
  isKnown :: User -> m Bool

instance IRCMonad m => IRCMonad (LoggingT m) where
  privMsg user msg = lift $ privMsg user msg
  chanMsg chan msg = lift $ chanMsg chan msg
  isKnown user = lift $ isKnown user

instance IRCMonad m => IRCMonad (ReaderT r m) where
  privMsg user msg = lift $ privMsg user msg
  chanMsg channel msg = lift $ chanMsg channel msg
  isKnown user = lift $ isKnown user

lengthLimit :: Int
lengthLimit = 456

ircLimit :: String -> Bool
ircLimit = (<lengthLimit) . length

type Page = Int

paging :: [String] -> (Int -> String)
paging = go 0 ""
  where
    go :: Page -> String -> [String] -> Int -> String
    go cur str [] n
      | cur == n  = str
      | otherwise = "Invalid page"
    go cur str (x:xs) n = let new = if null str then x else str ++ " " ++  x in
      if length new > lengthLimit then
        if cur == n then
          str
        else
          go (cur + 1) "" (x:xs) n
      else
        go cur new xs n

