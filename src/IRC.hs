module IRC where

type User = String
type Channel = String
type Message = String

class Monad m => IRCMonad m where
  privMsg :: User -> Message -> m ()
  chanMsg :: Channel -> Message -> m ()
  isKnown :: User -> m Bool


lengthLimit = 456

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

