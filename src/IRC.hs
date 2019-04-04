module IRC where

import           Data.Text (Text)
import qualified Data.Text as Text

type User = Text
type Channel = Text
type Message = Text

lengthLimit :: Int
lengthLimit = 454

ircLimit :: Text -> Bool
ircLimit = (<lengthLimit) . Text.length

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

