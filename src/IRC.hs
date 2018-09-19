module IRC where

lengthLimit = 456

type Page = Int
type PagePrefix = Page -> String

paging :: PagePrefix -> [String] -> (Int -> String)
paging pre = go 0 (pre 0)
  where
    go :: Page -> String -> [String] -> Int -> String
    go cur str [] n
      | cur == n  = str
      | otherwise = "Invalid page"
    go cur str (x:xs) n = let new = str ++ " " ++  x in
      if length new > lengthLimit then
        if cur == n then
          str
        else
          go (cur + 1) (pre (cur + 1)) (x:xs) n
      else
        go cur new xs n

