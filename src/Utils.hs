module Utils (prettySeconds, mostMatching, paging) where

import           Data.List
import           Data.Maybe

prettySeconds :: Int -> Integer -> String
prettySeconds relevant seconds = intercalate ", " relevantStrings
  where
    relevantStrings = take relevant . catMaybes $ format "year" rest : strings

    (rest, strings) = mapAccumR next seconds
      [ (52, "week")
      , (7, "day")
      , (24, "hour")
      , (60, "minute")
      , (60, "second")
      ]

    next val (unit, str) = format str <$> divMod val unit

    format _ 0   = Nothing
    format str 1 = Just $ "1 " ++ str
    format str n = Just $ show n ++ " " ++ str ++ "s"


-- | Given a set of items, a mapping from a page number and a set of items to a page, and a page validity condition, return the resulting valid pages. Doesn't return all elements paged in case the page condition can't be met.
paging :: [a] -> (Int -> [a] -> b) -> (b -> Bool) -> [b]
paging xs f p = getPages 0 xs
  where
    getPage n elems = mostMatching elems (\(a, b) -> (f n a, b)) (p . fst)
    getPages _ [] = []
    getPages n elems = case getPage n elems of
      Nothing           -> []
      Just (page, rest) -> page : getPages (n + 1) rest


-- | Returns the mapping of the largest list split for which the predicate still holds. The split will always be after the first element.
mostMatching :: [a] -> (([a], [a]) -> b) -> (b -> Bool) -> Maybe b
mostMatching xs f p = beforeFirstFalse p $ map (f . (`splitAt` xs)) [1..length xs]


-- | Returns the first element for which the predicate returns True while returning False for the next element
beforeFirstFalse :: (a -> Bool) -> [a] -> Maybe a
beforeFirstFalse _ [] = Nothing
beforeFirstFalse p (x:xs)
  | p x = Just $ fromMaybe x (beforeFirstFalse p xs)
  | otherwise = Nothing
