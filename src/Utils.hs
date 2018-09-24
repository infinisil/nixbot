module Utils (prettySeconds, mostMatching) where

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


-- | Returns the mapping of the largest list split for which the predicate still holds. The split will always be after the first element.
mostMatching :: [a] -> (([a], [a]) -> b) -> (b -> Bool) -> Maybe b
mostMatching xs f pred = beforeFirstFalse pred $ map (f . (`splitAt` xs)) [1..length xs]


-- | Returns the first element for which the predicate returns True while returning False for the next element
beforeFirstFalse :: (a -> Bool) -> [a] -> Maybe a
beforeFirstFalse _ [] = Nothing
beforeFirstFalse pred (x:xs)
  | pred x = Just $ fromMaybe x (beforeFirstFalse pred xs)
  | otherwise = Nothing
