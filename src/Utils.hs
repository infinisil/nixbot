module Utils (prettySeconds) where

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
