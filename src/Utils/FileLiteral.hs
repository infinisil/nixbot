module Utils.FileLiteral (litFile) where

-- From https://stackoverflow.com/a/12717160/6605742

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

literally :: String -> Q Exp
literally = return . LitE . StringL

lit :: QuasiQuoter
lit = QuasiQuoter { quoteExp = literally }

litFile :: QuasiQuoter
litFile = quoteFile lit
