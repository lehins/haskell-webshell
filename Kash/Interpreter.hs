module Kash.Interpreter (
  tToHtml
  ) where
import Kash.Parser
import Data.ByteString hiding (concat)
-- import Prelude

tToHtml s = pack $ concat [fil x | x <- parsed] where
  parsed = case tParse s of
    (Left _) -> []
    (Right p) -> p
  fil (KString str) = unpack str
  fil _ = unpack empty