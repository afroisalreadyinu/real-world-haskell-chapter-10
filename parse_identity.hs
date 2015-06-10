import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64           -- imported from Data.Int
    } deriving (Show)

newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of
        Left err          -> Left err
        Right (result, _) -> Right result

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))


main = do
  putStrLn (show (parse (identity "foo") undefined))
