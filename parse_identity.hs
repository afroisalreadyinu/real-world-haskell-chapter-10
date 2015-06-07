import PGM3 (ParseState(..), Parse(..))
import qualified Data.ByteString.Lazy as L

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of
        Left err          -> Left err
        Right (result, _) -> Right result

main = do
  putStrLn (show (parse (identity "foo") undefined))
