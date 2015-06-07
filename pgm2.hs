import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import System.Environment
import Common (Greymap(..))

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
| otherwise
    = Nothing

-- "nat" here is short for "natural number"
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
         Nothing -> Nothing
         Just (num,rest)
             | num <= 0    -> Nothing
             | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString
     -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count           = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v

parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
    matchHeader (L8.pack "P5") s      >>?
    \s -> skipSpace ((), s)           >>?
    (getNat . snd)                    >>?
    skipSpace                         >>?
    \(width, s) ->   getNat s         >>?
    skipSpace                         >>?
    \(height, s) ->  getNat s         >>?
    \(maxGrey, s) -> getBytes 1 s     >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)


parseP5_take2_truthful :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2_truthful s =
    matchHeader (L8.pack "P5") s      >>?
    \s -> skipSpace ((), s)           >>?
      (getNat . snd)                    >>?
      skipSpace                         >>?
      \(width, s) ->   getNat s         >>?
        skipSpace                         >>?
        \(height, s) ->  getNat s         >>?
          \(maxGrey, s) -> getBytes 1 s     >>?
            (getBytes (width * height) . snd) >>?
            \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

main :: IO ()
main = do
  args <- getArgs
  contents <- L8.readFile (head args)
  let parse_result = parseP5_take2 contents
  case parse_result of
    Nothing -> putStrLn "Error parsing file"
    Just (greymap, rest) -> putStrLn (show greymap)
  return ()
