import Control.Monad
import Data.Char (ord)
import qualified Data.Text as T
import System.IO
import Text.XHtml (content)

encode :: String -> String -> IO ()
encode file1 file2 = do
  let list = []
  handle <- openFile file1 ReadMode
  contents <- hGetContents handle
  let singlewords = words contents
      list = f singlewords
  print list
  writeFile file2 (unwords list)
  hClose handle

f :: [String] -> [String]
f arr =
  let ultItem = last (last arr)
   in filter (not . null) $ map (elimChar ultItem) arr

elimChar :: Char -> String -> String
elimChar s [] = []
elimChar s (x : xs) = if s == x then elimChar s xs else x : elimChar s xs