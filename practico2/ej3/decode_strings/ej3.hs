{-# LANGUAGE ViewPatterns #-}

import Data.Char (ord, toLower, toUpper)
import qualified Data.Text as T
import Text.XHtml (content)

encode :: String -> String -> IO ()
encode file1 file2 = do
  text <- readFile file1
  let ultChar = last text
  writeFile file2 $ ultChar : convertirChar ultChar 0 (reverse (map toUpper (elimNotAscii (init text))))
  return ()

encode' :: String -> String -> IO ()
encode' file1 file2 = do
  text <- readFile file1
  let new_text = T.pack text
  let ultChar = T.last new_text
  writeFile file2 $ (T.unpack . T.cons ultChar . snd . T.mapAccumL (\pos x -> (pos + 1, moduloToChar (fromEnum x + pos + fromEnum ultChar) 256)) 0 . T.reverse . T.toUpper . T.filter (\x -> fromEnum x <= 255) . T.init) new_text
  return ()

decode :: String -> String -> IO ()
decode file1 file2 = do
  text <- readFile file1
  let key_char = head text
  writeFile file2 (map toLower (reverse (desconvertirChar key_char 0 (tail text))) ++ [key_char])
  return ()

decode' :: String -> String -> IO ()
decode' file1 file2 = do
  text <- readFile file1
  let new_text = T.pack text
  let key_char = T.head new_text
  writeFile file2 $ (T.unpack . T.toLower . T.reverse . snd . T.mapAccumL (\pos x -> (pos + 1, moduloToChar (fromEnum x - pos - fromEnum key_char) 256)) 0 . T.tail) new_text ++ [key_char]
  return ()

elimNotAscii :: String -> String
elimNotAscii = foldr append []
  where
    append x acc = if fromEnum x > 255 then acc else x : acc

convertirChar :: Char -> Int -> String -> String
convertirChar _ _ [] = []
convertirChar char_elim pos (c : cs) = moduloToChar (fromEnum char_elim + pos + fromEnum c) 256 : convertirChar char_elim (pos + 1) cs

desconvertirChar :: Char -> Int -> String -> String
desconvertirChar _ _ [] = []
desconvertirChar key_char pos (c : cs) = moduloToChar (fromEnum c - fromEnum key_char - pos) 256 : desconvertirChar key_char (pos + 1) cs

moduloToChar :: Int -> Int -> Char
moduloToChar a b = toEnum (a `mod` b)

main = decode "encoded.txt" "decoded.txt"