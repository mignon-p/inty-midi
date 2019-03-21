module PreviewProgram (indent, mainProgram) where

import Data.Char
import Data.List
import Data.Maybe

indent :: String
indent = "    "

esc :: Char -> String
esc '\"' = "\\\""
esc '\\' = "\\\\"
-- GROM doesn't contain underscore, and title looks better with spaces anyway
esc '_' = " "
esc x
  | ord x > 127 || ord x < 32 = "?"
  | otherwise = [x]

escape :: String -> String
escape = concatMap esc

printCentered :: Int -> Int -> String -> String
printCentered y color str =
  let x = (20 - length str) `div` 2
      pos = y * 20 + max x 0
  in indent ++ "PRINT AT " ++ show pos ++ " COLOR " ++ show color ++ ", \"" ++ escape str ++ "\""

mainProgram :: String -> String -> [String]
mainProgram title label =
  [ i "CLS"
  , i "MODE 0, 1, 1, 1, 1"
  , i "BORDER 1"
  , i "WAIT"
  , ""
  , "RESTART:"
  , printCentered 3 5 "Press any button"
  , printCentered 4 5 "to play"
  , printCentered 6 7 title
  , ""
  , i "WHILE CONT = 0"
  , i $ i "WAIT"
  , i "WEND"
  , ""
  , i "FOR I = 60 TO 99"
  , i $ i "#BACKTAB(I) = 0"
  , i "NEXT I"
  , ""
  , i "WHILE CONT <> 0"
  , i $ i "WAIT"
  , i "WEND"
  , ""
  , i "PLAY FULL"
  , i $ "PLAY " ++ label
  , ""
  , i "WHILE MUSIC.PLAYING"
  , i $ i "WAIT"
  , i "WEND"
  , ""
  , i "GOTO RESTART"
  , ""
  , i $ "ASM CFGVAR \"name\" = \"" ++ escape title ++ "\""
  , ""
  ]
  where i = (indent ++)
