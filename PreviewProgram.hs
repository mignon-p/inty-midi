module PreviewProgram (indent, mainProgram) where

import Data.Char

indent :: String
indent = "    "

asciiChars =
  [ "*..*..*."
  , ".*.*.*.."
  , "..***..."
  , "*******."
  , "..***..."
  , ".*.*.*.."
  , "*..*..*."
  , "........"

  , "...*...."
  , "..*.*..."
  , ".*...*.."
  , "........"
  , "........"
  , "........"
  , "........"
  , "........"

  , "........"
  , "........"
  , "........"
  , "........"
  , "........"
  , "........"
  , "........"
  , "********"
  ]

esc :: Char -> String
esc '\"' = "\\\""
esc '\\' = "\\\\"
esc '*' = "\\256"
esc '^' = "\\257"
esc '_' = "\\258"
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

asciiCards :: [String]
asciiCards = map f asciiChars
  where f x = indent ++ "BITMAP " ++ show x

mainProgram :: String -> String -> [String]
mainProgram title label =
  [ i "CLS"
  , i "MODE 0, 1, 1, 1, 1"
  , i "BORDER 1"
  , i "DEFINE 0, 3, ASCII_CHARS"
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
  , "ASCII_CHARS:"
  ] ++ asciiCards ++ [""]
  where i = (indent ++)
