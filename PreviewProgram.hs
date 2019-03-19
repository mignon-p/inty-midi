module PreviewProgram (indent, mainProgram) where

import Data.Char
import Data.List
import Data.Maybe

indent :: String
indent = "    "

asciiChars :: [(Char, [String])]
asciiChars =
  [ ('*', [ "*..*..*."
          , ".*.*.*.."
          , "..***..."
          , "*******."
          , "..***..."
          , ".*.*.*.."
          , "*..*..*."
          , "........"
          ])
  , ('^', [ "...*...."
          , "..*.*..."
          , ".*...*.."
          , "........"
          , "........"
          , "........"
          , "........"
          , "........"
          ])
  , ('_', [ "........"
          , "........"
          , "........"
          , "........"
          , "........"
          , "........"
          , "........"
          , "********"
          ])
  ]

needChars :: String -> String
needChars = nub . filter f
  where f x = lookup x asciiChars /= Nothing

esc' :: Char -> String
esc' '\"' = "\\\""
esc' '\\' = "\\\\"
esc' x
  | ord x > 127 || ord x < 32 = "?"
  | otherwise = [x]

esc :: String -> Int -> Char -> String
esc [] _ c = esc' c
esc (x:xs) n c = if x == c
                 then "\\" ++ show n
                 else esc xs (n + 1) c

escape :: String -> String -> String
escape gramChars = concatMap (esc gramChars 256)

printCentered :: String -> Int -> Int -> String -> String
printCentered gramChars y color str =
  let x = (20 - length str) `div` 2
      pos = y * 20 + max x 0
  in indent ++ "PRINT AT " ++ show pos ++ " COLOR " ++ show color ++ ", \"" ++ escape gramChars str ++ "\""

asciiCards :: String -> [String]
asciiCards [] = []
asciiCards gramChars = "" : "ASCII_CHARS:" : map f chrs
  where f x = indent ++ "BITMAP " ++ show x
        chrs = concatMap g gramChars
        g c = fromMaybe [] $ lookup c asciiChars

defineStmt :: String -> String
defineStmt [] = ""
defineStmt gramChars =
  indent ++ "DEFINE 0, " ++ show (length gramChars) ++ ", ASCII_CHARS"

mainProgram :: String -> String -> [String]
mainProgram title label =
  [ i "CLS"
  , i "MODE 0, 1, 1, 1, 1"
  , i "BORDER 1"
  , defineStmt gramChars
  , i "WAIT"
  , ""
  , "RESTART:"
  , printCentered gramChars 3 5 "Press any button"
  , printCentered gramChars 4 5 "to play"
  , printCentered gramChars 6 7 title
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
  , i $ "ASM CFGVAR \"name\" = " ++ show title
  ] ++ asciiCards gramChars ++ [""]
  where i = (indent ++)
        gramChars = needChars title
