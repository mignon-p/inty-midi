module Main where

import System.Environment
import System.Exit
import System.IO
import ZMidi.Core.Canonical
import ZMidi.Core.Datatypes
import ZMidi.Core.ReadFile

convert :: MidiFile -> Either ErrMsg [String]
convert (MidiFile hdr trks) = Right $ map show (getTrackMessages $ trks !! 0)

main' :: FilePath -> IO ()
main' filename = do
  eth <- readMidi filename
  case eth of
    Left (ParseErr _ msg) -> do
      hPutStrLn stderr $ "error parsing " ++ filename ++ ": " ++ msg
      exitFailure
    Right midifile -> do
      let eth' = convert $ canonical midifile
      case eth' of
        Left msg -> do
          hPutStrLn stderr $ "error in " ++ filename ++ ": " ++ msg
          exitFailure
        Right lns -> putStr $ unlines lns

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> main' filename
    _ -> do
      hPutStrLn stderr "Usage: inty-midi file.mid"
      exitFailure
