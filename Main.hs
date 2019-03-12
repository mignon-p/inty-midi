module Main where

import Data.Char
import Data.List
import Data.Ord
import Data.Word
import System.Environment
import System.Exit
import System.IO
import ZMidi.Core.Canonical
import ZMidi.Core.Datatypes
import ZMidi.Core.ReadFile

type AbsMidiMessage = (Integer, MidiEvent)

indent :: String
indent = "    "

getMetaLines :: [AbsMidiMessage] -> [String]
getMetaLines [] = []
getMetaLines ((_, MetaEvent (TextEvent typ str)):rest) =
  trm (indent ++ "' " ++ show typ ++ ": " ++ str) : getMetaLines rest
  where trm = dropWhileEnd isSpace
getMetaLines (_:rest) = getMetaLines rest

getMusicLines :: [AbsMidiMessage] -> Either ErrMsg [String]
getMusicLines _ = Right ["TODO"]

absolutify :: Integer -> [MidiMessage] -> [AbsMidiMessage]
absolutify _ [] = []
absolutify now ((delta, ev):rest) =
  let next = now + fromIntegral delta
  in (next, ev) : absolutify next rest

combineTracks :: [MidiTrack] -> [AbsMidiMessage]
combineTracks =
  sortBy (comparing fst) . concatMap (absolutify 0 . getTrackMessages)

hasOnlyMetadata :: MidiTrack -> Bool
hasOnlyMetadata trk = onlyMeta $ getTrackMessages trk
  where onlyMeta [] = True
        onlyMeta ((_, MetaEvent {}):rest) = onlyMeta rest
        onlyMeta _ = False

convert' :: Word16 -> [MidiTrack] -> Either ErrMsg [String]
convert' tpb trks = do
  let (metadata, music) = partition hasOnlyMetadata trks
      mdTrk = combineTracks metadata
      musTrk = combineTracks music
      metaLines = getMetaLines mdTrk
  musicLines <- getMusicLines musTrk
  return $ metaLines ++ musicLines

convert :: MidiFile -> Either ErrMsg [String]
convert (MidiFile hdr trks) =
  case time_division hdr of
    TPB tpb -> convert' tpb trks
    _ -> Left "Time division not in ticks-per-beat"

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
