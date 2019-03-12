module Main where

import Data.Bits
import Data.Char
import Data.Containers.ListUtils
import Data.Int
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.Ord
import Data.Word
import System.Environment
import System.Exit
import System.IO
import ZMidi.Core.Canonical
import ZMidi.Core.Datatypes
import ZMidi.Core.ReadFile

type AbsTime = Word64
type Channel = Word8
type NoteValue = Int16
type AbsMidiMessage = (AbsTime, MidiEvent)
data Note = Note
  { nTime :: !AbsTime
  , nChan :: !Channel
  , nVal  :: !NoteValue
  } deriving (Eq, Ord, Show)
type ChannelSet = Word16
type ChannelMap = IM.IntMap Channel
type NoteMap = IM.IntMap String

indent :: String
indent = "    "

maxChannels :: Channel
maxChannels = 3

noteNames :: [String]
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

namesForOctave :: Int -> [String]
namesForOctave octave = map insertOct noteNames
  where octStr = show octave
        insertOct (letter:accidental) = letter : octStr ++ accidental

allNoteNames :: [String]
allNoteNames = concatMap namesForOctave [2..7]

noteList :: [(Int, String)]
noteList = zip [36..96] allNoteNames

noteMap :: NoteMap
noteMap = IM.fromList $ (-2, "S") : noteList

formatNote :: NoteValue -> String
formatNote nv = IM.findWithDefault "-" (fromIntegral nv) noteMap

formatLine :: [NoteValue] -> String
formatLine nvs = indent ++ "MUSIC " ++ intercalate "," (map formatNote nvs)

nvsFromPlaying :: ChannelSet -> [NoteValue]
nvsFromPlaying cs = map f [0 .. fromIntegral maxChannels - 1]
  where f n = if testBit cs n then (-2) else (-1)

playingFromNvs :: [NoteValue] -> ChannelSet
playingFromNvs [] = 0
playingFromNvs (nv:nvs) = f nv .|. (playingFromNvs nvs `shiftL` 1)
  where f (-1) = 0
        f _ = 1

setElem :: [a] -> Int -> a -> [a]
setElem xs idx x =
  let (before, (_:after)) = splitAt idx xs
  in before ++ x : after

mergeNotes :: [Note] -> [NoteValue] -> [NoteValue]
mergeNotes [] nvs = nvs
mergeNotes (note:rest) nvs = mergeNotes rest nvs'
  where nvs' = setElem nvs (fromIntegral $ nChan note) (nVal note)

convertNotes :: AbsTime -> [Note] -> ChannelSet -> [[NoteValue]]
convertNotes _ [] _ = []
convertNotes now notes playing =
  let (current, future) = partition isCurrent notes
      isCurrent note = nTime note == now
      nvs = mergeNotes current (nvsFromPlaying playing)
      playing' = playingFromNvs nvs
  in nvs : convertNotes (now + 1) future playing'

channelsUsed :: [Note] -> ChannelSet
channelsUsed = foldr addCh 0
  where addCh note acc = acc .|. bit (fromIntegral (nChan note))

channelList :: ChannelSet -> [Channel]
channelList cs = chkChan 0
  where chkChan 16 = []
        chkChan x =
          if testBit cs (fromIntegral x)
          then x : chkChan (x + 1)
          else chkChan (x + 1)

makeChannelMap :: ChannelSet -> ChannelMap
makeChannelMap cs =
  let clist = map fromIntegral $ channelList cs
      cmap = zip clist [0..maxChannels-1]
  in IM.fromList cmap

mapChannels :: ChannelMap -> [Note] -> [Note]
mapChannels _ [] = []
mapChannels cm (note:rest) =
  case IM.lookup (fromIntegral $ nChan note) cm of
    Just ch -> note { nChan = ch } : mapChannels cm rest
    _ -> mapChannels cm rest

divTime :: AbsTime -> [Note] -> [Note]
divTime divisor = map dt
  where dt note = note { nTime = nTime note `div` divisor }

findDivisor :: [Note] -> AbsTime
findDivisor notes =
  let times = nubOrd $ map nTime notes
  in foldr gcd 0 times

getNotes :: [AbsMidiMessage] -> [Note]
getNotes [] = []
getNotes ((t, VoiceEvent _ (NoteOn ch note vel)):rest) =
  let note' = if vel == 0 then (-1) else fromIntegral note
  in Note t ch note' : getNotes rest
getNotes ((t, VoiceEvent _ (NoteOff ch _ _)):rest) =
  Note t ch (-1) : getNotes rest
getNotes (_:rest) = getNotes rest

getMetaLines :: [AbsMidiMessage] -> [String]
getMetaLines [] = []
getMetaLines ((_, MetaEvent (TextEvent typ str)):rest) =
  trm (indent ++ "' " ++ show typ ++ ": " ++ str) : getMetaLines rest
  where trm = dropWhileEnd isSpace
getMetaLines (_:rest) = getMetaLines rest

getMusicLines :: [AbsMidiMessage] -> Either ErrMsg [String]
getMusicLines msgs =
  let notes = nubOrd $ getNotes msgs
      notes' = mapChannels (makeChannelMap (channelsUsed notes)) notes
      divisor = findDivisor notes'
      notes'' = divTime divisor notes'
      intyNotes = convertNotes 0 notes'' 0
  in Right $ map formatLine intyNotes

absolutify :: AbsTime -> [MidiMessage] -> [AbsMidiMessage]
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
