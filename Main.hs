module Main where

import Data.Bits
import Data.Char
import Data.Containers.ListUtils
import Data.Int
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.Word
import System.Environment
import System.Exit
import System.IO
import ZMidi.Core.Canonical
import ZMidi.Core.Datatypes
import ZMidi.Core.ReadFile

import PreviewProgram

type AbsTime = Word64
type Channel = Word8
type NoteValue = Int16
type AbsMidiMessage = (AbsTime, MidiEvent)

data NoteType = Off | On deriving (Eq, Ord, Show)

data Note = Note
  { nTime :: !AbsTime
  , nChan :: !Channel
  , nVal  :: !NoteValue
  , nType :: !NoteType
  } deriving (Eq, Ord, Show)

type ChannelSet = Word16
type ChannelMap = IM.IntMap Channel
type NoteMap = IM.IntMap String
type CurrentNoteMap = M.Map (Channel, NoteValue) Channel

data Metadata = Metadata
  { mFilename :: String
  , mTimeDivision :: MidiTimeDivision
  , mTempo :: Maybe Word32
  , mTimeSig :: Maybe (Word8, Word8, Word8, Word8)
  , mSeqName :: Maybe String
  } deriving (Eq, Show)

data TheOptions = TheOptions
  { oMain :: Bool
  } deriving (Eq, Show)

maxChannels :: Channel
maxChannels = 6

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

nodrums :: [String] -> [String]
nodrums (a:b:c:rest) = a : b : c : "-" : rest
nodrums x = x

formatLine :: [NoteValue] -> String
formatLine nvs =
  indent ++ "MUSIC " ++ intercalate "," (nodrums (map formatNote nvs))

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

noteTypeHack :: Note -> Note
noteTypeHack note@(Note {nType = On}) = note
noteTypeHack note = note { nVal = (-1) }

convertNotes :: AbsTime -> [Note] -> ChannelSet -> [[NoteValue]]
convertNotes _ [] _ = []
convertNotes now notes playing =
  let (current, future) = partition isCurrent notes
      isCurrent note = nTime note == now
      nvs = mergeNotes (map noteTypeHack current) (nvsFromPlaying playing)
      playing' = playingFromNvs nvs
  in nvs : convertNotes (now + 1) future playing'

{-

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

-}

findUnusedVoice :: ChannelSet -> Channel
findUnusedVoice cs = fu 0
  where fu 15 = 15
        fu n = if testBit cs (fromIntegral n)
               then fu (n + 1)
               else n

remapNote :: ChannelSet
          -> CurrentNoteMap
          -> Note
          -> (ChannelSet, CurrentNoteMap, Channel, Bool)
remapNote usedVoices currentNotes note@(Note { nType = On }) =
  let newChan = findUnusedVoice usedVoices
      usedVoices' = setBit usedVoices (fromIntegral newChan)
      currentNotes' = M.insert key newChan currentNotes
      key = (nChan note, nVal note)
  in (usedVoices', currentNotes', newChan, True)
remapNote usedVoices currentNotes note =  -- nType = Off
  let key = (nChan note, nVal note)
  in case M.lookup key currentNotes of
    Just newChan ->
      let usedVoices' = clearBit usedVoices (fromIntegral newChan)
          currentNotes' = M.delete key currentNotes
      in (usedVoices', currentNotes', newChan, True)
    _ -> (usedVoices, currentNotes, 0, False)

remapChannels' :: ChannelSet
               -> CurrentNoteMap
               -> [Note]
               -> [Note]
remapChannels' _ _ [] = []
remapChannels' usedVoices currentNotes (n:ns) =
  if ok
  then n' : remapChannels' usedVoices' currentNotes' ns
  else remapChannels' usedVoices' currentNotes' ns -- unpaired note off
  where n' = n { nChan = newChannel }
        (usedVoices', currentNotes', newChannel, ok) =
          remapNote usedVoices currentNotes n

remapChannels :: [Note] -> [Note]
remapChannels = remapChannels' 0 M.empty

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
  let typ = if vel == 0 then Off else On
  in Note t ch (fromIntegral note) typ : getNotes rest
getNotes ((t, VoiceEvent _ (NoteOff ch note _)):rest) =
  Note t ch (fromIntegral note) Off : getNotes rest
getNotes (_:rest) = getNotes rest

computeTempo' :: Double -> Double -> Double -> Double -> Double -> (Double, Double)
computeTempo' microsPerQuarter ticksPerQuarter ticksPerLine beatsPerMeasure beatsPerWholeNote =
  let microsPerTick = microsPerQuarter / ticksPerQuarter
      secondsPerTick = microsPerTick / 1e6
      secondsPerLine = secondsPerTick * ticksPerLine
      intyUnitsPerLine = secondsPerLine * 50
      beatsPerQuarter = beatsPerWholeNote / 4
      linesPerQuarter = ticksPerQuarter / ticksPerLine
      beatsPerLine = beatsPerQuarter / linesPerQuarter
      linesPerMeasure = beatsPerMeasure / beatsPerLine
  in (intyUnitsPerLine, linesPerMeasure)

computeTempo :: Metadata -> AbsTime -> (Int, Int)
computeTempo meta divisor =
  let tpq = case mTimeDivision meta of
              TPB x -> x
              _ -> 384
      uspq = fromMaybe 500000 (mTempo meta)
      (num, denom, _, _) = fromMaybe (4, 2, 0, 0) (mTimeSig meta)
      (intyTempo, linesPerMeasure) = computeTempo' (fromIntegral uspq) (fromIntegral tpq) (fromIntegral divisor) (fromIntegral num) (fromIntegral $ 2 ^ denom)
  in (round intyTempo, round linesPerMeasure)

handleMetaEvent :: Metadata -> MidiMetaEvent -> Metadata
handleMetaEvent md (TextEvent SEQUENCE_NAME name) =
  md { mSeqName = Just $ fromMaybe name (mSeqName md) }
handleMetaEvent md (SetTempo tempo) =
  md { mTempo = Just $ fromMaybe tempo (mTempo md) }
handleMetaEvent md (TimeSignature w x y z) =
  md { mTimeSig = Just $ fromMaybe (w, x, y, z) (mTimeSig md) }
handleMetaEvent md _ = md

extractMetadata' :: Metadata -> [AbsMidiMessage] -> Metadata
extractMetadata' md ((_, MetaEvent mev):rest) =
  extractMetadata' (handleMetaEvent md mev) rest
extractMetadata' md (_:rest) = extractMetadata' md rest
extractMetadata' md _ = md

extractMetadata :: FilePath -> MidiTimeDivision -> [AbsMidiMessage] -> Metadata
extractMetadata filename timeDiv =
  extractMetadata' $ Metadata filename timeDiv Nothing Nothing Nothing

insertBlankLines :: Int -> [String] -> [String]
insertBlankLines _ [] = []
insertBlankLines lpm lns =
  let (x, y) = splitAt lpm lns
  in x ++ [""] ++ insertBlankLines lpm y

determineTitle :: Metadata -> String
determineTitle meta = fromMaybe base (mSeqName meta)
  where base = reverse $ takeWhile notSlash $ reverse $ mFilename meta
        notSlash '/' = False
        notSlash '\\' = False
        notSlash _ = True

labelFromTitle :: String -> String
labelFromTitle = map f
  where f x = if isAlphaNum x then x else '_'

getMusicLines :: TheOptions -> Metadata -> [AbsMidiMessage] -> Either ErrMsg [String]
getMusicLines opts meta msgs =
  let notes = remapChannels $ nubOrd $ getNotes msgs
      divisor = findDivisor notes
      notes' = divTime divisor notes
      intyNotes = convertNotes 0 notes' 0
      (intyTempo, linesPerMeasure) = computeTempo meta divisor
      title = determineTitle meta
      label = labelFromTitle title
      labelLine = label ++ ":"
      tempoLine = indent ++ "DATA " ++ show intyTempo
      endLine = indent ++ "MUSIC STOP"
      mainLines = if (oMain opts)
                  then mainProgram title label
                  else []
  in Right $ mainLines ++ labelLine : tempoLine : "" : insertBlankLines linesPerMeasure (map formatLine intyNotes) ++ [endLine]

absolutify :: AbsTime -> [MidiMessage] -> [AbsMidiMessage]
absolutify _ [] = []
absolutify now ((delta, ev):rest) =
  let next = now + fromIntegral delta
  in (next, ev) : absolutify next rest

combineTracks :: [MidiTrack] -> [AbsMidiMessage]
combineTracks =
  sortBy (comparing fst) . concatMap (absolutify 0 . getTrackMessages)

convert :: TheOptions -> FilePath -> MidiFile -> Either ErrMsg [String]
convert opts filename (MidiFile hdr trks) =
  let combined = combineTracks trks
      meta = extractMetadata filename (time_division hdr) combined
  in getMusicLines opts meta combined

main' :: TheOptions -> FilePath -> FilePath -> IO ()
main' opts infile outfile = do
  eth <- readMidi infile
  case eth of
    Left (ParseErr _ msg) -> do
      hPutStrLn stderr $ "error parsing " ++ infile ++ ": " ++ msg
      exitFailure
    Right midifile -> do
      let eth' = convert opts infile $ canonical midifile
      case eth' of
        Left msg -> do
          hPutStrLn stderr $ "error in " ++ infile ++ ": " ++ msg
          exitFailure
        Right lns -> writeFile outfile $ unlines lns

extractOptions :: [String] -> (TheOptions, [String])
extractOptions ("-m" : args) = (TheOptions True, args)
extractOptions args = (TheOptions False, args)

main :: IO ()
main = do
  args <- getArgs
  let (opts, args') = extractOptions args
  case args' of
    [infile, outfile] -> main' opts infile outfile
    _ -> do
      hPutStrLn stderr "Usage: inty-midi [-m] input.mid output.bas"
      hPutStrLn stderr "    -m  include a main program in output"
      exitFailure
