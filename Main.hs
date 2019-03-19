module Main where

import Control.Monad
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
import Text.Read
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

data NoteLine = NoteLine
  { lNotes :: [NoteValue]
  , lDropped :: [NoteValue]
  } deriving (Eq, Show)

data Metadata = Metadata
  { mFilename :: String
  , mTimeDivision :: MidiTimeDivision
  , mTempo :: Maybe Word32
  , mTimeSig :: Maybe (Word8, Word8, Word8, Word8)
  , mSeqName :: Maybe String
  } deriving (Eq, Show)

data TheOptions = TheOptions
  { oMain :: Bool
  , oQuantize :: Int -- quantize to 1/oQuantize notes (e. g. 16 for 16th notes)
  , oArgs :: [String] -- non-option arguments
  , oErrors :: [String]
  } deriving (Eq, Show)

noteNames :: [String]
noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

namesForOctave :: Int -> [String]
namesForOctave octave = map insertOct noteNames
  where octStr = show octave
        insertOct (letter:accidental) = letter : octStr ++ accidental

allNoteNames :: [String]
allNoteNames = concatMap namesForOctave [-1..9]

noteList :: [(Int, String)]
noteList = zip [0..127] allNoteNames

-- range of MIDI notes supported by IntyBASIC
lowestNote, highestNote :: NoteValue
lowestNote = 36
highestNote = 96

noteMap :: NoteMap
noteMap = IM.fromList $ (-2, "S") : noteList

formatNote :: NoteValue -> String
formatNote nv = IM.findWithDefault "-" (fromIntegral nv) noteMap

nodrums :: [String] -> [String]
nodrums (a:b:c:rest@(_:_)) = a : b : c : "-" : rest
nodrums x = x

formatLine :: NoteLine -> String
formatLine (NoteLine { lNotes = nvs, lDropped = drp }) = stmt ++ comment
  where
    stmt = indent ++ "MUSIC " ++ intercalate "," (nodrums (map formatNote nvs))
    spaces = replicate (max 0 (40 - length stmt)) ' '
    comment = case drp of
                [] -> ""
                xs -> spaces ++ "' dropped " ++ intercalate ", " (map formatNote xs)

nvsFromPlaying' :: ChannelSet -> Int -> [NoteValue]
nvsFromPlaying' 0 _ = []
nvsFromPlaying' cs b =
  let cs' = clearBit cs b
      next = nvsFromPlaying' cs' (b + 1)
      x = if testBit cs b then (-2) else (-1)
  in x : next

nvsFromPlaying :: ChannelSet -> [NoteValue]
nvsFromPlaying cs = nvsFromPlaying' cs 0

playingFromNvs :: [NoteValue] -> ChannelSet
playingFromNvs [] = 0
playingFromNvs (nv:nvs) = f nv .|. (playingFromNvs nvs `shiftL` 1)
  where f (-1) = 0
        f _ = 1

setElem :: [NoteValue] -> Int -> NoteValue -> [NoteValue]
setElem xs idx x =
  case splitAt idx xs of
    (before, (_:after)) -> before ++ x : after
    _ -> take idx (xs ++ repeat (-1)) ++ [x]

mergeNotes :: [Note] -> [NoteValue] -> [NoteValue]
mergeNotes [] nvs = nvs
mergeNotes (note:rest) nvs = mergeNotes rest nvs'
  where nvs' = setElem nvs (fromIntegral $ nChan note) (nVal note)

noteTypeHack :: Note -> Note
noteTypeHack note@(Note {nType = On}) = note
noteTypeHack note = note { nVal = (-1) }

convertNotes :: AbsTime -> [Note] -> ChannelSet -> [NoteLine]
convertNotes _ [] _ = []
convertNotes now notes playing =
  let (current, future) = partition isCurrent notes
      isCurrent note = nTime note == now
      nvs = mergeNotes (map noteTypeHack current) (nvsFromPlaying playing)
      playing' = playingFromNvs nvs
      nl = NoteLine { lNotes = nvs, lDropped = [] }
  in nl : convertNotes (now + 1) future playing'

countVoices :: [NoteLine] -> Int
countVoices = maximum . map (length . lNotes)

padVoices' :: Int -> [NoteLine] -> [NoteLine]
padVoices' nVoices = map pad
  where pad nls =
          let nvs = lNotes nls
              len = length nvs
              dropped = filter (>= 0) (drop nVoices nvs)
          in if len <= nVoices
             then nls { lNotes = take nVoices $ nvs ++ repeat (-1) }
             else nls { lNotes = take nVoices nvs
                      , lDropped = dropped
                      }

padVoices :: [NoteLine] -> [NoteLine]
padVoices nls =
  let nVoices = countVoices nls
      nVoices' = if nVoices < 4 then 3 else 6
  in padVoices' nVoices' nls

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
      inRange = nVal note >= lowestNote && nVal note <= highestNote
  in if inRange
     then (usedVoices', currentNotes', newChan, True)
     else (usedVoices, currentNotes, 0, False) -- note out of range
remapNote usedVoices currentNotes note =  -- nType = Off
  let key = (nChan note, nVal note)
  in case M.lookup key currentNotes of
    Just newChan ->
      let usedVoices' = clearBit usedVoices (fromIntegral newChan)
          currentNotes' = M.delete key currentNotes
      in (usedVoices', currentNotes', newChan, True)
    _ -> (usedVoices, currentNotes, 0, False) -- unpaired note off

remapChannels' :: ChannelSet
               -> CurrentNoteMap
               -> [Note]
               -> [Note]
remapChannels' _ _ [] = []
remapChannels' usedVoices currentNotes (n:ns) =
  if ok
  then n' : remapChannels' usedVoices' currentNotes' ns
  else remapChannels' usedVoices' currentNotes' ns
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

getTicksPerQuarter :: MidiTimeDivision -> Word16
getTicksPerQuarter (TPB x) = x
getTicksPerQuarter _ = 384

computeTempo :: Metadata -> AbsTime -> (Int, Int)
computeTempo meta divisor =
  let tpq = getTicksPerQuarter $ mTimeDivision meta
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

rmExt :: String -> String
rmExt s =
  case dropWhile (/= '.') s of
    ('.':rest) -> rest
    _ -> s

determineTitle :: Metadata -> String
determineTitle meta = fromMaybe base (mSeqName meta)
  where base = reverse $ rmExt $ takeWhile notSlash $ reverse $ mFilename meta
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
      intyNotes = padVoices $ convertNotes 0 notes' 0
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

-- sort primarily by absolute time, but for message of equal time,
-- put NoteOn messages after the NoteOff messages, because NoteOff
-- may free up voices that can be used by NoteOn
absSortKey :: AbsMidiMessage -> (AbsTime, Bool)
absSortKey (t, (VoiceEvent _ (NoteOn _ _ 0))) = (t, False)
absSortKey (t, (VoiceEvent _ (NoteOff _ _ _))) = (t, False)
absSortKey (t, _) = (t, True)

combineTracks :: [MidiTrack] -> [AbsMidiMessage]
combineTracks =
  sortBy (comparing absSortKey) . concatMap (absolutify 0 . getTrackMessages)

quantize' :: AbsTime -> [AbsMidiMessage] -> [AbsMidiMessage]
quantize' quantum = map f
  where quantum' = fromIntegral quantum :: Double
        f (t, ev) = (quantum * round (fromIntegral t / quantum'), ev)

quantize :: TheOptions -> Metadata -> [AbsMidiMessage] -> [AbsMidiMessage]
quantize opts@(TheOptions { oQuantize = q }) meta msgs
  | q <= 0 = msgs
  | otherwise =
    let tpq = fromIntegral $ getTicksPerQuarter $ mTimeDivision meta
        ticksPerWhole = tpq * 4
        quantum = ticksPerWhole `div` fromIntegral q
    in quantize' quantum msgs

convert :: TheOptions -> FilePath -> MidiFile -> Either ErrMsg [String]
convert opts filename (MidiFile hdr trks) =
  case (trks, hdr_format hdr) of
    (_, MF2) -> Left "Format 2 MIDI files are not currently supported."
    ((trk1:_), _) ->
      let combined = combineTracks trks
          metaTrk = absolutify 0 $ getTrackMessages trk1
          meta = extractMetadata filename (time_division hdr) metaTrk
      in getMusicLines opts meta (quantize opts meta combined)
    _ -> Left "No tracks in file!"

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

extractOptions :: [String] -> TheOptions
extractOptions [] = TheOptions { oMain = False
                               , oQuantize = 0
                               , oArgs = []
                               , oErrors = []
                               }
extractOptions ("-m" : args) = (extractOptions args) { oMain = True }
extractOptions ("-q" : n : args) =
  case readMaybe n of
    Just n' -> (extractOptions args) { oQuantize = n' }
    _ -> let args' = extractOptions args
         in args' { oErrors = "Argument to -q must be an integer" : oErrors args' }
extractOptions (bad@('-':_):args) =
  let args' = extractOptions args
  in args' { oErrors = ("Unrecognized option " ++ bad) : oErrors args' }
extractOptions (arg:args) =
  let args' = extractOptions args
  in args' { oArgs = arg : oArgs args' }

main :: IO ()
main = do
  args <- getArgs
  let opts = extractOptions args
  case (oErrors opts, oArgs opts) of
    ([], [infile, outfile]) -> main' opts infile outfile
    (errs, _) -> do
      let e = hPutStrLn stderr
      forM_ errs e
      e "Usage: inty-midi [-m] [-q n] input.mid output.bas"
      e "    -m    include a main program in output"
      e "    -q n  quantize to 1/n notes (e. g. 16 for 16th notes)"
      exitFailure
