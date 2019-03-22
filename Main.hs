module Main where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Containers.ListUtils
import Data.Either
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
type Program = Word8
type NoteValue = Int16
type AbsMidiMessage = (AbsTime, MidiEvent)

data Instrument = Piano | Clarinet | Flute | Bass deriving (Eq, Ord, Show)

data NoteType = Off | On !Instrument | DrumOff | DrumOn | Dropped
  deriving (Eq, Ord, Show)

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
type InstMap = IM.IntMap Instrument

data NoteLine = NoteLine
  { lNotes :: [NoteValue]
  , lInsts :: [Instrument]
  , lDrums :: [NoteValue]
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
  , oPickup :: Double -- number of quarter notes in first measure
  , oQuantize :: Int -- quantize to 1/oQuantize notes (e. g. 16 for 16th notes)
  , oArgs :: [String] -- non-option arguments
  , oErrors :: [String]
  } deriving (Eq, Show)

drumChannel :: Channel
drumChannel = 9 -- channel 10, converted to 0-based number

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

formatDrum :: [NoteValue] -> String
formatDrum [] = "-"
formatDrum ((-2):_) = "-"
formatDrum _ = "M1"

addDrums :: String -> [String] -> [String]
addDrums drums (a:b:c:rest) = a : b : c : drums : rest
addDrums _ x = x -- shouldn't happen

formatLine :: NoteLine -> String
formatLine (NoteLine { lNotes = nvs, lDrums = drms, lDropped = drp }) =
  stmt ++ comment
  where
    notes = map formatNote nvs
    drums = formatDrum drms
    stmt = indent ++ "MUSIC " ++ intercalate "," (addDrums drums notes)
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

-- Sets element idx of list xs to x.  If idx is greater than
-- list length, fills unused indices with dflt.
setElem :: a -> [a] -> Int -> a -> [a]
setElem dflt xs idx x =
  case splitAt idx xs of
    (before, (_:after)) -> before ++ x : after
    _ -> take idx (xs ++ repeat dflt) ++ [x]

mergeNotes :: [Note] -> [NoteValue] -> [NoteValue]
mergeNotes [] nvs = nvs
mergeNotes (note:rest) nvs = mergeNotes rest nvs'
  where nvs' = setElem (-1) nvs (fromIntegral $ nChan note) (nVal note)

mergeInsts :: [Note] -> [Instrument] -> [Instrument]
mergeInsts [] insts = insts
mergeInsts (note@(Note { nType = On inst }):rest) insts = mergeInsts rest insts'
  where insts' = setElem Piano insts (fromIntegral $ nChan note) inst
mergeInsts (_:rest) insts = mergeInsts rest insts

noteTypeHack :: Note -> Either Note Note
noteTypeHack note@(Note {nType = On _}) = Right note
noteTypeHack note@(Note {nType = Off}) = Right $ note { nVal = (-1) }
noteTypeHack note = Left note

handleDrums :: Bool -> [Note] -> ([NoteValue], Bool)
handleDrums drumsPlaying notes =
  case (find isOn notes, notes, drumsPlaying) of
    (Just n, _, _) -> ([nVal n], True)
    (_, [], True) -> ([(-2)], True)
    (_, [], False) -> ([], False)
    _ -> ([(-1)], False)
  where isOn (Note { nType = DrumOn }) = True
        isOn _ = False

isDrum :: Note -> Bool
isDrum (Note { nType = DrumOff }) = True
isDrum (Note { nType = DrumOn }) = True
isDrum _ = False

convertNotes :: AbsTime
             -> [Note]
             -> (ChannelSet, Bool)
             -> [Instrument]
             -> [NoteLine]
convertNotes _ [] _ _ = []
convertNotes now notes (playing, drumsPlaying) insts =
  let (current, future) = partition isCurrent notes
      isCurrent note = nTime note == now
      drums = filter isDrum current
      notDrums = filter (not . isDrum) current
      (dropped, hacked) = partitionEithers $ map noteTypeHack notDrums
      (drumVals, drumsPlaying') = handleDrums drumsPlaying drums
      nvs = mergeNotes hacked (nvsFromPlaying playing)
      insts' = mergeInsts notDrums insts
      playing' = playingFromNvs nvs
      nl = NoteLine { lNotes = nvs,
                      lInsts = insts',
                      lDrums = drumVals,
                      lDropped = nub $ map nVal dropped }
  in nl : convertNotes (now + 1) future (playing', drumsPlaying') insts'

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
                      , lDropped = (lDropped nls) ++ dropped
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
          -> (ChannelSet, CurrentNoteMap, Note, Bool)
remapNote usedVoices currentNotes note@(Note { nType = On _ }) =
  let newChan = findUnusedVoice usedVoices
      usedVoices' = setBit usedVoices (fromIntegral newChan)
      currentNotes' = M.insert key newChan currentNotes
      key = (nChan note, nVal note)
      inRange = nVal note >= lowestNote && nVal note <= highestNote
  in if inRange
     then (usedVoices', currentNotes', note { nChan = newChan }, True)
     else (usedVoices, currentNotes, note { nChan = 0, nType = Dropped }, True) -- note out of range
remapNote usedVoices currentNotes note@(Note { nType = Off }) =
  let key = (nChan note, nVal note)
  in case M.lookup key currentNotes of
    Just newChan ->
      let usedVoices' = clearBit usedVoices (fromIntegral newChan)
          currentNotes' = M.delete key currentNotes
      in (usedVoices', currentNotes', note { nChan = newChan }, True)
    _ -> (usedVoices, currentNotes, note { nChan = 0 } , False) -- unpaired note off
remapNote usedVoices currentNotes note =
  (usedVoices, currentNotes, note, True) -- drums

remapChannels' :: ChannelSet
               -> CurrentNoteMap
               -> [Note]
               -> [Note]
remapChannels' _ _ [] = []
remapChannels' usedVoices currentNotes (n:ns) =
  if ok
  then n' : remapChannels' usedVoices' currentNotes' ns
  else remapChannels' usedVoices' currentNotes' ns
  where (usedVoices', currentNotes', n', ok) =
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

bass_lo, bass_hi, reed_lo, reed_hi, pipe_lo, pipe_hi :: Program
bass_lo = 33
bass_hi = 40
reed_lo = 65
reed_hi = 72
pipe_lo = 73
pipe_hi = 80

-- takes a 1-based (not 0-based) program number, as given on:
-- https://en.wikipedia.org/wiki/General_MIDI#Program_change_events
instFromProg :: Program -> Instrument
instFromProg prog
  | prog >= bass_lo && prog <= bass_hi = Bass
  | prog >= reed_lo && prog <= reed_hi = Clarinet
  | prog >= pipe_lo && prog <= pipe_hi = Flute
  | otherwise = Piano

getNotes :: InstMap -> [AbsMidiMessage] -> [Note]
getNotes _ [] = []
getNotes im ((t, VoiceEvent _ (NoteOn ch note vel)):rest)
  | ch == drumChannel =
    let typ = if vel == 0 then DrumOff else DrumOn
    in Note t ch (fromIntegral note) typ : getNotes im rest
  | otherwise =
    let typ = if vel == 0 then Off else On inst
        inst = IM.findWithDefault Piano (fromIntegral ch) im
    in Note t ch (fromIntegral note) typ : getNotes im rest
getNotes im ((t, VoiceEvent _ (NoteOff ch note _)):rest)
  | ch == drumChannel =
    Note t ch (fromIntegral note) DrumOff : getNotes im rest
  | otherwise =
    Note t ch (fromIntegral note) Off : getNotes im rest
getNotes im ((t, VoiceEvent _ (ProgramChange ch prog)):rest) =
  let im' = IM.insert (fromIntegral ch) (instFromProg (prog + 1)) im
  in getNotes im' rest
getNotes im (_:rest) = getNotes im rest

computeTempo' :: Double
              -> Double
              -> Double
              -> Double
              -> Double
              -> Double
              -> (Double, Double, Double)
computeTempo' microsPerQuarter ticksPerQuarter ticksPerLine beatsPerMeasure beatsPerWholeNote pickupQuarters =
  let microsPerTick = microsPerQuarter / ticksPerQuarter
      secondsPerTick = microsPerTick / 1e6
      secondsPerLine = secondsPerTick * ticksPerLine
      intyUnitsPerLine = secondsPerLine * 50
      beatsPerQuarter = beatsPerWholeNote / 4
      linesPerQuarter = ticksPerQuarter / ticksPerLine
      beatsPerLine = beatsPerQuarter / linesPerQuarter
      linesPerMeasure = beatsPerMeasure / beatsPerLine
      pickupLines = if pickupQuarters > 0
                    then pickupQuarters * linesPerQuarter
                    else linesPerMeasure
  in (intyUnitsPerLine, pickupLines, linesPerMeasure)

getTicksPerQuarter :: MidiTimeDivision -> Word16
getTicksPerQuarter (TPB x) = x
getTicksPerQuarter _ = 384

computeTempo :: TheOptions -> Metadata -> AbsTime -> (Int, Int, Int)
computeTempo opts meta divisor =
  let tpq = getTicksPerQuarter $ mTimeDivision meta
      uspq = fromMaybe 500000 (mTempo meta)
      (num, denom, _, _) = fromMaybe (4, 2, 0, 0) (mTimeSig meta)
      (intyTempo, pickup, linesPerMeasure) =
        computeTempo' (fromIntegral uspq) (fromIntegral tpq) (fromIntegral divisor) (fromIntegral num) (fromIntegral $ 2 ^ denom) (oPickup opts)
  in (round intyTempo, round pickup, round linesPerMeasure)

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

insertBlankLines :: Int -> Int -> [String] -> [String]
insertBlankLines _ _ [] = []
insertBlankLines pickup lpm lns =
  let (x, y) = splitAt pickup lns
  in x ++ [""] ++ insertBlankLines lpm lpm y

rmExt :: String -> String
rmExt s =
  case dropWhile (/= '.') s of
    ('.':rest) -> rest
    _ -> s

determineTitle :: Metadata -> String
-- the metadata is too unreliable; we end up with names like "track 0"
-- determineTitle meta = fromMaybe base (mSeqName meta)
determineTitle meta = base
  where base = reverse $ rmExt $ takeWhile notSlash $ reverse $ mFilename meta
        notSlash '/' = False
        notSlash '\\' = False
        notSlash _ = True

dedupUnderscores :: String -> String
dedupUnderscores "" = ""
dedupUnderscores ('_':xs) =
  case dedupUnderscores xs of
    rest@('_':_) -> rest
    rest -> '_':rest
dedupUnderscores (x:xs) = x : dedupUnderscores xs

labelFromTitle :: String -> String
labelFromTitle = dedupUnderscores . map f
  where f x = if isAlphaNum x then x else '_'

getMusicLines :: TheOptions -> Metadata -> [AbsMidiMessage] -> Either ErrMsg [String]
getMusicLines opts meta msgs =
  let notes = remapChannels $ nubOrd $ getNotes IM.empty msgs
      divisor = findDivisor notes
      notes' = divTime divisor notes
      intyNotes = padVoices $ convertNotes 0 notes' (0, False) []
      (intyTempo, pickup, linesPerMeasure) = computeTempo opts meta divisor
      title = determineTitle meta
      label = labelFromTitle title
      labelLine = label ++ ":"
      tempoLine = indent ++ "DATA " ++ show intyTempo
      endLine = indent ++ "MUSIC STOP"
      mainLines = if (oMain opts)
                  then mainProgram title label
                  else []
      lns = insertBlankLines pickup linesPerMeasure (map formatLine intyNotes)
  in if intyTempo < 1
     then Left "Needs quantization (try \"-q 16\")"
     else Right $ mainLines ++ labelLine : tempoLine : "" : lns ++ [endLine]

absolutify :: AbsTime -> [MidiMessage] -> [AbsMidiMessage]
absolutify _ [] = []
absolutify now ((delta, ev):rest) =
  let next = now + fromIntegral delta
  in (next, ev) : absolutify next rest

-- sort primarily by absolute time, but for messages of equal time,
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
                               , oPickup = 0
                               , oQuantize = 0
                               , oArgs = []
                               , oErrors = []
                               }
extractOptions ("-m" : args) = (extractOptions args) { oMain = True }
extractOptions ("-p" : p : args) =
  case readMaybe p of
    Just p' -> (extractOptions args) { oPickup = p' }
    _ -> let args' = extractOptions args
         in args' { oErrors = "Argument to -p must be an number (floating point OK)" : oErrors args' }
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
      e "    -p n  number of quarter notes in first measure (can be fractional)"
      e "    -q n  quantize to 1/n notes (e. g. 16 for 16th notes)"
      exitFailure
