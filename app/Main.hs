#!/usr/bin/env cabal
{- cabal:
build-depends: base ^>= 4.18.2.0
             , containers
             , bluefin >= 0.0.14
             , cassava
             , vector
             , text
             , bytestring
             , time
default-language: GHC2021
-}
{- project:
allow-newer: all:^base
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Bluefin.Eff
import Bluefin.Compound
import Bluefin.State
import Bluefin.StateSource
import Bluefin.Reader
import Bluefin.Writer
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector)
import Data.Time
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Function
import Data.Ord
import Data.List
import Data.Foldable
import Control.Monad
import Data.Maybe
import System.Environment

-- Constants defined across this file which can be adjusted to tune the
-- system:
--
-- - ptbFactor: Conversion between real and stunts minutes.
-- - ptbEarningRange: Positions to consider for PTB.
-- - chosenResolution: How to round earned minutes.
-- - grantPlusHalf: Whether to have PTB +0.5.
-- - hoursForPlusOne: Hours for PTB +1 (the base earning rate).
--
-- Ideally, all of them would be proper configurable parameters.

-- The window in a race within which replays count for PTB.
data BonusWindow = BonusWindow
    { windowStart :: !LocalTime
    , quietDaysStart :: !LocalTime
    }
    deriving Show

-- Player and track identifiers. Mere synonyms of Text for now.
type Player = Text
type Track = Text

-- Sets up bonus windows from a CSV with fields:
-- track name, season, official start day, deadline day.
-- Note that the season field is unused for now. Quiet days are assumed
-- to begin two days before the deadline.
readBonusWindows
    :: FilePath
    -> IO (Map Track BonusWindow)
readBonusWindows csvPath = do
    csvData <- BL.readFile csvPath
    pure $ case Csv.decode Csv.HasHeader csvData of
        Left err -> error err
        Right v -> v & foldl'
            (\ws (name, _ :: String, start, end) ->
                Map.insert name BonusWindow
                    { windowStart = LocalTime (read start) midnight
                    -- Subtracting one day from 00:00 at the deadline
                    -- gives 00:00 at the beginning of quiet days.
                    , quietDaysStart = addLocalTime (-nominalDay)
                        (LocalTime (read end) midnight)
                    }
                    ws)
            Map.empty

-- Various time counts expressed as integers. Just synonyms of Int for
-- now.
type RMin = Int  -- Real minutes.
type SMin = Int  -- Stunts Minutes.
type Credit = Int  -- PTB credit.
type Hsec = Int  -- Hundredths of a second (for lap times)

-- Relevant information about a replay.
data ReplayInfo = ReplayInfo
    { replayTrack :: !Track
    , replayPlayer :: !Player
    , correctedHsec :: !Hsec
    , submittedOn :: !LocalTime
    , visibility :: !Text
    }
    deriving Show

-- Compares replays according to lap time, using submission time as
-- tiebreaker.
compareForScoreboard :: ReplayInfo -> ReplayInfo -> Ordering
compareForScoreboard rix riy =
    comparing correctedHsec rix riy <> comparing submittedOn rix riy

-- Reads replays from a CSV with fields matching those of ReplayInfo.
readAllReplays
    :: FilePath
    -> IO (Vector ReplayInfo)
readAllReplays csvPath = do
    csvData <- BL.readFile csvPath
    pure $ case Csv.decode Csv.HasHeader csvData of
        Left err -> error err
        Right v -> v & fmap (\(track, name, hsec, sbmtOn, vis) ->
            ReplayInfo
                { replayTrack = track
                , replayPlayer = name
                , correctedHsec = hsec
                , submittedOn = read sbmtOn
                , visibility = vis
                })

-- An unsorted scoreboard for a specific race, with the current replay
-- for each racer partaking in it.
data Scoreboard = Scoreboard
    { scoreboardTrack :: !Track
    , scoreboardEntries :: Map Player ReplayInfo
    }
    deriving Show

-- Using an empty string is inelegant and a bit fragile, but it will do
-- for now.
initialScoreboard :: Scoreboard
initialScoreboard = Scoreboard
    { scoreboardTrack = ""
    , scoreboardEntries = Map.empty
    }

-- Checks whether a scoreboard has been initialised. This is just a
-- hacky way of not needing a Maybe Scoreboard for the scoreboard state.
isInitialised :: Scoreboard -> Bool
isInitialised = not . T.null . scoreboardTrack

-- Adds a replay to the scoreboard (if it's from a new racer, or an
-- improvement for its racer).
addToScoreboard :: ReplayInfo -> Scoreboard -> Scoreboard
addToScoreboard rplNew sb = sb
    { scoreboardEntries = scoreboardEntries sb &
        Map.alter (\case
            Nothing -> Just rplNew
            Just rplOld -> Just $
                case compareForScoreboard rplNew rplOld of
                    LT -> rplNew
                    _ -> rplOld)
            (replayPlayer rplNew)
    }

-- Extracts a sorted list of replays from an unsorted Scoreboard.
sortedScoreboard :: Scoreboard -> [ReplayInfo]
sortedScoreboard = sortBy compareForScoreboard
    . Map.elems . scoreboardEntries

-- An enumeration of scoreboard positions for the purposes of PTB.
-- PTBNth means seventh or lower, and PTBAbsent means currently not in
-- the race.
data PTBPos = PTB1st | PTB2nd | PTB3rd | PTB4th | PTB5th | PTB6th
    | PTBNth | PTBAbsent
    deriving (Eq, Ord, Show, Enum, Bounded)

-- Conversion from a one-based position.
toPTBPos :: Int -> PTBPos
toPTBPos n
    | n <= 6 = toEnum (n - 1)
    | otherwise = PTBNth

-- Conversion to a one-based position. Use fromEnum to convert to a
-- zero-based index.
fromPTBPos :: PTBPos -> Maybe Int
fromPTBPos = \case
    PTBNth -> Nothing
    PTBAbsent -> Nothing
    pos -> Just $! fromEnum pos + 1

-- Conversion factor for real hours needed to earn credit, or "stunts
-- hours". This function ultimately determines the range of positions
-- considered in the PTB system.
ptbFactor :: PTBPos -> Maybe Int
ptbFactor = \case
    PTB1st -> Just 1
    PTB2nd -> Just 2
    PTB3rd -> Just 3
    PTB4th -> Just 5
    PTB5th -> Just 8
    PTB6th -> Just 13
    _ -> Nothing

-- Range from first up to the lowest PTB earning position.
ptbEarningRange :: [PTBPos]
ptbEarningRange = [PTB1st .. PTB6th]
-- This could have been made to automatically reflect the definition of
-- ptbFactor:
--
-- ptbEarningRange = dropWhileEnd (isNothing . ptbFactor) [minBound .. maxBound]
--
-- That's probably too fussy, though.

-- Highest PTB-earning position. For instance, it is 6 for a top 6 PTB
-- system, and 1 for an LTB system.
nPTB :: Int
nPTB = length ptbEarningRange

-- Stores minutes spent in each position within ptbEarningRange.
--
-- Ideally this would be something like a sized vector. The minutes are
-- stored after being rounded down from seconds, which is why Int is
-- used instead of NominalDiffTime. Note that any further rounding down
-- to hours or stunts hours is not applied here.
type MinutesCounter = Seq RMin

initialMinutesCounter :: MinutesCounter
initialMinutesCounter = Seq.replicate nPTB 0

-- Get minutes from a counter at the position given by a PTBPos.
getMinutesAt :: PTBPos -> MinutesCounter -> RMin
getMinutesAt pos counter = case pos of
    PTBNth -> 0
    PTBAbsent -> 0
    _ -> counter `Seq.index` fromEnum pos

-- Add minutes to a counter at the position given by a PTBPos.
addMinutesAt :: PTBPos -> RMin -> MinutesCounter -> MinutesCounter
addMinutesAt pos mins counter = case pos of
    PTBNth -> counter
    PTBAbsent -> counter
    _ -> Seq.adjust' (mins +) (fromEnum pos) counter

-- Player data as it evolves during a race.
data PlayerState = PlayerState
    { playerPTBPos :: !PTBPos
    , playerMinutes :: MinutesCounter
    , playerCarryover :: !Credit
    , playerLastUpdate :: !LocalTime
    }
    deriving Show

-- PlayerState for a player whose state wasn't been tracked before the
-- current replay.
newPlayerState :: PTBPos -> LocalTime -> PlayerState
newPlayerState pos upd = PlayerState
    { playerPTBPos = pos
    , playerMinutes = initialMinutesCounter
    , playerCarryover = 0
    , playerLastUpdate = upd
    }

-- Checks whether a player has anything that merits reporting, that is,
-- either minutes in PTB positions or carryover from past races.
hasEarnings :: PlayerState -> Bool
hasEarnings ps = any (/= 0) (playerMinutes ps)
    || playerCarryover ps /= 0

-- Resolution to round down earned time into credit. R is for real
-- units, S is for stunts units.
--
-- Note that real minutes resolution would be awkward to have, as it
-- would imply having to give credit for sub-minute intervals e.g. at
-- the lead.
data CreditResolution = SMinRes | RHourRes | SHourRes
    deriving (Eq, Show, Ord, Enum)

-- Default resolution for assigning credit.
chosenResolution :: CreditResolution
chosenResolution = SMinRes

-- Earned real minutes given two consecutive update times.
earnedMinutes :: LocalTime -> LocalTime -> RMin
earnedMinutes prevUpd currUpd = floor (secondsElapsed / 60)
    where
    secondsElapsed = nominalDiffTimeToSeconds (diffLocalTime currUpd prevUpd)

-- Converts real minutes to stunts minutes. Stronger types here might be
-- nice to have.
toStuntsMinutes :: PTBPos -> RMin -> SMin
toStuntsMinutes pos mins = case ptbFactor pos of
    Just factor -> mins `div` factor
    Nothing -> 0

-- Credit earned for each stunts hour (real hour in 1st place). Divide
-- by ptbFactor to convert to other positions. The least common multiple
-- of the possible factors and 60. For instance, if the possible factors
-- are 1, 2, 3, 5, 8 and 13, creditPerLeadHour is 1560.
creditPerLeadHour :: Credit
creditPerLeadHour = foldl' lcm 60 $
    max 1 . fromMaybe 1 . ptbFactor <$> [minBound .. maxBound]

-- Credit earned for each stunts minute (real minute in 1st place).
creditPerLeadMinute :: Credit
creditPerLeadMinute = creditPerLeadHour `div` 60

-- Tally the credit for a minutes counter.
counterToCredit :: CreditResolution -> MinutesCounter -> Credit
counterToCredit res counter = creditPerLeadMinute
    * foldl' (\acc pos -> acc + roundStuntsMins pos (getMinutesAt pos counter))
        0 ptbEarningRange
    where
    -- The different ways of rounding get applied here.
    roundStuntsMins pos = case res of
        SMinRes -> toStuntsMinutes pos
        RHourRes -> toStuntsMinutes pos . (60 *) . (`div` 60)
        SHourRes -> (60 *) . (`div` 60) . toStuntsMinutes pos

-- Whether to have a PTB +0.5 step.
grantPlusHalf :: Bool
grantPlusHalf = True

-- Credit for PTB +0.5. If grantPlusHalf is false, this will be equal to
-- plusOneThreshold, and the PTB +0.5 step will, in effect, be skipped.
-- plusOneThreshold.
plusHalfThreshold :: Credit
plusHalfThreshold = plusOneThreshold
    `div` (if grantPlusHalf then 2 else 1)

-- Hours needed to earn PTB +1. An even value is preferred. This is the
-- base parameter which determines the earning rate everywhere.
hoursForPlusOne :: Int
hoursForPlusOne = 264

-- Credit for PTB +1. This is where changes to the point earning
-- should be done. An even number of hours multiplier is preferred.
plusOneThreshold :: Credit
plusOneThreshold = creditPerLeadHour * hoursForPlusOne

-- Credit for PTB +2.
plusTwoThreshold :: Credit
plusTwoThreshold = plusOneThreshold * 2

-- Converts credit to points.
creditToPoints :: Credit -> Double
creditToPoints credit = min 2 (fromIntegral wholePoints + fracPoints)
    where
    (wholePoints, remainder) = credit `divMod` plusOneThreshold
    fracPoints = case wholePoints of
        0 -> fromIntegral (remainder `div` plusHalfThreshold) / 2
        _ -> 0

-- Calculates the credit carryover for the next race, accounting for
-- the point assignments.
nextRaceCarryover :: CreditResolution -> PlayerState -> Credit
nextRaceCarryover res ps
    -- The order of the guards is such that if plusOneThreshold and
    -- plusTwoThreshold are equal then the middle case becomes
    -- impossible.
    | totalCredit >= plusOneThreshold = 0
    | totalCredit >= plusHalfThreshold = totalCredit - plusHalfThreshold
    | otherwise = totalCredit
    where
    totalCredit = addedCredit + playerCarryover ps
    addedCredit = counterToCredit res (playerMinutes ps)

-- Prepares a PlayerState for the next race. Carryover is calculated
-- here, based on the state at the end of the previous race.
nextRacePlayerState :: CreditResolution -> LocalTime -> PlayerState -> PlayerState
nextRacePlayerState res startTime ps = ps
    { playerPTBPos = PTBAbsent
    , playerMinutes = initialMinutesCounter
    , playerCarryover = nextRaceCarryover res ps
    , playerLastUpdate = startTime
    }

-- The "simulator" effect, which covers:
--
-- - Read-only track information (for now, just the bonus windows);
-- - As output, the PlayerStates at the end of each race;
-- - As updatable state, the unsorted scoreboard for the current race.
-- - Also as updatable state, the PlayerStates during the current race.
data Simulator e = Simulator
    (Reader (Map Track BonusWindow) e)
    (Writer (Map Track (Map Player PlayerState)) e)
    (State Scoreboard e)
    (State (Map Player PlayerState) e)

-- Runs a simulation solely for the writer output (that is, the player
-- states grouped by track).
execSimulator
    :: Map Track BonusWindow
    -> (forall e. Simulator e -> Eff (e :& es) x)
    -> Eff es (Map Track (Map Player PlayerState))
execSimulator windows k =
    execWriter $ \wt_pss ->
    runReader windows $ \rd_wnd ->
    withStateSource $ \source -> do
        st_sb <- newState source initialScoreboard
        st_pss <- newState source Map.empty
        useImplIn k $ Simulator
            (mapHandle rd_wnd)
            (mapHandle wt_pss)
            (mapHandle st_sb)
            (mapHandle st_pss)

-- The main loop.
processReplays :: e :> es => Vector ReplayInfo -> Simulator e -> Eff es ()
processReplays rpls (Simulator rd_wnd wt_pss st_sb st_pss) = do
    let res = chosenResolution
    for_ rpls $ \rpl -> do
        let trk = replayTrack rpl
        -- This assumes the track CSV has no missing tracks.
        wnd <- asks rd_wnd (Map.! trk)
        sbOld <- get st_sb
        let trkOld = scoreboardTrack sbOld
        -- Track switching (the input replays are assumed to be oredered
        -- chronologically).
        when (trk /= trkOld) $ do
            when (isInitialised sbOld) $ do
                -- Tally credit and report states at the end of the
                -- race.
                wrapUpTrack trkOld
                -- Prepare the player states for the next race.
                modify st_pss $
                    fmap (nextRacePlayerState res (windowStart wnd))
            -- Clear the scoreboard for the next race.
            put st_sb Scoreboard
                { scoreboardTrack = trk
                , scoreboardEntries = Map.empty
                }
        -- Replays sent on quiet days are ignored for the purposes of
        -- PTB. Replays sent before the start of the window have their
        -- submission time shifted forward.
        when (submittedOn rpl < quietDaysStart wnd) $ do
            modify st_sb $ addToScoreboard rpl
            -- For instance, assuming a top 6 PTB system, top seven is
            -- the range subject to status changes. Note that this means
            -- the state of the players who occupy the seventh place
            -- will be tracked as well, which, though not particularly
            -- useful, is tolerable.
            updateTopN (max (submittedOn rpl) (windowStart wnd))
    -- Tally credit and report states at the end of the final race. The
    -- duplication is needed because this update is otherwise only done
    -- before a track change.
    finalTrk <- scoreboardTrack <$> get st_sb
    wrapUpTrack finalTrk
    where
    wrapUpTrack trk = do
        -- Tally the remaining credit until the start of quiet days.
        wnd <- asks rd_wnd (Map.! trk)
        refreshTopN (quietDaysStart wnd)
        -- Remove player states without earnings.
        modify st_pss $ Map.filter hasEarnings
        -- Report the player states at the start of quiet days.
        pss <- get st_pss
        tell wt_pss (Map.singleton trk pss)
    updateTopN updTime = do
        topN <- zip [1 .. nPTB + 1] . sortedScoreboard <$> get st_sb
        for_ topN $ updatePlayerState st_pss updTime
    refreshTopN updTime = do
        topN <- take nPTB . sortedScoreboard <$> get st_sb
        for_ topN $ refreshPlayerCredit st_pss updTime

-- Update player state given a potentially relevant change to the
-- scoreboard.
updatePlayerState
    :: st_pss :> es
    => State (Map Player PlayerState) st_pss
    -> LocalTime  -- ^ Submission time from the replay that triggered
                  --   the update.
    -> (Int, ReplayInfo)
    -> Eff es ()
updatePlayerState st_pss updTime (nPos, rpl) = do
    let pos = toPTBPos nPos
        player = replayPlayer rpl
    modify st_pss $ Map.alter (\case
        Nothing -> Just $! newPlayerState pos updTime
        Just ps -> Just $! let prevPos = playerPTBPos ps
            in if playerPTBPos ps /= pos
                then ps
                    { playerPTBPos = pos
                    , playerMinutes = addMinutesAt prevPos
                        (earnedMinutes (playerLastUpdate ps) updTime)
                        (playerMinutes ps)
                    , playerLastUpdate = updTime
                    }
                else ps)
        player

-- Update player credit unconditionally, as in the start of quiet days.
refreshPlayerCredit
    :: st_pss :> es
    => State (Map Player PlayerState) st_pss
    -> LocalTime  -- ^ Update time.
    -> ReplayInfo
    -> Eff es ()
refreshPlayerCredit st_pss updTime rpl = do
    let player = replayPlayer rpl
    -- A refresh only deals with players already included, thus adjust
    -- instead of alter
    modify st_pss $ Map.adjust (\ps -> ps
            { playerMinutes = addMinutesAt (playerPTBPos ps)
                (earnedMinutes (playerLastUpdate ps) updTime)
                (playerMinutes ps)
            , playerLastUpdate = updTime
            })
        player

-- A display-oriented distillation of PlayerState.
data PlayerSummary = PlayerSummary
    { summaryMinutes :: MinutesCounter
    , summaryPrevCarryover :: !Credit
    , summaryEarnedCredit :: !Credit
    , summaryNextCarryover :: !Credit
    , summaryPoints :: !Double
    }
    deriving Show

-- Add corresponding fields of summaries.
addSummaries :: PlayerSummary -> PlayerSummary -> PlayerSummary
addSummaries su1 su2 = PlayerSummary
    { summaryMinutes = Seq.zipWith (+) (summaryMinutes su1) (summaryMinutes su2)
    , summaryPrevCarryover = summaryPrevCarryover su1 + summaryPrevCarryover su2
    , summaryEarnedCredit = summaryEarnedCredit su1 + summaryEarnedCredit su2
    , summaryNextCarryover = summaryNextCarryover su1 + summaryNextCarryover su2
    , summaryPoints = summaryPoints su1 + summaryPoints su2
    }

-- Converts a PlayerState to a PlayerSummary. Points are assigned here.
toSummary :: CreditResolution -> PlayerState -> PlayerSummary
toSummary res ps = PlayerSummary
    { summaryMinutes = playerMinutes ps
    , summaryPrevCarryover = broughtCarryover
    , summaryEarnedCredit = earnedCredit
    , summaryNextCarryover = nextRaceCarryover res ps
    , summaryPoints = creditToPoints (earnedCredit + broughtCarryover)
    }
    where
    earnedCredit = counterToCredit res (playerMinutes ps)
    broughtCarryover = playerCarryover ps

-- Formats minutes for display in the CSV output.
formatMinutes :: Int -> String
formatMinutes m = formatTime defaultTimeLocale "%h:%M"
    (secondsToNominalDiffTime (60 * fromIntegral m))

-- Makes CSV rows from the player summaries.
-- The fields are:
-- track, racer, carryover from earlier races,
-- minutes at PTB positions (one field per position),
-- total stunts minutes earned, carryover to the next race,
-- points earned.
summariesToCsvRecords :: Map Track (Map Player PlayerSummary) -> [Csv.Record]
summariesToCsvRecords =  map makeRecord
    . concat . fmap sequenceA
    . Map.assocs
    . fmap (sortBy (comparing orderProjection) . Map.assocs)
    where
    orderProjection (_, ps) = Down
        ( summaryPoints ps
        , summaryNextCarryover ps
        , summaryEarnedCredit ps
        , summaryPrevCarryover ps
        )

    makeRecord (trk, (p, su)) = Csv.record $
        [ Csv.toField trk
        , Csv.toField p
        , Csv.toField . formatMinutes $
            summaryPrevCarryover su `div` creditPerLeadMinute
        ]
        ++ map (Csv.toField . formatMinutes) (toList $ summaryMinutes su)
        ++
        [ Csv.toField . formatMinutes $
            summaryEarnedCredit su `div` creditPerLeadMinute
        , Csv.toField . formatMinutes $
            summaryNextCarryover su `div` creditPerLeadMinute
        , Csv.toField (summaryPoints su)
        ]

-- Header row for the CSV output.
summaryHeader :: Csv.Header
summaryHeader = Csv.record . map Csv.toField $
    [ "Track"
    , "Racer"
    , "Previous"
    ]
    ++ map show ptbEarningRange
    ++
    [ "Earned"
    , "Next"
    , "Points"
    ]

-- Invoke in GHCi with:
-- :main "/path/to/replays.csv"
main :: IO ()
main = do
    rpls <- getArgs >>= readAllReplays . \case
        path : _ -> path
        [] -> "data/results-2023.csv"
    windows <- readBonusWindows "data/tracks.csv"
    let foo = runPureEff $ execSimulator windows (processReplays rpls)
    {-
    print $
        Map.filter ((0 <) . summaryPoints)
        . fmap (toSummary chosenResolution)
        $ foo Map.! "ZCT268"
        -}
    {-
    print $
        sortBy (comparing (Down . summaryPoints . snd))
        . Map.assocs
        . foldl' (Map.unionWith addSummaries) Map.empty
        . map (fmap (toSummary chosenResolution))
        $ Map.elems foo
        -}
    BL.writeFile "test.csv" . Csv.encode $
        (summaryHeader :) . summariesToCsvRecords
        . fmap (fmap (toSummary chosenResolution))
        $ foo
