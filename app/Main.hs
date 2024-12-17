{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Bluefin.Eff
import Bluefin.State
import Bluefin.StateSource
import Bluefin.Reader
import Bluefin.Writer
import Bluefin.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector)
import qualified Data.Vector as V
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
import Debug.Trace

data BonusWindow = BonusWindow
    { windowStart :: !LocalTime
    , quietDaysStart :: !LocalTime
    }
    deriving Show

readBonusWindows
    :: FilePath
    -> IO (Map Text BonusWindow)
readBonusWindows csvPath = do
    csvData <- BL.readFile csvPath
    pure $ case Csv.decode Csv.HasHeader csvData of
        Left err -> error err  -- throw ex err
        Right v -> v & foldl'
            (\ws (name, _ :: String, start, end) ->
                Map.insert name BonusWindow
                    { windowStart = LocalTime (read start) midnight
                    , quietDaysStart = addLocalTime (-nominalDay)
                        (LocalTime (read end) midnight)
                    }
                    ws)
            Map.empty

type Player = Text
type Track = Text

data ReplayInfo = ReplayInfo
    { replayTrack :: !Track
    , replayPlayer :: !Player
    , correctedHsec :: !Int
    , submittedOn :: !LocalTime
    , visibility :: !Text
    }
    deriving Show

compareForScoreboard :: ReplayInfo -> ReplayInfo -> Ordering
compareForScoreboard rix riy =
    comparing correctedHsec rix riy <> comparing submittedOn rix riy

readAllReplays
    :: FilePath
    -> IO (Vector ReplayInfo)
readAllReplays csvPath = do
    csvData <- BL.readFile csvPath
    pure $ case Csv.decode Csv.HasHeader csvData of
        Left err -> error err  -- throw ex err
        Right v -> v & fmap (\(track, name, hsec, sbmtOn, vis) ->
            ReplayInfo
                { replayTrack = track
                , replayPlayer = name
                , correctedHsec = hsec
                , submittedOn = read sbmtOn
                , visibility = vis
                })

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

sortedScoreboard :: Scoreboard -> [ReplayInfo]
sortedScoreboard = sortBy compareForScoreboard
    . Map.elems . scoreboardEntries

data PTBPos = PTB1st | PTB2nd | PTB3rd | PTB4th | PTB5th | PTB6th
    | PTBnth | PTBAbsent
    deriving (Eq, Ord, Show, Enum)

-- Conversion from a one-based position.
toPTBPos :: Int -> PTBPos
toPTBPos n
    | n <= 6 = toEnum (n - 1)
    | otherwise = PTBnth

-- Conversion to a one-based position.
fromPTBPos :: PTBPos -> Maybe Int
fromPTBPos = \case
    PTBnth -> Nothing
    PTBAbsent -> Nothing
    pos -> Just $! fromEnum pos + 1

-- Conversion factor for real hours needed to earn credit, or "stunts
-- hours".
ptbFactor :: PTBPos -> Maybe Int
ptbFactor = \case
    PTB1st -> Just 1
    PTB2nd -> Just 2
    PTB3rd -> Just 3
    PTB4th -> Just 5
    PTB5th -> Just 8
    PTB6th -> Just 13
    _ -> Nothing

data PlayerState = PlayerState
    { playerPTBPos :: !PTBPos
    , playerAddedCredit :: !Int
    , playerCarryover :: !Int
    , playerLastUpdate :: !LocalTime
    }
    deriving Show

newPlayerState :: PTBPos -> LocalTime -> PlayerState
newPlayerState pos upd = PlayerState
    { playerPTBPos = pos
    , playerAddedCredit = 0
    , playerCarryover = 0
    , playerLastUpdate = upd
    }

-- Ideally this would be something like a sized vector. The minutes are
-- stored after being rounded down, which is why Int is used instead of
-- NominalDiffTime.
type MinutesCounter = Seq Int

initialMinutesCounter :: MinutesCounter
initialMinutesCounter = Seq.replicate 6 0

getMinutesAt :: PTBPos -> MinutesCounter -> Int
getMinutesAt pos counter = case pos of
    PTBnth -> 0
    PTBAbsent -> 0
    _ -> counter `Seq.index` fromEnum pos

addMinutesAt :: PTBPos -> Int -> MinutesCounter -> MinutesCounter
addMinutesAt pos mins counter = case pos of
    PTBnth -> counter
    PTBAbsent -> counter
    _ -> Seq.adjust' (mins +) (fromEnum pos) counter

-- Resloution to round down earned time into credit.
data CreditResolution = MinRes | HourRes | StuntsHourRes
    deriving (Eq, Show, Ord, Enum)

-- Default resolution for assigning credit.
chosenResolution :: CreditResolution
chosenResolution = HourRes

-- Earned real minutes given two consecutive update times.
earnedMinutes :: LocalTime -> LocalTime -> Int
earnedMinutes prevUpd currUpd = floor (secondsElapsed / 3600)
    where
    secondsElapsed = nominalDiffTimeToSeconds (diffLocalTime currUpd prevUpd)

-- Converts real minutes to stunts minutes. Stronger types here might be
-- nice to have.
toStuntsMinutes :: PTBPos -> Int -> Int
toStuntsMinutes pos mins = case ptbFactor pos of
    Just factor -> mins `div` factor
    Nothing -> 0

-- Converts real minutes to stunts hours, rounding down.
toStuntsHours :: PTBPos -> Int -> Int
toStuntsHours pos = (`div` 60) . toStuntsMinutes pos

-- Credit earned for each hour in 1st place. Divide by ptbFactor to
-- convert to other positions. Ideally, this should be the least common
-- multiple of the possible factors and 60, assuming that conversion to
-- minutes matters.
creditPerLeadHour :: Int
creditPerLeadHour = 120 * 13

-- Credit earned for each minute in 1st place
creditPerLeadMinute :: Int
creditPerLeadMinute = creditPerLeadHour `div` 60

-- Tally the credit for a minutes counter.
counterToCredit :: CreditResolution -> MinutesCounter -> Int
counterToCredit res counter = creditPerLeadMinute
    * foldl' (\acc pos -> acc + refloor (stuntsMins pos)) 0 [PTB1st .. PTB6th]
    where
    stuntsMins pos = toStuntsMinutes pos (getMinutesAt pos counter)
    -- Round to stunts hours if StuntsHourRes is being used.
    refloor = case res of
        MinRes -> id
        HourRes -> id
        StuntsHourRes -> (60 *) . (`div` 60)

-- Earned credit given a position and two consecutive update times.
-- Provided for debugging purposes.
earnedCredit :: CreditResolution -> PTBPos -> LocalTime -> LocalTime -> Int
earnedCredit res pos prevUpd currUpd = case res of
    MinRes -> creditPerLeadMinute * toStuntsMinutes pos realMinutes
    HourRes -> creditPerLeadMinute * toStuntsMinutes pos (60 * fullHours)
    StuntsHourRes -> creditPerLeadHour * toStuntsHours pos realMinutes
    where
    realMinutes = earnedMinutes prevUpd currUpd
    fullHours = realMinutes `div` 60

-- The first sketch of an implementation for earnedCredit. A good
-- illustration of the differences in rounding between HourRes and
-- StuntsHourRes. Dividing hours by the factor before converting to
-- credit discard quite a few more minutes:
--
-- (8 `div` 13) * 60 * creditPerLeadMinute = 0
-- (60 * 8 `div` 13) * creditPerLeadMinute = 936
earnedCredit' :: CreditResolution -> PTBPos -> LocalTime -> LocalTime -> Int
earnedCredit' res pos prevUpd currUpd =
    case ptbFactor pos of
        -- fullHours `div` factor gives the stunts hours earned.
        Just factor -> case res of
            MinRes -> (fullMinutes `div` factor) * creditPerLeadMinute
            HourRes -> (60 * fullMinutes `div` factor) * creditPerLeadHour
            StuntsHourRes -> (fullHours `div` factor) * creditPerLeadHour
        Nothing -> 0
        where
        secondsElapsed = nominalDiffTimeToSeconds (diffLocalTime currUpd prevUpd)
        fullMinutes = floor (secondsElapsed / 60)
        fullHours = floor (secondsElapsed / 3600)

-- Credit for PTB +0.5. This is where changes to the point earning
-- should be done.
plusHalfThreshold :: Int
plusHalfThreshold = creditPerLeadHour * 120

-- Credit for PTB +1.
plusOneThreshold :: Int
plusOneThreshold = plusHalfThreshold * 2

-- Credit for PTB +2.
plusTwoThreshold :: Int
plusTwoThreshold = plusOneThreshold * 2

nextRaceCarryover :: PlayerState -> Int
nextRaceCarryover ps
    | totalCredit < plusHalfThreshold = totalCredit
    | totalCredit < plusOneThreshold = totalCredit - plusHalfThreshold
    | otherwise = 0
    where
    totalCredit = playerAddedCredit ps + playerCarryover ps

nextRacePlayerState :: LocalTime -> PlayerState -> PlayerState
nextRacePlayerState startTime ps = ps
    { playerPTBPos = PTBAbsent
    , playerAddedCredit = 0
    , playerCarryover = nextRaceCarryover ps
    , playerLastUpdate = startTime
    }

processReplays
    :: (rd_wnd :> es, wt_pss :> es, st_sb :> es, st_pss :> es)
    => Reader (Map Track BonusWindow) rd_wnd
    -> Writer (Map Track (Map Player PlayerState)) wt_pss
    -> State Scoreboard st_sb
    -> State (Map Player PlayerState) st_pss
    -> Vector ReplayInfo
    -> Eff es ()
processReplays rd_wnd wt_pss st_sb st_pss rpls = do
    for_ rpls $ \rpl -> do
        let trk = replayTrack rpl
        -- This assumes the track CSV has no missing tracks.
        wnd <- (Map.! trk) <$> ask rd_wnd
        sbOld <- get st_sb
        let trkOld = scoreboardTrack sbOld
        -- Track switching (the input replays are assumed to be oredered
        -- chronologically).
        when (trk /= trkOld) $ do
            when (isInitialised sbOld) $ do
                -- Tally the remaining credit until the start of quiet
                -- days.
                wndOld <- (Map.! trkOld) <$> ask rd_wnd
                refreshTopN (quietDaysStart wndOld) 6
                -- Report the player states at the start of quiet days.
                pss <- get st_pss
                tell wt_pss (Map.singleton trkOld pss)
                -- Prepare the player states for the next race.
                modify st_pss $ fmap (nextRacePlayerState (windowStart wnd))
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
            -- Top seven is the range subject to status changes on a top
            -- 6 PTB system. Note that this means the state of the
            -- players who occupy the seventh place will be tracked as
            -- well, which though not particularly useful is tolerable.
            updateTopN (max (submittedOn rpl) (windowStart wnd)) 7
    -- Tally credit for final hours of final race. The duplication is
    -- needed because this update is otherwise only done before a track
    -- change.
    finalTrk <- scoreboardTrack <$> get st_sb
    finalWnd <- (Map.! finalTrk) <$> ask rd_wnd
    refreshTopN (quietDaysStart finalWnd) 6
    finalPss <- get st_pss
    tell wt_pss (Map.singleton finalTrk finalPss)
    where
    updateTopN updTime n = do
        topN <- zip [1..] . take n . sortedScoreboard <$> get st_sb
        for_ topN $ updatePlayerState st_pss updTime
    refreshTopN updTime n = do
        topN <- take n . sortedScoreboard <$> get st_sb
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
                    , playerAddedCredit = playerAddedCredit ps
                        + earnedCredit chosenResolution
                            prevPos (playerLastUpdate ps) updTime
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
            { playerAddedCredit = playerAddedCredit ps
                + earnedCredit chosenResolution
                    (playerPTBPos ps) (playerLastUpdate ps) updTime
            , playerLastUpdate = updTime
            })
        player

main :: IO ()
main = do
    rpls <- readAllReplays "data/results-2023.csv"
    windows <- readBonusWindows "data/tracks.csv"
    let foo = runPureEff $
            execWriter $ \wt_pss ->
            runReader windows $ \rd_wnd ->
            withStateSource $ \source -> do
                st_sb <- newState source initialScoreboard
                st_pss <- newState source Map.empty
                processReplays rd_wnd wt_pss st_sb st_pss rpls
                -- get st_pss
    print (foo Map.! "ZCT269")

