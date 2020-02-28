-- |
--
-- Copyright   : (C) Keera Studios Ltd, 2018
-- License     : GPL-3
-- Maintainer  : support@keera.co.uk
module FRP.Titan.Protocol where

-- Basic Titan Protocol

data Response = CurrentFrame   Int
              | CurrentHistory Int
              | CurrentTime    Float
              | MaxTime        Float
  deriving Read

data TitanEvent = HistoryChanched
  deriving Read

data TitanCommand = GetMaxTime
                  | GetCurrentTime
                  | GetCurrentFrame
                  | SummarizeHistory
                  | Step
                  | Skip
                  | StepUntil
                  | SkipBack
                  | Redo
                  | Play
                  | Stop
                  | Pause
                  | DeleteTrace
                  | ReplayTrace
                  | TravelToFrame Int
                  | JumpTo Int
                  | IOSense Int
                  | DiscardFuture Int
                  | LoadTraceFromString String
  deriving Show
