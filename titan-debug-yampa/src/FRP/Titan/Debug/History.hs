{-# LANGUAGE MultiWayIf #-}

-- | Execution History
--
-- Copyright   : (C) Keera Studios Ltd, 2018
-- License     : GPL-3
-- Maintainer  : support@keera.co.uk
module FRP.Titan.Debug.History where

import Control.Monad
import Data.Maybe
import FRP.Yampa                 (SF, FutureSF, DTime)

import Data.Extra
import FRP.Titan.Debug.Comm

data History a b = History
  -- { getHistory :: (Maybe (a, Maybe (SF a b)), [(a, DTime, Maybe (FutureSF a b))])
  { getInputHistory :: Maybe (Stream a (DTime, a))
  , getSFHistory    :: Stream (SF a b) (FutureSF a b)
  , getPos          :: Int
  , getCurSF        :: Either (SF a b) (FutureSF a b)
  , getLastInput    :: Maybe a
  }

type Stream a a' = (a, [a'])

getCurSF' :: History a b -> Either (SF a b) (FutureSF a b)
getCurSF' history = fromMaybe (Left $ fst $ getSFHistory history) (getSampleAt (getSFHistory history) (getPos (history)))


-- INV: forall h . isNothing (fst (getHistory h)) \/ isNothing (fst (getFuture h))
-- INV: forall h . not (null (getHistory h)) ==> isNothing (fst (getFuture h))

-- ** Construction

-- | Create empty history pending to run a signal function
mkEmptyHistory :: SF a b -> History a b
mkEmptyHistory sf = History Nothing (sf, []) 0 (Left  sf) Nothing

-- | Create empty history with an initial sample and sf, and a next FutureSF
mkHistory :: (a, SF a b) -> FutureSF a b -> History a b
mkHistory (a0, sf0) sf' =
  History (Just (a0, [])) (sf0, [sf']) 1 (Right sf') (Just a0)

-- | Determine if history is currently pointing to a running SF.
historyIsRunning :: History a b -> Bool
historyIsRunning history = ((>0) . getPos) (history)

-- | Replace the input for a given frame/sample
historyReplaceInputAt :: History a b -> Int -> a -> History a b
historyReplaceInputAt history f a
    | ns < f    = history
    | f == 0    = if isNothing hs
                    then history
                    else history { getInputHistory = Just (a, ps)
                                 , getSFHistory    = ((\(x,y) -> (x, [])) $ getSFHistory history)
                                 }
    | otherwise = history { getInputHistory = Just (a0, appAt (f-1) (\(dt, _) -> (dt, a)) ps)
                          , getSFHistory    = ((\(x,y) -> (x, take f y)) $ getSFHistory history)
                          }
  where
    hs = getInputHistory history
    Just (a0, ps) = hs
    ns = maybe 0 (length.snd) hs

-- | Replace the time for a given frame/sample
historyReplaceDTimeAt :: History a b -> Int -> DTime -> History a b
historyReplaceDTimeAt history f dt =
  let Just (a0, ps) = getInputHistory history
      dts                  = 0 : map (\(dt,_) -> dt) ps
  in if length dts >= f
       then history
       else if f == 0
              then history
              else history { getInputHistory = Just (a0, appAt (f-1) (\(_,a) -> (dt, a)) ps)
                           , getSFHistory    = ((\(x,y) -> (x, take f y)) $ getSFHistory history)
                           }

-- | Replace the input and the time for a given frame/sample
historyReplaceInputDTimeAt :: History a b -> Int -> DTime -> a -> History a b
historyReplaceInputDTimeAt history f dt a =
  let (Just (a0, ps)) = getInputHistory history
      as              = a0 : map (\(_, a) -> a) ps
  in if length as >= f
       then history
       else if f == 0
              then history { getInputHistory = Just (a, ps)
                           , getSFHistory    = ((\(x,y) -> (x, [])) $ getSFHistory history)}
              else history { getInputHistory = Just (a0, appAt (f-1) (\(_,_) -> (dt, a)) ps)
                           , getSFHistory    = ((\(x,y) -> (x, take f y)) $ getSFHistory history)}

-- | Get the total time at a given point/frame
historyGetMaxTime :: History a b -> DTime
historyGetMaxTime history =
  case getInputHistory history of
    Nothing       -> 0
    Just (a0, ps) -> sum $ map (\(dt,_) -> dt) ps

-- | Get the total time at a given point/frame
historyGetGTime :: History a b -> Int -> Maybe DTime
historyGetGTime history f =
  case getInputHistory history of
    Nothing       -> Nothing
    Just (a0, ps) -> let dts = 0 : map fst ps
                         l   = length dts
                         e   = if l < f then Nothing else Just (sum (drop (l-f) dts))
                     in e

-- | Get the time delta for a given frame
historyGetDTime :: History a b -> Int -> Maybe DTime
historyGetDTime history f =
  case getInputHistory history of
    Nothing       -> Nothing
    Just (a0, ps) -> let dts = 0 : map fst ps
                         e   = if length dts < f || f < 0 then Nothing else Just (dts !! f)
                     in e

-- | Get the input for a given frame
historyGetInput :: History a b -> Int -> Maybe a
historyGetInput history f =
  case getInputHistory history of
    Nothing       -> Nothing
    Just (a0, ps) -> let as = a0 : map snd ps
                         e  = if length as < f  || f < 0then Nothing else Just (as !! f)
                     in e

-- | Get the time for the current frame
historyGetCurrentTime :: History t b -> DTime
historyGetCurrentTime history =
  case getInputHistory history of
    Just (a0, ps)  -> sum $ map (\(dt,_) -> dt) (take (getPos history) ps)
    Nothing        -> 0

-- | Get the current frame number.
historyGetCurrentFrame :: History a b -> Int
historyGetCurrentFrame history =  getPos history

-- | Record a running frame
historyRecordFrame1 :: History a b -> (a, DTime, FutureSF a b) -> History a b
historyRecordFrame1 history (a', dt, sf') = historySF
 where
   historyInput = case getInputHistory history of
                    Nothing       -> history
                    Just (a0, ps) -> if | pos > 0 && pos < length ps -> history { getInputHistory = Just (a0, appAt pos (const (dt, a')) ps) }
                                        | pos > 0                    -> history { getInputHistory = Just (a0, ps ++ [(dt, a')]) }
                                        | otherwise                  -> history

   historySF = let (s0, ss) = getSFHistory historyInput
               in if getPos history <= 0
                    then historyInput
                    else historyInput { getSFHistory = (s0, take (getPos history) ss ++ [sf'])
                                      , getPos       = pos + 1
                                      }
   pos = getPos history

-- | Get the total number of frames
historyGetNumFrames :: History t b -> Int
historyGetNumFrames history =
  case getInputHistory history of
    Just (a0, ps) -> length ps + 1
    Nothing       -> 0

-- | Get the current frame info
--
-- TODO: Partial function
historyGetCurFrame :: History a b -> (a, Maybe DTime, Maybe (Either (SF a b) (FutureSF a b)))
historyGetCurFrame history =
  case curInput of
    Just (Left a0)       -> (a0, Nothing, curSF)
    Just (Right (dt, a)) -> (a,  Just dt, curSF)
    Nothing              -> error "No current frame"
 where
   curInput = (`getSampleAt` (getPos history)) =<< getInputHistory history
   curSF    = (`getSampleAt` (getPos history)) (getSFHistory history)

getSampleAt :: Stream a a' -> Int -> Maybe (Either a a')
getSampleAt (s0, ss) 0 = Just (Left s0)
getSampleAt (s0, ss) n
  | n <= length ss = Just (Right (ss!!(n-1)))
  | otherwise      = Nothing

-- | Move one step back in history
historyBack :: History a b -> History a b
historyBack history = history { getPos = max 0 (getPos history - 1)}

  -- case getHistory history of
  --   (Just (a0, sf0), _:(_a,_dt, sf'):prevs@((lastInput, _, _):_)) -> (Just $ History (Just (a0, sf0), prevs) (Right sf') (Just lastInput), Right (sf', lastInput))
  --   (Just (a0, sf0), _:(_a,_dt, sf'):[])                          -> (Just $ History (Just (a0, sf0), [])    (Right sf') (Just a0),        Right (sf', a0))
  --   (Just (a0, sf0), _:[])                                        -> (Just $ History (Just (a0, sf0), [])    (Left sf0)  Nothing,          Left sf0)
  --   (Just (a0, sf0), [])                                          -> (Just $ History (Nothing, [])           (Left sf0)  Nothing,          Left sf0)
    -- TODO: undefined
    -- (Nothing, [])                                                 -> (Just $ history,                                                      getCurSF history)

-- | Jump to a specific frame number.
-- historyJumpTo :: History a b -> Int -> History a b
-- historyJumpTo history n =
--   case getHistory history of
--     (Nothing,_)          -> history
--     (Just (a0, sf0), ps) ->
--       if length ps + 1 > n
--         then if n > 0
--                then let ((_a,_dt, sf'):prevs@((lastInput, _, _):_)) = takeLast n ps
--                     in History (Just (a0, sf0), prevs) n (Right (fromJust sf')) (Just lastInput)
--                else mkEmptyHistory (fromMaybe (fromLeft (getCurSF history)) sf0)
--         else history

historyJumpTo :: History a b -> Int -> History a b
historyJumpTo history n =
  case getInputHistory history of
    Nothing       -> history
    Just (a0, ps) ->
      if length ps + 1 > n
        then history { getPos = n }
        else history

-- | Discard the future after a given frame.
historyDiscardFuture :: History a b -> Int -> History a b
historyDiscardFuture history n =
  case getInputHistory history of
    Nothing       -> history
    Just (a0, ps) ->
      if length ps + 1 > n
        then if n > 0
               then history { getInputHistory = Just (a0, take n ps)
                            , getSFHistory    = (\(s0, ss) -> (s0, take n ss)) (getSFHistory history)
                            , getPos          = min n (getPos history)
                            }
               else History { getInputHistory = Just (a0, [])
                            , getSFHistory    = (\(s0, ss) -> (s0, [])) (getSFHistory history)
                            , getPos          = 0
                            }
        else history

-- ** Command Queue

-- | Obtain a command from the command queue, polling the communication
--   bridge if the queue is empty.
getCommand :: (Read a, Show a) => ExternalBridge -> [a] -> IO (Maybe a, [a])
-- getCommand bridge (c:cs) = return (Just c, cs)
getCommand bridge cmds = do
  mLines <- filter (not . null) <$> getAllMessages bridge
  let cmLines = map maybeRead mLines
      cLines  = catMaybes cmLines
  unless (null mLines) $ do
    ebPrint bridge (show mLines)
    ebPrint bridge (show cmLines)
  case cmds ++ cLines of
    []     -> return (Nothing, [])
    (c:cs) -> return (Just c, cs)

-- | Place one command on the top of the queue.
pushCommand :: [a] -> a -> [a]
pushCommand cs c = c:cs

-- | Place one command on the top of the queue.
appendCommand :: [a] -> a -> [a]
appendCommand cs c = cs ++ [c]
