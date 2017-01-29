module Model.Model where

data Model = Model
  { selectedFrame         :: Maybe Int
  , selectedFrameInput    :: Maybe String
  , curSimFrame           :: Maybe Int
  , frames                :: [Frame]
  }

data Frame = Frame
  { fSelected   :: Bool
  , fCached     :: Bool
  , fCurrent    :: Bool
  , fBreakpoint :: Bool
  , fError      :: Bool
  , fNumber     :: Int
  }
 deriving (Show, Eq)

frameDeselect :: Frame -> Frame
frameDeselect f = f { fSelected = False }

frameSelect :: Frame -> Frame
frameSelect f = f { fSelected = True }

frameNotCurrent :: Frame -> Frame
frameNotCurrent f = f { fCurrent = False }

frameCurrent :: Frame -> Frame
frameCurrent f = f { fCurrent = True }

emptyBM :: Model
emptyBM = Model
  { selectedFrame      = Nothing
  , selectedFrameInput = Nothing
  , curSimFrame        = Nothing
  , frames             = defaultFrames
  }

defaultSelectedFrame   = Frame True  True False False False
defaultFrame           = Frame False True False False False
defaultCurrentFrame    = Frame False False True False False
defaultBreakpointFrame = Frame False True False True  False
defaultErrorFrame      = Frame False True False False True

defaultFrames = zipWith (\f x -> f x)
         [ defaultFrame,         defaultFrame,           defaultFrame
         , defaultFrame,         defaultFrame,           defaultFrame
         , defaultFrame,         defaultFrame,           defaultFrame
         , defaultFrame,         defaultFrame,           defaultFrame
         , defaultFrame,         defaultFrame,           defaultFrame
         , defaultFrame,         defaultFrame,           defaultFrame
         , defaultFrame,         defaultFrame,           defaultFrame
         , defaultFrame,         defaultFrame,           defaultFrame
         , defaultErrorFrame,    defaultFrame,           defaultCurrentFrame
         , defaultFrame,         defaultBreakpointFrame, defaultSelectedFrame
         , defaultFrame,         defaultFrame,           defaultFrame
         , defaultFrame,         defaultFrame,           defaultFrame
         , defaultFrame,         defaultFrame,           defaultFrame
         , defaultFrame,         defaultFrame
         ]
         [0..]
