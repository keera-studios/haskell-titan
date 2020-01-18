module Model.Model where

-- | Application Model. Contains information about the simulation and the
--   frames captured so far.
data Model = Model
  { selectedFrame         :: Maybe Int      -- ^ Currently selected frame in the GUI
  , selectedFrameInput    :: Maybe String   -- ^ Input of currently selected frame
  , curSimFrame           :: Maybe Int      -- ^ Currently simulated frame in the game
  , frames                :: [Frame]        -- ^ (Best) knowledge of frames in the game
  }

-- | Default model with no loaded simulation.
emptyBM :: Model
emptyBM = Model
  { selectedFrame      = Nothing
  , selectedFrameInput = Nothing
  , curSimFrame        = Nothing
  , frames             = []
  }

-- | Game frame.
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

defaultSelectedFrame   = Frame True  True False False False
defaultFrame           = Frame False True False False False
defaultCurrentFrame    = Frame False False True False False
defaultBreakpointFrame = Frame False True False True  False
defaultErrorFrame      = Frame False True False False True

-- | Selection of frames used to visualize the Stream Viewer.
defaultFrames = zipWith ($)
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
