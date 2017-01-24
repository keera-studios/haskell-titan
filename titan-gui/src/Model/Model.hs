module Model.Model where

data Model = Model
  { selectedFrame :: Maybe Int
  , selectedFrameInput :: Maybe String
  }

emptyBM :: Model
emptyBM = Model
  { selectedFrame = Nothing
  , selectedFrameInput = Nothing
  }
