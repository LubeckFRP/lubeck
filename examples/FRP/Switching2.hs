
{-# LANGUAGE NoImplicitPrelude #-}

import Lubeck.FRP
import Lubeck.Drawing
import Linear.V2
import BasePrelude hiding (Signal, Const)
import Control.Monad.Fix (mfix)

data ShipState = SS
  { course :: V2 Double     -- movement during a second
  , position :: P2 Double
  , size :: Double          -- radius for now, really a BB/envelope

  , hitpoints :: Double
  , damage :: Double        -- HP deduct per second when in area
  , damageRadius :: Double

  , playerControlled :: Bool
  , selected :: Bool
  }
drawShipState :: ShipState -> Drawing
drawShipState _ = mempty



type Ship = Signal ShipState
type Game = Signal [Ship]

drawGame :: Game -> Signal Drawing
drawGame x = fmap (mconcat . fmap drawShipState ) $ join $ fmap sequence x


game :: Events Ship -> FRP Game
game newShip = [] `accumS` (fmap (:) newShip)

-- Continuous transformation of ships per time unit (i.e. damage over time, movement over time etc).
-- Integrate to get basic ship movement.
physics :: Game -> Events (ShipState -> ShipState)
physics _ = mempty

-- often :: Events Duration -- time since last firing, current time
-- often = mempty

loop
  :: Events ShipState -- new ship created
  -> Events (ShipState -> ShipState) -- player order to ships (including selection)
  -> FRP (Signal Drawing)
loop newShip updateShips = do
  g <- mfix $ \g -> do
          newShip2 <- reactimateIO $ fmap (`accumS` (updateShips <> physics g)) newShip
          game newShip2
  return $ drawGame g


-- depends on game
-- game depends on this

type Duration = Double
type Time = Double -- seconds
