{-# LANGUAGE DataKinds #-}
module Main where

import LSODA
import Numeric.LinearAlgebra.Static
import Text.Printf
import Control.Monad
import qualified Numeric.LinearAlgebra as LA


fprim :: RHS 3
fprim = MkRHS fprim'

fprim' :: (Double -> R 3 ->  R 3)
fprim' t y =
  let y0 = extract y LA.! 0
      y1 = extract y LA.! 1
      y2 = extract y LA.! 2
      ydot0 = -0.4E0*y0 + 1.0E4*y1*y2;
      ydot2 = 3.0e7*y1*y1;
      ydot1 = -ydot0 - ydot2;
  in vector [ydot0,ydot1,ydot2]


-- TODO only give outputs at the desired points
-- this is triggered by using a different timespec in the API
-- the time-spec should be the range [0,0.4,4,40,400,....4e10]
-- the task should be 1 to integrate until the next time in the time-spec

-- TODO adjust tolerances
-- rTolPtr <- new 1e-4
-- aTolPtr <- newArray [1e-6,1e-10,1e-6]
-- itol = 1 if atol is scalar
-- itol = 2 if atol is array
-- soecify this by a new datatype for atol in this input to simpLsoda
main :: IO ()
main = do
  let y0 = vector [1,0,0] :: R 3
      tMax = 4e10
      res = simpLsoda fprim y0 (StartStop 0 tMax)
      LSODARes {success = didSucceed, ts = t, ys = ys', msg = msg', optOutput = LSODAOO {nfe = feval, nst=nst,nje=nje,mused=mused,tsw=tsw}} = res
  sequenceA $ zipWith (\a b -> (printf " at t = %12.4e, y = %s\n" b (show a))) ys' t
  unless didSucceed . printf "Failed solving. Message: %s" $ msg'
  printf " no. steps = %4d  no. f-s = %4d  no. j-s =  %4d\n"  nst feval nje
  printf " method last used = %2d   last switch was at t =  %12.4e\n" mused tsw
