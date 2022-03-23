{-# LANGUAGE DataKinds #-}
module Main where

import LSODA
import Numeric.LinearAlgebra.Static
import Text.Printf
import Control.Monad
import qualified Numeric.LinearAlgebra as LA


fprim :: (Double -> R 3 ->  R 3)
fprim t y =
  let y0 = extract y LA.! 0
      y1 = extract y LA.! 1
      y2 = extract y LA.! 2
      ydot0 = -0.4E0*y0 + 1.0E4*y1*y2;
      ydot2 = 3.0e7*y1*y1;
      ydot1 = -ydot0 - ydot2;
  in vector [ydot0,ydot1,ydot2]

main :: IO ()
main = do
  let y0 = vector [1,0,0]
      res = simpLsoda fprim y0 (TSpace [0,4e-1,4e0,4e1,4e2,4e3,4e4,4e5,4e6,4e7,4e8,4e9,4e10]) (TolV 1e-4 ( vector [1e-6,1e-10,1e-6]))
      LSODARes {success = didSucceed, ts = t, ys = ys', msg = msg', optOutput = LSODAOO {nfe = feval, nst=nst,nje=nje,mused=mused,tsw=tsw}} = res
  zipWithM_ (\a b -> printf " at t = %12.4e, y = %14.6e %14.6e %14.6e\n" b (extract a LA.! 0) (extract a LA.! 1) (extract a LA.! 2)) ys' t
  unless didSucceed . printf "Failed solving. Message: %s" $ msg'
  printf " no. steps =%4d  no. f-s =%4d  no. j-s =%4d\n"  nst feval nje
  printf " method last used =%2d   last switch was at t =%12.4e\n" mused tsw
