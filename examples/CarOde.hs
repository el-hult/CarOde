{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import Data.List
import GHC.Real
import Graphics.Vega.VegaLite
import LSODA
    ( simpLsoda,
      LSODARes(LSODARes, success, ts, ys, msg, optOutput),
      OptOut(LSODAOO, nfe),
      RHS(..),
      TimeSpec(StartStop) )
import Text.Printf
import Numeric.LinearAlgebra.Static
import qualified Numeric.LinearAlgebra.Data as LAD



g :: Double
g = 9.81 --[m/s^2] gravitational acceleration

wheelRadius :: Double
wheelRadius = 0.34 --[m]

inertia :: Double
inertia = 5 --[kg m] moment of inertia of wheel (+drive line)

mass :: Double
mass = 1500 --[kg] mass of the care

tMax :: Double
tMax = 20

inf :: Double
inf = fromRational infinity

clamp1 :: Double -> Double
clamp1 x = min 1 $ max x (-1)

sigma :: Double -> Double -> Double
sigma 0 0 = 0
sigma omega 0
  | omega < 0 = -inf
  | otherwise = inf
sigma omega v = (omega * wheelRadius - v) / abs v

tDrive :: Double -> Double
tDrive t
  | t < 3 = 200
  | t < 4 = 0
  | t < 7 = -500
  | t < 8 = 5500
  | t < 13 = 2000
  | otherwise = 0

fDrag :: Double -> Double
fDrag v = (-c) * v * abs v
  where
    c = 2 -- air resistence constant

fRoll :: Double -> Double
fRoll v = (-c) * v
  where
    c = 0.7 -- rolling resistance constant

fprim :: RHS 2
fprim = MkRHS fprim'

fprim' :: (Double -> R 2 ->  R 2)
fprim' t y =
  let (v, tmp) = headTail y
      (omega,_) = headTail tmp
      slipRatio = sigma omega v
      fTraction = clamp1 (slipRatio / 0.06) * mass * g
      a = (fTraction + fDrag v + fRoll v) / mass
      alpha = (-fTraction * wheelRadius + tDrive t) / inertia
  in vector [a, alpha]

main :: IO ()
main = do
  let c_t = "Time (s)"
  let c_v = "Car speed (m/s)"
  let c_acc = "Acceleration (m/s)"
  let c_wAcc = "Wheel acceleration (m/s^2)"
  let c_omegaR = "Wheel speed (m/s)"
  let c_drive = "Drive torque (kNm)"
  let c_roll = "Roll resistance (kN)"
  let c_drag = "Drag force (kN)"
  let c_sr = "Slip ratio (%)"
  let res = simpLsoda fprim (0 :: R 2 ) (StartStop 0 tMax)
  let LSODARes {success = didSucceed, ts = t, ys = ys', msg = msg', optOutput = LSODAOO {nfe = feval}} = res
  unless didSucceed . printf "Failed solving. Message: %s" $ msg'
  printf "It took %d function evaluations to complete all" feval
  let [v, omega] = transpose $ map (LAD.toList . unwrap) ys'
  let ydots = zipWith (\ a b -> LAD.toList ( unwrap ( (unRHS fprim) a b))) t ys'
  let [a, alpha] = transpose ydots
  let omegaR = [w * wheelRadius | w <- omega]
  let wAcc = [alpha' * wheelRadius | alpha' <- alpha]
  let manualData =
        dataFromColumns []
          . dataColumn c_t (Numbers t)
          . dataColumn c_v (Numbers v)
          . dataColumn c_acc (Numbers a)
          . dataColumn c_wAcc (Numbers wAcc)
          . dataColumn c_omegaR (Numbers omegaR)
          . dataColumn c_drive (Numbers [tDrive s / 1000 | s <- t])
          . dataColumn c_drag (Numbers (map (\v' -> fDrag v' / 1000) v))
          . dataColumn c_roll (Numbers (map (\w' -> fRoll w' / 1000) omega))
          . dataColumn c_sr (Numbers (map (100 *) (zipWith sigma omega v)))
          $ []
  let trans =
        transform
          . foldAs
            [ c_v,
              c_omegaR,
              c_drive,
              c_sr,
              c_drag,
              c_roll,
              c_acc,
              c_wAcc
            ]
            "Data"
            "Value"
  let enc =
        encoding
          . position X [PName "Time (s)", PmType Quantitative]
          . position Y [PName "Value", PmType Quantitative, PScale [SDomain (DNumbers [-2, 10])]]
          . color [MName "Data", MmType Nominal]
  let sel = selection . select "view" Interval [BindScales] $ []
  let vlPlot =
        toVegaLite
          [ manualData,
            trans [],
            mark Line [MPoint (PMMarker [])],
            enc [],
            height 600,
            width 600,
            sel
          ]
  toHtmlFile "plot.html" vlPlot
  putStrLn "Wrote plot.html. Check it!"