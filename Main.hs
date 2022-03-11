{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Debug.Trace
import Graphics.Vega.VegaLite
import LSODA
import Text.Printf
import GHC.Real

g = 9.81 --[m/s^2] gravitational acceleration

wheelRadius = 0.34 --[m]

inertia = 5 --[kg m] moment of inertia of wheel (+drive line)

mass = 1500 --[kg] mass of the care

-- tMax = 20
tMax = 4.5e-11

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

fprim :: RHS
fprim t y =
  let v : omega : _ = y
      slipRatio = sigma omega v
      fTraction = clamp1 (slipRatio / 0.06) * mass * g
      a = (fTraction + fDrag v + fRoll v) / mass
      alpha = (-fTraction * wheelRadius + tDrive t) / inertia
  in traceShow [a,alpha*wheelRadius,omega,v] [a, alpha]

main :: IO ()
main = do
  let res = simpLsoda fprim [0, 0] (StartStop 0 tMax)
  let LSODARes {success = didSucceed, ts = t, ys = ys, msg = msg, optOutput = LSODAOO {nfe=feval}} = res
  unless didSucceed . printf "Failed solving. Message: %s" $ msg
  printf "It took %d function evaluations to complete all" feval
  let [v, omega] = foldr (\[v, w] [vs, ws] -> [vs ++ [v], ws ++ [w]]) [[], []] ys
  let [a,alpha] =  foldr (\(t', y') [as, alphas] -> let [a,alpha] = fprim t' y' in [as ++ [a], alphas ++ [alpha]] ) [[], []] (zip t ys)
  let manualData =
        dataFromColumns []
          . dataColumn "Time (s)" (Numbers t)
          . dataColumn "Speed (m/s)" (Numbers v)
          . dataColumn "Acceleration (m/s^2)" (Numbers a)
          . dataColumn "Wheel acceleration (m/s^2)" (Numbers [alpha' * wheelRadius | alpha' <- alpha])
          . dataColumn "Wheel speed (m/s)" (Numbers [w * wheelRadius | w <- omega])
          . dataColumn "Drive torque (kNm)" (Numbers [tDrive s / 1000 | s <- t])
          . dataColumn "Roll resistance (kN)" (Numbers [fDrag w / 1000 | w <- omega])
          . dataColumn "Drag force (kN)" (Numbers [fRoll w / 1000 | w <- omega])
          . dataColumn "Slip ratio (%)" (Numbers [100 * sigma om ve | (om, ve) <- zip omega v])
          $ []
  let trans =
        transform
          . foldAs
            [ "Speed (m/s)",
              "Acceleration (m/s)",
              "Wheel acceleration (m/s^2)",
              "Wheel speed (m/s)",
              "Drive torque (kNm)",
              "Roll resistance (kN)",
              "Drag force (kN)",
              "Slip ratio (%)"
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