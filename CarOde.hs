{-# LANGUAGE OverloadedStrings #-}

-- | Try out numerical stiff solvers in Haskell. Use it with
-- @
--      runhaskell CarOde.hs -package hvega
-- @
module CarOde where

import Prelude hiding (filter, lookup, repeat)

import Graphics.Vega.VegaLite









{-

Numerical solvers 

-- PETZOLD, Linda. 
-- Automatic selection of methods for solving stiff and nonstiff systems of ordinary differential equations.
-- SIAM journal on scientific and statistical computing, 1983, 4.1: 136-148.
-- https://doi.org/10.1137/0904010


https://hackage.haskell.org/package/hmatrix-gsl 
needs BLAS and LAPACK C-libraries, and that is more easily installed on linux than on windows.
provides Adams and BDF solvers for stiff problems.
uses GSL (gnu scientific library), so the solution is really just C interop.

https://hackage.haskell.org/package/numeric-ode 
package is underdeveloped and clashes with modern versions of Base.
So that one is off the table as well.
It would provide some implicit and implicit solvers, as well as symplectic euler and Störmer-Verlet 
It is implemented in Haskell, so it is quite nice for inspiration.

LSODA seems to be kind of gold standard on stiff problems.
There is no port to Haskell. 
The standard thing is to use C FFI and connect to the Fortran library.

I Decided to go the FFI route, after all.

-}


























{- 
Plot results! I used hvega https://hackage.haskell.org/package/hvega
which produces a HTML output.

The decision is based on the simple installation. It needs no complitated graphics
installations. There where some options I considered thouhg.

1. Chart.  https://hackage.haskell.org/package/Chart
However, you need some backend.It seems that the package Chart-cairo is the most versatile, but it needs cairo installed
And that seems like a mess on a windows machine. see e.g. https://stackoverflow.com/questions/9526375/how-to-install-cairo-on-windows

2. Matplotlib https://hackage.haskell.org/package/matplotlib
I am familiar with this, but it seems that it is a pain to install with tons of dependencies.

3. Gloss https://hackage.haskell.org/package/gloss 
I have fiddeled a little in Gloss, and it seemed like a a fun experience. But too much work, I worry.
So I didn't. Simple!

-}
manualData = dataFromColumns []
                 . dataColumn "Time (s)" (Numbers time)
                 . dataColumn "Speed (m/s)" (Numbers speed)
                 . dataColumn "Drag (kN)" (Numbers drag)
                 $ []

time = [t/10 | t<- [1..100]]
speed = [x*x | x<- time]
drag = [ (- 2) * x * abs x /1000  | x <- speed]

trans = transform . foldAs [ "Speed (m/s)"
                          , "Drag (kN)"
                      ] "Data"  "Value"

enc = encoding
      . position X [PName "Time (s)", PmType Quantitative]
      . position Y [PName "Value", PmType Quantitative]
      . color [ MName "Data", MmType Nominal ]

vlPlot= toVegaLite
   [ manualData
   , trans []
   , mark Line []
   , enc []
   , height 500
   , width 600
   ]
main = do
    toHtmlFile "plot.html" vlPlot