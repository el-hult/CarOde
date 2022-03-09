{-# LANGUAGE ForeignFunctionInterface #-}

module LSODA where

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import System.IO.Unsafe
import Text.Printf

-- | The right hand side function f in a ODE
-- y' = f(t,y)
type RHS =
  -- | The dimensionality of f
  Int ->
  -- | t
  Double ->
  -- | y
  [Double] ->
  -- | y'
  [Double]

-- | A F-fun computes the right hand side in a ODE
-- It is a subroutine. I.e. returns void.
type FFun =
  -- | `neq` The number of equations
  Ptr Int ->
  -- | The independent variable. Called t normally
  Ptr Double ->
  -- | The current state vector. An array of length `neq`
  Ptr Double ->
  -- | The memory where to put the output. Points to an array of length `neq`
  Ptr Double ->
  IO ()

-- | a subroutine to compute the jacobian of the thing
-- jac (neq, t, y, ml, mu, pd, nrowpd)
type JacFun =
  -- | `neq` the number of equations
  Ptr Int ->
  -- | `t` the number of equations
  Ptr Double ->
  -- | `y` the current state
  Ptr Double ->
  -- | `ml`
  Ptr Double ->
  -- | `mu`
  Ptr Double ->
  -- | `pd` the vector to fill with partial derivatives. an array representing a matrix with `nrowpd` rows, and `neq` columns
  -- Dont forget that fortran is column major! so entry [k `neq` + j] is the kth column, jth row
  -- The exact format depends on the jacobian type passed to @lsoda'
  Ptr Double ->
  -- | `nrowpd` the number of equations
  Ptr Int ->
  ()

foreign import ccall safe "lsoda_"
  lsoda' ::
    -- | void *f, // subroutine for right-hand side vector f.
    FunPtr FFun ->
    -- | int *neq, // number of first order ode-s.
    Ptr Int ->
    -- | double *y, //array of initial values, of length neq.
    Ptr Double ->
    -- | double *t, // the initial value of the independent variable.
    Ptr Double ->
    -- | double *tout, // first point where output is desired (.ne. t).
    Ptr Double ->
    -- | int *itol, // 1 or 2 according as atol (below) is a scalar or array.
    Ptr Int ->
    -- | double *rtol, // relative tolerance parameter (scalar).
    Ptr Double ->
    -- | double *atol, // absolute tolerance parameter (scalar or array).
    Ptr Double ->
    -- | int *itask, // 1 for normal computation of output values of y at t = tout.
    Ptr Int ->
    -- | int *istate, // integer flag (input and output).  set istate = 1.
    Ptr Int ->
    -- | int *iopt, // 0 to indicate no optional inputs used.
    Ptr Int ->
    -- | double *rwork, // real work array of length at least. 22 + neq * max(16, neq + 9).
    Ptr Double ->
    -- | int *lrw, // declared length of rwork (in user-s dimension).
    Ptr Int ->
    -- | int *iwork, // integer work array of length at least  20 + neq.
    Ptr Int ->
    -- | int *liw, // declared length of iwork (in user-s dimension).
    Ptr Int ->
    -- | void *jac, // name of subroutine for jacobian matrix. use a dummy name.  see also paragraph e below.
    FunPtr JacFun ->
    -- | int *jt // jacobian type indicator.  set jt = 2.
    -- If jt=2, then `jac` can be a null pointer, and the jacobian is numerically evaluated instead.
    Ptr Int ->
    IO ()

foreign import ccall "wrapper" wrapFFun :: FFun -> IO (FunPtr FFun)

foreign import ccall "wrapper" wrapJacFun :: JacFun -> IO (FunPtr JacFun)

data LSODARes = LSODARes
  { ys :: [[Double]],
    ts :: [Double],
    success :: Bool,
    msg :: String,
    noStepsTaken :: Int,
    noFs :: Int,
    noJs :: Int,
    lastMethodUsed :: Int,
    lastSwitchedAt :: Double
  }
  deriving (Show)

res0 =
  LSODARes
    { ys = [],
      ts = [],
      success = True,
      msg = "Not even started",
      noStepsTaken = 0,
      noFs = 0,
      noJs = 0,
      lastMethodUsed = -1,
      lastSwitchedAt = -1
    }

fprimWrapper :: RHS -> FFun
fprimWrapper fprim neqPtr tPtr yPtr yDotPtr = do
  neq <- peek neqPtr
  y <- peekArray neq yPtr
  t <- peek tPtr
  pokeArray yDotPtr $ fprim neq t y

-- | a simplified LSODA API for my specific use case :)
simpLsoda ::
  -- | The right hand side in the ODE
  RHS ->
  -- | The initial state
  [Double] ->
  -- | the time steps T for which to report results
  [Double] ->
  -- | returns
  -- left = error message
  -- right = the trajectory at the times t
  LSODARes
{-# NOINLINE simpLsoda #-}
simpLsoda ffun y0 ts = unsafePerformIO $ do
  let fex = fprimWrapper ffun
  if length ts <= 2
    then return $ res0 {msg = "Too short time vector", success = False, ys = [], ts = []}
    else simpLsodaAux fex y0 ts

-- PRE length ts >= 2
simpLsodaAux :: FFun -> [Double] -> [Double] -> IO LSODARes
simpLsodaAux ffun y0 ts = do
  let neqVal = length y0
  let lrwVal = 70 -- this work array size must be adjusted somehow.
  let liwVal = 23 -- Im not sure if this value can ALWAYS be 23
  fPtr <- wrapFFun ffun
  neqPtr <- new neqVal
  iWorkPtr <- newArray $ replicate liwVal 0
  rWorkPtr <- newArray $ replicate lrwVal 0
  yPtr <- newArray y0
  tPtr <- new $ head ts
  tOutPtr <- new 0.4
  iTolPtr <- new 2
  rTolPtr <- new 1e-4
  aTolPtr <- newArray [1e-6, 1e-10, 1e-6]
  iTaskPtr <- new 1
  iStatePtr <- new 1
  iOptPtr <- new 0
  lrwPtr <- new lrwVal
  liwPtr <- new liwVal
  jtPtr <- new 2
  let jacDummyPtr = nullFunPtr -- since I use jt=2, the jacobian can be a dummy argument. e.g. a null pointer
  -- step1
  -- PRE. initialize with success = True
  let step1 :: LSODARes -> [Double] -> IO LSODARes
      step1 res@LSODARes {success = False} _ = return res
      step1 res@LSODARes {success = True} [] = return res {msg = "Finished!"}
      step1 res@LSODARes {ys = yOuts, ts = tsThisFar} (t : ts) = do
        poke tOutPtr t -- set the new target time
        -- do the solving
        lsoda' fPtr neqPtr yPtr tPtr tOutPtr iTolPtr rTolPtr aTolPtr iTaskPtr iStatePtr iOptPtr rWorkPtr lrwPtr iWorkPtr liwPtr jacDummyPtr jtPtr
        -- find out how it went
        iState <- peek iStatePtr
        t <- peek tPtr
        yNew <- peekArray neqVal yPtr
        if iState < 0
          then return res {msg = printf "Stopped. istate =%d at t=%f" iState t, success = False}
          else step1 res {ys = yOuts ++ [yNew], ts = tsThisFar ++ [t], msg = "Completed a step"} ts

  finalVal <- step1 res0 {ys = [], ts = [], success = True, msg = ""} ts

  rwork <- peekArray lrwVal rWorkPtr
  iwork <- peekArray liwVal iWorkPtr
  return
    finalVal
      { noStepsTaken = iwork !! 10,
        noFs = iwork !! 11,
        noJs = iwork !! 12,
        lastMethodUsed = iwork !! 18,
        lastSwitchedAt = rwork !! 14
      }
