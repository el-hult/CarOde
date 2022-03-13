{-# LANGUAGE ForeignFunctionInterface #-}

module LSODA where

import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Text.Printf
import Data.Int

-- | Fortran int. 
-- the exact type is dependant on the compiler options
-- In my case, it cmpiled to single precision ints.
type FInt = Int32

-- | The right hand side function f in a ODE
-- y' = f(t,y)
type RHS =
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
  Ptr FInt ->
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
  Ptr FInt ->
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
  Ptr FInt ->
  ()

foreign import ccall safe "lsoda_"
  lsoda' ::
    -- | void *f, // subroutine for right-hand side vector f.
    FunPtr FFun ->
    -- | int *neq, // number of first order ode-s.
    Ptr FInt ->
    -- | double *y, //array of initial values, of length neq.
    Ptr Double ->
    -- | double *t, // the initial value of the independent variable.
    Ptr Double ->
    -- | double *tout, // first point where output is desired (.ne. t).
    Ptr Double ->
    -- | int *itol, // 1 or 2 according as atol (below) is a scalar or array.
    Ptr FInt ->
    -- | double *rtol, // relative tolerance parameter (scalar).
    Ptr Double ->
    -- | double *atol, // absolute tolerance parameter (scalar or array).
    Ptr Double ->
    -- | int *itask, // 1 for normal computation of output values of y at t = tout.
    Ptr FInt ->
    -- | int *istate, // integer flag (input and output).  set istate = 1.
    Ptr FInt ->
    -- | int *iopt, // 0 to indicate no optional inputs used.
    Ptr FInt ->
    -- | double *rwork, // real work array of length at least. 22 + neq * max(16, neq + 9).
    Ptr Double ->
    -- | int *lrw, // declared length of rwork (in user-s dimension).
    Ptr FInt ->
    -- | int *iwork, // integer work array of length at least  20 + neq.
    Ptr FInt ->
    -- | int *liw, // declared length of iwork (in user-s dimension).
    Ptr FInt ->
    -- | void *jac, // name of subroutine for jacobian matrix. use a dummy name.  see also paragraph e below.
    FunPtr JacFun ->
    -- | int *jt // jacobian type indicator.  set jt = 2.
    -- If jt=2, then `jac` can be a null pointer, and the jacobian is numerically evaluated instead.
    Ptr FInt ->
    IO ()

foreign import ccall "wrapper" wrapFFun :: FFun -> IO (FunPtr FFun)

foreign import ccall "wrapper" wrapJacFun :: JacFun -> IO (FunPtr JacFun)

data LSODARes = LSODARes
  { ys :: [[Double]],
    ts :: [Double],
    success :: Bool,
    msg :: String,
    optOutput :: OptOut
  }
  deriving (Show)

res0 :: LSODARes
res0 =
  LSODARes
    { ys = [],
      ts = [],
      success = True,
      msg = "Not even started",
      optOutput = oo0
    }

fprimWrapper :: RHS -> FFun
fprimWrapper fprim neqPtr tPtr yPtr yDotPtr = do
  neq <- fromIntegral <$> peek neqPtr
  y <- peekArray neq yPtr
  t <- peek tPtr
  pokeArray yDotPtr $ fprim t y

data TimeSpec
  = -- | the start end end of the time interval. Indicates that the optimizer decides on step length
    StartStop Double Double

-- | a simplified LSODA API for my specific use case :)
simpLsoda ::
  -- | The right hand side in the ODE
  RHS ->
  -- | The initial state
  [Double] ->
  TimeSpec ->
  LSODARes
{-# NOINLINE simpLsoda #-}
simpLsoda ffun y0 ttt = unsafePerformIO $ do
  let fex = fprimWrapper ffun
  simpLsodaAux fex y0 ttt

-- | a dummy OO object
oo0 :: OptOut
oo0 =
  LSODAOO
    { nst = -1,
      nfe = -1,
      nje = -1,
      nqu = -1,
      nqcur = -1,
      imxer = -1,
      lenrw = -1,
      leniw = -1,
      mused = -1,
      mcur = -1,
      hu = -1,
      hcur = -1,
      tcur = -1,
      tolsf = -1,
      tsw = -1
    }

data OptOut = LSODAOO
  { -- |
    -- nst     iwork(11) the number of steps taken for the problem so far.
    nst :: Int,
    -- |
    -- nfe     iwork(12) the number of f evaluations for the problem so far.
    nfe :: Int,
    -- |
    -- nje     iwork(13) the number of jacobian evaluations (and of matrix
    --                   lu decompositions) for the problem so far.
    nje :: Int,
    -- |
    -- nqu     iwork(14) the method order last used (successfully).
    nqu :: Int,
    -- |
    -- | nqcur   iwork(15) the order to be attempted on the next step.
    nqcur :: Int,
    -- |
    -- imxer   iwork(16) the index of the component of largest magnitude in
    --                   the weighted local error vector ( e(i)/ewt(i) ),
    --                   on an error return with istate = -4 or -5.
    imxer :: Int,
    -- | lenrw   iwork(17) the length of rwork actually required, assuming
    --                   that the length of rwork is to be fixed for the
    --                   rest of the problem, and that switching may occur.
    --                   this is defined on normal returns and on an illegal
    --                   input return for insufficient storage.
    lenrw :: Int,
    -- | leniw   iwork(18) the length of iwork actually required, assuming
    --                   that the length of iwork is to be fixed for the
    --                   rest of the problem, and that switching may occur.
    --                   this is defined on normal returns and on an illegal
    --                   input return for insufficient storage.
    leniw :: Int,
    -- | mused   iwork(19) the method indicator for the last successful step..
    --                   1 means adams (nonstiff), 2 means bdf (stiff).
    --  c
    mused :: Int,
    ---                  on the next step.  thus it differs from mused
    --                  only if a method switch has just been made.

    -- | mcur    iwork(20) the current method indicator..
    --                   1 means adams (nonstiff), 2 means bdf (stiff).
    --                   this is the method to be attempted
    mcur :: Int,
    -- hu      rwork(11) the step size in t last used (successfully).
    hu :: Double,
    -- hcur    rwork(12) the step size to be attempted on the next step.
    hcur :: Double,
    -- tcur    rwork(13) the current value of the independent variable
    --                    which the solver has actually reached, i.e. the
    --                    current internal mesh point in t.  on output, tcur
    --                    will always be at least as far as the argument
    --                    t, but may be farther (if interpolation was done).
    tcur :: Double,
    -- tolsf   rwork(14) a tolerance scale factor, greater than 1.0,
    --                    computed when a request for too much accuracy was
    --                    detected (istate = -3 if detected at the start of
    --                    the problem, istate = -2 otherwise).  if itol is
    --                    left unaltered but rtol and atol are uniformly
    --                    scaled up by a factor of tolsf for the next call,
    --                    then the solver is deemed likely to succeed.
    --                    (the user may also ignore tolsf and alter the
    --                    tolerance parameters in any other way appropriate.)
    tolsf :: Double,
    -- tsw     rwork(15) the value of t at the time of the last method
    --                    switch, if any.
    tsw :: Double
  }
  deriving (Show)

parseOptOutputs :: [FInt] -> [Double] -> OptOut
parseOptOutputs iwork' rwork
  | length iwork < 20 = error "Invalid input to parseOptOutputs. iwork vector too short!"
  | length rwork < 15 = error "Invalid input to parseOptOutputs. rwork vector too short!"
  | otherwise =
    LSODAOO
      { nst = iwork !! 10,
        nfe = iwork !! 11,
        nje = iwork !! 12,
        nqu = iwork !! 13,
        nqcur = iwork !! 14,
        imxer = iwork !! 15,
        lenrw = iwork !! 16,
        leniw = iwork !! 17,
        mused = iwork !! 18,
        mcur = iwork !! 19,
        hu = rwork !! 10,
        hcur = rwork !! 11,
        tcur = rwork !! 12,
        tolsf = rwork !! 13,
        tsw = rwork !! 14
      }
  where iwork = map fromIntegral iwork'

lsodaDoneMsg :: String
lsodaDoneMsg = "Finished!"


-- TODO
-- max nonstiff order = iwork[7]= 12
-- max stiff order = iwork[8]=5
-- maxstep = 0 (means infinity to LSODA)
-- minstep = 0 
-- nsteps = 500
-- first_step = 0 (automatic step determination

-- iwork[5] = 500 .... why?

simpLsodaAux :: FFun -> [Double] -> TimeSpec -> IO LSODARes
simpLsodaAux ffun y0 (StartStop tStart tEnd) = do
  let neq = length y0
  let maxOrderNonStiff = 12 -- the default, here made explicit
  let maxOrderStiff = 5 -- the default, here made explicit
  let jtVal = 2
  let nSteps = 500 -- the default, here made explicit
  let lrn = 20 + (maxOrderNonStiff+4) * neq -- length of rwork for nonstiff mode
  let lrs = if jtVal `elem` [1,2]
                then 22 + (maxOrderStiff + 4) * neq + neq * neq -- length of rwork for    stiff mode
                else error "Invalid! I can only handle jt=2"
  let lrw = max lrn lrs
  let liw = 20 + neq
  -- task 1 means integrate up to tOut, by overshoot+interpolation.
  -- task 2 means one step only and return
  -- task 5 means take a variable sized step towards tOut, but don't overshoot tCrit
  let task = 5
  -- tol, or itol, is the type of error control
  let tol = 1
  fPtr <- wrapFFun ffun
  neqPtr <- new $ fromIntegral neq
  iWorkPtr <- newArray $ replicate liw 0
  rWorkPtr <- newArray $ replicate lrw 0
  yPtr <- newArray y0
  tPtr <- new tStart
  tOutPtr <- new tEnd -- it is not clear to me if this matters much
  tolPtr <- new tol
  rTolPtr <- new 1e-3
  aTolPtr <- newArray $ replicate neq 1e-6
  iTaskPtr <- new task :: IO (Ptr FInt)
  poke rWorkPtr tEnd -- `tCrit` must be the value in the first index in `rwork`
  poke (plusPtr iWorkPtr 5 :: Ptr FInt) $ fromIntegral nSteps
  poke (plusPtr iWorkPtr 7 :: Ptr FInt) $ fromIntegral maxOrderNonStiff
  poke (plusPtr iWorkPtr 8 :: Ptr FInt) $ fromIntegral maxOrderStiff
  iStatePtr <- new 1 -- state 1 = making the first call
  iOptPtr <- new 0 -- no optional arguments
  lrwPtr <- new $ fromIntegral lrw
  liwPtr <- new $ fromIntegral liw
  jtPtr <- new jtVal
  let jacDummyPtr = nullFunPtr -- since I use jt=2, the jacobian can be a dummy argument. e.g. a null pointer
  -- step1
  -- PRE. initialize with success = True
  let step1 :: LSODARes -> IO LSODARes
      step1 res@LSODARes {ys = yOuts, ts = tsThisFar} = do
        -- take a step
        lsoda' fPtr neqPtr yPtr tPtr tOutPtr tolPtr rTolPtr aTolPtr iTaskPtr iStatePtr iOptPtr rWorkPtr lrwPtr iWorkPtr liwPtr jacDummyPtr jtPtr
        -- find out how it went
        iState <- peek iStatePtr
        t <- peek tPtr
        yNew <- peekArray neq yPtr
        -- printf "t %f  " =<< peek tPtr
        -- printf "iwork(11) %d  " =<< peek (plusPtr iWorkPtr 10 :: Ptr Int)
        -- printf "iwork(12) %d  " =<< peek (plusPtr iWorkPtr 11 :: Ptr Int)
        -- printf "istate %d \n" =<< peek iStatePtr
        -- print =<< liftA2 parseOptOutputs (peekArray liw iWorkPtr) (peekArray lrw rWorkPtr)
        if iState /= 2
          then return res {msg = printf "Stopped. istate =%d at t=%f" iState t, success = False}
          else
            if t == tEnd
              then return res {ys = yOuts ++ [yNew], ts = tsThisFar ++ [t], msg = lsodaDoneMsg}
              else step1 res {ys = yOuts ++ [yNew], ts = tsThisFar ++ [t], msg = "Completed a step"}

  finalVal <- step1 res0 {ys = [y0], ts = [tStart], success = True, msg = ""}

  rwork <- peekArray lrw rWorkPtr
  iwork <- peekArray liw iWorkPtr
  return
    finalVal {optOutput = parseOptOutputs iwork rwork}
