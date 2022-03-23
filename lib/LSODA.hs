{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- for wrapJacFun
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module LSODA
  ( simpLsoda,
    LSODARes (..),
    OptOut (..),
    RHS (..),
    TimeSpec (..),
    TolSpec (..),
  )
where

import Data.Int
import Data.Proxy (Proxy (..))
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.TypeNats (KnownNat, Nat, natVal)
import qualified Numeric.LinearAlgebra.Data as LAD
import Numeric.LinearAlgebra.Static
import System.IO.Unsafe
import Text.Printf

-- | Fortran int.
-- the exact type is dependant on the compiler options
-- In my case, it cmpiled to single precision ints.
type FInt = Int32

-- | The right hand side function f in a ODE
-- y' = f(t,y)
newtype RHS (n :: Nat) = MkRHS {unRHS :: Double -> R n -> R n}

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

-- | A function that would be needed if an analytical jacobian was available
foreign import ccall "wrapper" wrapJacFun :: JacFun -> IO (FunPtr JacFun)

data LSODARes (n :: Nat) = LSODARes
  { ys :: [R n],
    ts :: [Double],
    success :: Bool,
    msg :: String,
    optOutput :: OptOut
  }
  deriving (Show)

res0 :: LSODARes n
res0 =
  LSODARes
    { ys = [],
      ts = [],
      success = True,
      msg = "Not even started",
      optOutput = oo0
    }

-- | A helper that takes care of the pointer work
-- PRE the neq supplied in calling the FFun must be the same as the type level n
-- in defining R n
fprimWrapper :: forall n. KnownNat n => (Double -> R n -> R n) -> FFun
fprimWrapper fprim neqPtr tPtr yPtr yDotPtr = do
  neq <- peek neqPtr
  yList <- peekArray (fromIntegral neq) yPtr
  let yVector = vector yList :: R n -- assumes 'n == neq'
  t <- peek tPtr
  let ydot = fprim t yVector
  pokeArray yDotPtr (LAD.toList (unwrap ydot))

data TimeSpec
  = -- | the start end end of the time interval. Indicates that the optimizer decides on step length
    StartStop Double Double
    | -- | A list of time points, for which we want to get the results
    TSpace [Double]

-- | Tolerance specification
-- Sensible default is 'TolS 1e-3 1e-6'
data TolSpec (n :: Nat)
  = -- | A scalar absolute . Use if you have the same tolerance for all components
    -- 'TolS rtol atol'
    -- encodes relative and absolute tolerance
    TolS Double Double
  | -- | A vector. Use if you have a specific tolerance for each component
    -- 'TolV rtol vec'
    -- a scalar relative tolerance, and a specific absolute tolerance per component
    TolV Double (R n)

-- | a simplified LSODA API for my specific use case :)
simpLsoda ::
  KnownNat n =>
  -- | The right hand side in the ODE
  (Double -> R n -> R n) ->
  -- | The initial state
  R n ->
  TimeSpec ->
  -- | use 'TolS 1e-6' to use same default as numpy
  TolSpec n ->
  LSODARes n
{-# NOINLINE simpLsoda #-}
simpLsoda ffun y0 ttt tol = unsafePerformIO $ do
  let fex = fprimWrapper ffun
  simpLsodaAux fex y0 ttt tol

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
    mused :: Int,
    -- | mcur    iwork(20) the current method indicator..
    --                   1 means adams (nonstiff), 2 means bdf (stiff).
    --                   this is the method to be attempted
    --                   on the next step.  thus it differs from mused
    --                   only if a method switch has just been made.
    mcur :: Int,
    -- | hu      rwork(11) the step size in t last used (successfully).
    hu :: Double,
    -- | hcur    rwork(12) the step size to be attempted on the next step.
    hcur :: Double,
    -- | tcur    rwork(13) the current value of the independent variable
    --                    which the solver has actually reached, i.e. the
    --                    current internal mesh point in t.  on output, tcur
    --                    will always be at least as far as the argument
    --                    t, but may be farther (if interpolation was done).
    tcur :: Double,
    -- | tolsf   rwork(14) a tolerance scale factor, greater than 1.0,
    --                    computed when a request for too much accuracy was
    --                    detected (istate = -3 if detected at the start of
    --                    the problem, istate = -2 otherwise).  if itol is
    --                    left unaltered but rtol and atol are uniformly
    --                    scaled up by a factor of tolsf for the next call,
    --                    then the solver is deemed likely to succeed.
    --                    (the user may also ignore tolsf and alter the
    --                    tolerance parameters in any other way appropriate.)
    tolsf :: Double,
    -- | tsw     rwork(15) the value of t at the time of the last method
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
  where
    iwork = map fromIntegral iwork'

lsodaDoneMsg :: String
lsodaDoneMsg = "Finished!"

-- | Implementation of all the pointer-passing-around-stuff
simpLsodaAux ::
  forall n.
  KnownNat n =>
  FFun ->
  R n ->
  TimeSpec ->
  TolSpec n ->
  IO (LSODARes n)
simpLsodaAux ffun y0 timeSpec tolSpec = do
  let neq = fromIntegral $ natVal (Proxy :: Proxy n)
  let maxOrderNonStiff = 12 -- the default, here made explicit
  let maxOrderStiff = 5 -- the default, here made explicit
  let jtVal = 2 -- compute jacobian by numerical approximation
  let nSteps = 500 :: FInt -- the default, here made explicit
  let lrn = 20 + (maxOrderNonStiff + 4) * neq -- length of rwork for nonstiff mode
  let lrs =
        if jtVal `elem` [1, 2]
          then 22 + (maxOrderStiff + 4) * neq + neq * neq -- length of rwork for    stiff mode
          else error "Invalid! I can only handle jt=2"
  let lrw = max lrn lrs
  let liw = 20 + neq
  let task = case timeSpec of 
               (TSpace _) -> 1 -- task 1 means integrate up to tOut, by overshoot+interpolation.
               (StartStop _ _) -> 5 -- task 5 means take a variable sized step towards tOut, but don't overshoot tCrit
  --itol   = 1 or 2 according as atol (below) is a scalar or array.
  let itol = case tolSpec of
        TolS _ _ -> 1
        TolV _ _ -> 2

  fPtr <- wrapFFun ffun
  neqPtr <- new $ fromIntegral neq
  iWorkPtr <- newArray $ replicate liw 0
  rWorkPtr <- newArray $ replicate lrw 0
  yPtr <- newArray $ LAD.toList $ unwrap y0
  let (tStart,tEnd) = case timeSpec of
                      (StartStop tStart' tEnd') -> (tStart',tEnd')
                      (TSpace (tStart':tEnd':_)) ->(tStart',tEnd')
                      _ -> error "The TSpace has less than 2 elements!"
  tPtr <- new tStart
  tOutPtr <- new tEnd
  itolPtr <- new itol
  rTolPtr <- case tolSpec of
    TolS rtol _ -> new rtol
    TolV rtol _ -> new rtol
  aTolPtr <- case tolSpec of
    TolS _ atol -> new atol
    TolV _ atol -> newArray $ LAD.toList $ unwrap atol
  iTaskPtr <- new task :: IO (Ptr FInt)
  poke rWorkPtr tEnd -- `tCrit` must be the value in the first index in `rwork`
  poke (plusPtr iWorkPtr 5 :: Ptr FInt) nSteps
  poke (plusPtr iWorkPtr 7 :: Ptr FInt) $ fromIntegral maxOrderNonStiff
  poke (plusPtr iWorkPtr 8 :: Ptr FInt) $ fromIntegral maxOrderStiff
  iStatePtr <- new 1 -- state 1 = making the first call
  iOptPtr <- new 0 -- no optional arguments
  lrwPtr <- new $ fromIntegral lrw
  liwPtr <- new $ fromIntegral liw
  jtPtr <- new jtVal
  let jacDummyPtr = nullFunPtr -- since I use jt=2, the jacobian can be a dummy argument. e.g. a null pointer
  let step1 :: [Double] -> LSODARes n -> IO (LSODARes n)
      step1 [] res = return res{msg = lsodaDoneMsg}
      step1 (nextT:times) res@LSODARes {ys = yOuts, ts = tsThisFar} = do
        poke tOutPtr nextT
        lsoda' fPtr neqPtr yPtr tPtr tOutPtr itolPtr rTolPtr aTolPtr iTaskPtr iStatePtr iOptPtr rWorkPtr lrwPtr iWorkPtr liwPtr jacDummyPtr jtPtr
        iState <- peek iStatePtr
        t <- peek tPtr
        yNew <- vector <$> peekArray neq yPtr
        if iState /= 2
          then return res {msg = printf "Stopped. istate=%d at t=%f, which means %s" iState t (iStateToMsg iState), success = False}
          else step1 times res {ys = yOuts ++ [yNew], ts = tsThisFar ++ [t], msg = "Completed a step"}
  let step5 :: LSODARes n -> IO (LSODARes n)
      step5 res@LSODARes {ys = yOuts, ts = tsThisFar} = do
        lsoda' fPtr neqPtr yPtr tPtr tOutPtr itolPtr rTolPtr aTolPtr iTaskPtr iStatePtr iOptPtr rWorkPtr lrwPtr iWorkPtr liwPtr jacDummyPtr jtPtr
        iState <- peek iStatePtr
        t <- peek tPtr
        yNew <- vector <$> peekArray neq yPtr
        if iState /= 2
          then return res {msg = printf "Stopped. istate=%d at t=%f, which means %s" iState t (iStateToMsg iState), success = False}
          else
            if t == tEnd
              then return res {ys = yOuts ++ [yNew], ts = tsThisFar ++ [t], msg = lsodaDoneMsg}
              else step5 res {ys = yOuts ++ [yNew], ts = tsThisFar ++ [t], msg = "Completed a step"}

  finalVal <- case timeSpec of
    (TSpace times) -> step1 (tail times) res0 {ys = [y0], ts = [tStart], success = True, msg = "Uninitialized"}
    (StartStop _ _) -> step5 res0 {ys = [y0], ts = [tStart], success = True, msg = "Uninitialized"}

  rwork <- peekArray lrw rWorkPtr
  iwork <- peekArray liw iWorkPtr
  return
    finalVal {optOutput = parseOptOutputs iwork rwork}

iStateToMsg :: FInt -> String
iStateToMsg 1 = "nothing was done, as tout was equal to t with istate = 1 on input.  (however, an internal counter was set to detect and prevent repeated calls of this type.)"
iStateToMsg 2 = "the integration was performed successfully."
iStateToMsg (-1) = "an excessive amount of work (more than mxstep steps) was done on this call, before completing the requested task, but the integration was otherwise successful as far as t.  (mxstep is an optional input and is normally 500.)  to continue, the user may simply reset istate to a value .gt. 1 and call again (the excess work step counter will be reset to 0). in addition, the user may increase mxstep to avoid this error return (see below on optional inputs)."
iStateToMsg (-2) = "too much accuracy was requested for the precision of the machine being used.  this was detected before completing the requested task, but the integration was successful as far as t.  to continue, the tolerance parameters must be reset, and istate must be set to 3.  the optional output tolsf may be used for this purpose.  (note.. if this condition is detected before taking any steps, then an illegal input return (istate = -3) occurs instead.)"
iStateToMsg (-3) = "illegal input was detected, before taking any integration steps.  see written message for details. note..  if the solver detects an infinite loop of calls to the solver with illegal input, it will cause the run to stop."
iStateToMsg (-4) = "there were repeated error test failures on one attempted step, before completing the requested task, but the integration was successful as far as t. the problem may have a singularity, or the input may be inappropriate."
iStateToMsg (-5) = "there were repeated convergence test failures on one attempted step, before completing the requested task, but the integration was successful as far as t. this may be caused by an inaccurate jacobian matrix, if one is being used."
iStateToMsg (-6) = "ewt(i) became zero for some i during the integration.  pure relative error control (atol(i)=0.0) was requested on a variable which has now vanished. the integration was successful as far as t."
iStateToMsg (-7) = "the length of rwork and/or iwork was too small to proceed, but the integration was successful as far as t. this happens when lsoda chooses to switch methods but lrw and/or liw is too small for the new method."
iStateToMsg _ = error "This iState should never happen!!!"