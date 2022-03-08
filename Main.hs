{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Int
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Marshal.Utils
import Foreign.Storable
import Text.Printf
import Control.Monad

type Vector = [Double]

type Matrix = [[Double]]

main :: IO ()
main = do
  print "fNorm example:"
  let a =
        [ [2.1, 1.3],
          [1.2, 4.2]
        ]
      w = [1.0, 1.0]
  norm <- fNorm a w
  print norm
  print "lsoda example:"
  runLSODA

fNorm :: Matrix -> Vector -> IO Double
fNorm a w = do
  n <- newArray [length w]
  p1 <- newArray $ concat a
  p2 <- newArray w
  fNorm' n p1 p2

foreign import ccall unsafe "fnorm_"
  fNorm' :: Ptr Int -> Ptr Double -> Ptr Double -> IO Double

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

fex :: FFun
fex neqPtr tPtr yPtr yDotPtr= do 
    [y0,y1,y2] <- peekArray 3 yPtr
    let ydot0 = -0.4E0*y0 + 1.0E4*y1*y2;
        ydot2 = 3.0e7*y1*y1;
        ydot1 = -ydot0 - ydot2;
    -- printf "hej. dots! %f %f %f" ydot0 ydot1 ydot2
    pokeArray yDotPtr [ydot0,ydot1,ydot2]


runLSODA :: IO ()
runLSODA = do
    let neqVal = 3
    let lrwVal = 70
    let liwVal = 23
    fPtr <- wrapFFun fex
    neqPtr <- new neqVal
    iWorkPtr <- newArray $ replicate liwVal 0
    rWorkPtr <- newArray $ replicate lrwVal 0
    yPtr <- newArray [1,0,0]
    tPtr <- new 0
    tOutPtr <- new 0.4
    iTolPtr <- new 2
    rTolPtr <- new 1e-4
    aTolPtr <- newArray [1e-6,1e-10,1e-6]
    iTaskPtr <- new 1
    iStatePtr <- new 1
    iOptPtr <- new 0
    lrwPtr <- new lrwVal
    liwPtr <- new liwVal
    jtPtr <- new 2
    let jacDummyPtr =nullFunPtr  -- since I use jt=2, the jacobian can be a dummy argument. e.g. a null pointer
    let step1 k = do
        lsoda' fPtr neqPtr yPtr tPtr tOutPtr iTolPtr rTolPtr aTolPtr iTaskPtr iStatePtr iOptPtr rWorkPtr lrwPtr iWorkPtr liwPtr jacDummyPtr jtPtr
        iState <- peek iStatePtr
        t <- peek tPtr
        tOut <- peek tOutPtr
        [y1,y2,y3] <- peekArray neqVal yPtr
        printf "at t = %-12.4e  y = %14.6e  %14.6e  %14.6e\n" t y1 y2 y3
        if iState < 0
            then printf "error halt.. istate =%d" iState
            else poke tOutPtr (tOut*10)
        when (k <= 12) $ step1 (k+1)

    step1 1

    rwork <- peekArray lrwVal rWorkPtr
    iwork <- peekArray liwVal iWorkPtr
    putStrLn ""
    printf "no. steps =%4d  no. f-s =%4d  no. j-s =%3d\nmethod last used =%2d last switch was at t =%e\n"         (iwork !! 10) ( iwork !!11)( iwork !!12)( iwork !!18) (rwork !!14)

