{-# LANGUAGE ForeignFunctionInterface #-}

module LSODA where
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Marshal.Utils
import Foreign.Storable

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
