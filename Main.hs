
import Data.Int

import Text.Printf
import Control.Monad

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Marshal.Utils
import Foreign.Storable
import LSODA

main :: IO ()
main = do
  print "lsoda example:"
  runLSODA


fex :: FFun
fex neqPtr tPtr yPtr yDotPtr= do 
    [y0,y1,y2] <- peekArray 3 yPtr
    let ydot0 = -0.4E0*y0 + 1.0E4*y1*y2;
        ydot2 = 3.0e7*y1*y1;
        ydot1 = -ydot0 - ydot2;
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
    printf "no. steps =%4d  no. f-s =%4d  no. j-s =%3d\nmethod last used =%2d last switch was at t =%e\n" (iwork !! 10) ( iwork !!11)( iwork !!12)( iwork !!18) (rwork !!14)

