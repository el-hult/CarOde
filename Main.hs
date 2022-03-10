import LSODA
import Text.Printf

main :: IO ()
main = do
  print "lsoda example:"
  runLSODA

-- PRE y has length 3
-- TODO move to some sized vector type
fprim :: RHS
fprim neq t y = 
    let y0:y1:y2:_ = y
        ydot0 = -0.4E0 * y0 + 1.0E4 * y1 * y2
        ydot2 = 3.0e7 * y1 * y1
        ydot1 = -ydot0 - ydot2
    in [ydot0, ydot1, ydot2]


runLSODA :: IO ()
runLSODA = do
  let ts = [4 * 10 ** m | m <- [-1 .. 11]]
  let res = simpLsoda fprim [1, 0, 0] (StartStop 0 4e11)
  let LSODARes {success = didSucceed, ts = ts, ys = ys} = res
  if didSucceed
    then
      ( do
          mapM_ (\(t, y) -> printf "at t = %-12.4e  y = %14.6e  %14.6e  %14.6e\n" t (head y) (y !! 1) (y !! 2)) $ zip ts ys
          printf "no. steps =%4d  no. f-s =%4d  no. j-s =%3d\nmethod last used =%2d last switch was at t =%e\n" (noStepsTaken res) (noFs res) (noJs res) (lastMethodUsed res) (lastSwitchedAt res)
      )
    else print "Failed solving"
