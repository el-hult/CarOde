# Car Ode example

This project explores how to use a stiff solver written in fortran, to solve car physics simulations according to the outline in https://www.asawicki.info/Mirror/Car%20Physics%20for%20Games/Car%20Physics%20for%20Games.html

The issue with the simulation rules presented in that blog post is that they are stiff. It is alluded to in the post, and they claim they solved it with a RK$ integrator. However, I did not have any luck achieving acceptable performance with that, and had to use some stiff solver. After looking around ad ODE solvers for Haskell, I decided to do like `scipy.integrate.solve_ivp` and interface with ODEPACK https://computing.llnl.gov/projects/odepack/software , specifically the adaptive solver LSODA which chooses between a stiff and a nonstiff solver as needed.

The result is plotted with `hvega`, as it seemed to be a powerful plotting solution, with minimal dependencies. 

# Installation

I did not check that this method is portable to other OS'es, but this is what I did...

I ran `conda install -c conda-forge mingw lapack blas` to get gfortran, blas and lapack.

The ODEPACK library files is located in `odepack_scipy` is simply the source files from scipy https://github.com/scipy/scipy/tree/a06cc0da56df9741105eedcd160b77f6f08236e1/scipy/integrate/odepack . Compile then by running 

```powershell
rm bin -recurse -force
mkdir bin
gfortran -g -c odepack_numpy/lsoda.f -o bin/lsoda.o
gfortran -g -c odepack_numpy/vode.f -o bin/vode.o
gfortran -g -c odepack_numpy/srcma.f -o bin/srcma.o
gfortran -g -c odepack_numpy/xerrwv.f -o bin/xerrwv.o
gfortran -g -c odepack_numpy/prja.f -o bin/prja.o
gfortran -g -c odepack_numpy/intdy.f -o bin/intdy.o
gfortran -g -c odepack_numpy/vmnorm.f -o bin/vmnorm.o
gfortran -g -c odepack_numpy/fnorm.f -o bin/fnorm.o
gfortran -g -c odepack_numpy/ewset.f -o bin/ewset.o
gfortran -g -c odepack_numpy/bnorm.f -o bin/bnorm.o
gfortran -g -c odepack_numpy/solsy.f -o bin/solsy.o
gfortran -g -c odepack_numpy/stoda.f -o bin/stoda.o
gfortran -g -c odepack_numpy/cfode.f -o bin/cfode.o
ar rcs bin/libodepack.a bin/*.o
rm bin/*.o
cabal run
```

to produce a static library file that contains the compiled fortran code. It is an archive file (`*.a`), and you can inspect it with e.g. `nm` or `objdump`.

On non-windows, you might need `-fPIC` as well, but on windows it just gives a warning. See https://stackoverflow.com/questions/16708148/fpic-ignored-for-target-all-code-is-position-independent-useless-warning

Next check the `CarOde.cabal`-file, and make sure that the `extra-lib-dirs` section is ok. You need to provide paths to `libblas`, `liblapack`, `libgfortran` and the above compiled library as well, which should be located in `./bin`.

After that, you should just call `cabal run` and it should be all!

# Development

This code is a wild mess. But it was a good learning experience!

The fortran code is precompiled into a library archive file.
The Haskell code interfaces via C-FFI. It needs to pass doubles and intes. The doubles are explicitly double precision floats, but the fortran integers are "integer", so compiler options could make then 64-bit instead of the standard 32-bit. So that is a thing to look out for.

The fortran code passes everything by references all the time, so the code is a pointer mess. But it seems workable. So all that is hidden in the LSODA module, and the user should just work with the `simpLsoda` function only.

One curious thing in the code is that the fortran code want function pointers to evaluate the righ hand side of the ODE. To achieve this, you must wrap the haskell function (which works in the GHC RTS) with a nicer wrapper, that works in the C-FFI context. This is implemented by the functions `wrapJacFun` and `wrapFFun`

All work with pointers requires the IO monad, so the code runs in that. But since I allocate new memory for each call so `simpLsoda`, and never allow anyone access to that memory, I gave it access to `unsafePerformIO`. This should be fine, but PLEASE to not make any other IO shenanigans in that context. e.g. printing. It can really mess up the output.

# Known issues

The fortran code uses a global variable called `ls0001` to pass data between subroutines. In the compiled object files, this means that this symbol is repeatedly exported. This should not be an issue, and the linker should handle this. However, there is a bug in the dynamic linked of GHCi on windows, so GHCi is impossible to use with this library, on windows.

I wrote a short addition on the GHC bug tracker about this https://gitlab.haskell.org/ghc/ghc/-/issues/6107 to show that this is an active bug.

# Alternative solutions

## ODE solving

1. `hmatrix-gsl` https://hackage.haskell.org/package/hmatrix-gsl 
seems to be a nice alternative, that provides adams/bdf solver for stiff problems.
However, it provides no switching solver (which some explorations in numpy/scipy indicated was too inefficient) 
and it needs gsl installed. And on my windows machine, that seemed like a no-go.

2. `numeric-ode` https://hackage.haskell.org/package/numeric-ode 
package is underdeveloped and clashes with modern versions of Base.
So that one is off the table as well.
It would provide some implicit and implicit solvers, as well as symplectic euler and Störmer-Verlet 
It is implemented in Haskell, so it is quite nice for inspiration.


## Plotting

Here, there were a couple of variants thinkable

1. Chart.  https://hackage.haskell.org/package/Chart
However, you need some backend.It seems that the package Chart-cairo is the most versatile, but it needs cairo installed
And that seems like a mess on a windows machine. see e.g. https://stackoverflow.com/questions/9526375/how-to-install-cairo-on-windows

2. Matplotlib https://hackage.haskell.org/package/matplotlib
I am familiar with matplotlib on python, but it seems that it is a pain to install with tons of dependencies.
So I would prefer to NOT use this.
It is also a quite stateful API, which seems terrible to port to haskell.

3. Gloss https://hackage.haskell.org/package/gloss 
I have fiddeled a little in Gloss, and it seemed like a a fun experience to implement a simple graphing library.
One variant could be to implement a Chart-backend in Gloss, so that I get all nice power of Chart, but with the dependency on Gloss instead, which
is a 'solved problem' for me.
But it is too much workto go this route, I worry.