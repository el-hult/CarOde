# Car Ode example

This project explores how to use a stiff solver written in fortran, to solve car physics simulations according to the outline in https://www.asawicki.info/Mirror/Car%20Physics%20for%20Games/Car%20Physics%20for%20Games.html Its purpose is to be a note for my memory

The issue with the simulation rules presented in that blog post is that they are stiff. It is alluded to in the post, and they claim they solved it with a RK$ integrator. However, I did not have any luck achieving acceptable performance with that, and had to use some stiff solver. After looking around ad ODE solvers for Haskell, I decided to do like `scipy.integrate.solve_ivp` and interface with ODEPACK https://computing.llnl.gov/projects/odepack/software , specifically the adaptive solver LSODA which chooses between a stiff and a nonstiff solver as needed.

The result is plotted with `hvega`, as it seemed to be a powerful plotting solution, with minimal dependencies. 

# Installation

The steps below work on my Windows 10 machine, but I am quite certain that you need to do something else on other OS'es.

I ran `conda install -c conda-forge mingw lapack blas` to get gfortran, blas and lapack installed into my `base` conda environment.

The ODEPACK library files is located in `odepack_scipy` is simply the source files from scipy https://github.com/scipy/scipy/tree/a06cc0da56df9741105eedcd160b77f6f08236e1/scipy/integrate/odepack . Compile them by running 
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
to produce a static library file that contains the compiled fortran code. The library is an archive file (`*.a`), and you can inspect it with e.g. `nm` or `objdump` to find all the `.o`-files inside it.

On non-windows, you might need the `-fPIC` in the calls to `gfortran`, but on Windows it just gives a warning. See https://stackoverflow.com/questions/16708148/fpic-ignored-for-target-all-code-is-position-independent-useless-warning

Next check the `CarOde.cabal`-file, and make sure that the `extra-lib-dirs` section is ok. You need to provide paths to `libblas`, `liblapack`, `libgfortran` and the above compiled library as well, which should be located in `./bin`.

After that, you should just call `cabal run` and it should be all!

# Development

This code is a wild mess. But it was a good learning experience!

The fortran code is precompiled into a library archive fileas described above. All fortran symbol names re mangeled, so the fortran subroutine `LSODA` is called `lsoda_` in the compiled output.

The Haskell code interfaces to that library via C-FFI. It needs to pass floats, ints, and pointers. The floats are explicitly defined to be `double precision` floats in the fortran code, but the fortran integers are simply `integer`, so compiler options could change the integer type. Default is 32 bit, so that is what I code against in the Haskell code. However, one might have to be careful about bugs here.

All arguments to subroutines are passed reference. So there is a LOT of pointers passed around. The `LSODA` module tries to abstract that away. The client code should work with the `simpLsoda` function only.

One curious thing in the code is that the fortran code wants a function pointers to evaluate the righ hand side of the ODE. Haskell functions are boxed since they are to be used in the Haskell Runtime System, so one must wrap those functions in a C-FFI-wrapper, which produces a function pointer. This is implemented by the functions `wrapJacFun` and `wrapFFun`. These are not exported from the `LSODA` module, so the client code need not care, but the module code must.

Most work with pointers requires the IO monad. But since I allocate new memory for each call so `simpLsoda`, and never allow anyone access to that memory, I unwrap the `IO` by `System.IO.Unsafe.unsafePerformIO`. This should be fine since there should be no side effects. But be weary; if you print-debug in that code section, the output can become quite jumbeled.

# Known issues

The fortran code uses a global variable called `ls0001` to pass data between subroutines. In the `libodepack.a`, you can therefore find the symbol `ls0001_` exported several times, under the `common`-symbol type. This should not be an issue, as the linker is supposed to handle this. However, there is a bug in the dynamic linker of GHCi on Windows, so GHCi is unable to use this library interactively. I wrote a message on the GHC bug tracker about this https://gitlab.haskell.org/ghc/ghc/-/issues/6107 to show that this is an active bug.

# Alternative solutions

## ODE solving

1. `hmatrix-gsl` https://hackage.haskell.org/package/hmatrix-gsl 
seems to be a nice alternative, that provides adams/bdf solver for stiff problems.
There are two issues.
Firstly, it provides no switching solver and my explorations in numpy/scipy indicated that simple BDF was too inefficient for this problem.
Secondly, it needs gsl installed. On my windows machine, that seemed like a no-go.

2. `numeric-ode` https://hackage.haskell.org/package/numeric-ode 
This package is underdeveloped and clashes with modern versions of Base, so it is a no-go.
It would provide some implicit and implicit solvers, as well as symplectic euler and Störmer-Verlet,
and I think a symplectic solver could be valuable for working with rotation of the car while turning.
The lib is implemented in Haskell, so it is quite nice for inspiration.

## Plotting

Here, there were a couple of variants thinkable

1. Chart  https://hackage.haskell.org/package/Chart
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