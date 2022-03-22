# Car Ode example

This project explores how to use a stiff solver written in fortran, to solve car physics simulations according to the outline in https://www.asawicki.info/Mirror/Car%20Physics%20for%20Games/Car%20Physics%20for%20Games.html Its purpose is to be a note for my memory

The issue with the simulation rules presented in that blog post is that they are stiff. It is alluded to in the post, and they claim they solved it with a RK$ integrator. However, I did not have any luck achieving acceptable performance with that, and had to use some stiff solver. After looking around ad ODE solvers for Haskell, I decided to do like `scipy.integrate.solve_ivp` and interface with ODEPACK https://computing.llnl.gov/projects/odepack/software , specifically the adaptive solver LSODA which chooses between a stiff and a nonstiff solver as needed.

The result is plotted with `hvega`, as it seemed to be a powerful plotting solution, with minimal dependencies. 

I wanted the API to work with statically sized vectors. At first, I tried the `Linear` package. But that was honestly quite hard to work with. Therefore, I swapped over to using `hmatrix` instead. Which is quite nice. More readings on the topics include these:

- https://en.wikibooks.org/wiki/Haskell/GADT
- https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html 
- https://blog.jle.im/entry/practical-dependent-types-in-haskell-2.html 
- https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/type_literals.html
- https://stackoverflow.com/questions/28068129/how-to-use-linear-v-to-write-static-type-checked-matrix-operations-haskell

# Installation (wsl2)

```bash
sudo apt-get update
sudo apt install gfortran libblas-dev liblapack-dev
rm -rf bin
mkdir bin
gfortran -c -std=legacy odepack_scipy/lsoda.f -o bin/lsoda.o
gfortran -c -std=legacy odepack_scipy/vode.f -o bin/vode.o
gfortran -c -std=legacy odepack_scipy/srcma.f -o bin/srcma.o
gfortran -c -std=legacy odepack_scipy/xerrwv.f -o bin/xerrwv.o
gfortran -c -std=legacy odepack_scipy/prja.f -o bin/prja.o
gfortran -c -std=legacy odepack_scipy/intdy.f -o bin/intdy.o
gfortran -c -std=legacy odepack_scipy/vmnorm.f -o bin/vmnorm.o
gfortran -c -std=legacy odepack_scipy/fnorm.f -o bin/fnorm.o
gfortran -c -std=legacy odepack_scipy/ewset.f -o bin/ewset.o
gfortran -c -std=legacy odepack_scipy/bnorm.f -o bin/bnorm.o
gfortran -c -std=legacy odepack_scipy/solsy.f -o bin/solsy.o
gfortran -c -std=legacy odepack_scipy/stoda.f -o bin/stoda.o
gfortran -c -std=legacy odepack_scipy/cfode.f -o bin/cfode.o
ar rcs bin/libodepack.a bin/*.o
rm bin/*.o
cabal run
```


# Development

This code is a wild mess. But it was a good learning experience!

The fortran code is precompiled into a library archive fileas described above. All fortran symbol names re mangeled, so the fortran subroutine `LSODA` is called `lsoda_` in the compiled output.

The Haskell code interfaces to that library via C-FFI. It needs to pass floats, ints, and pointers. The floats are explicitly defined to be `double precision` floats in the fortran code, but the fortran integers are simply `integer`, so compiler options could change the integer type. Default is 32 bit, so that is what I code against in the Haskell code. However, one might have to be careful about bugs here.

All arguments to subroutines are passed reference. So there is a LOT of pointers passed around. The `LSODA` module tries to abstract that away. The client code should work with the `simpLsoda` function only.

One curious thing in the code is that the fortran code wants a function pointers to evaluate the righ hand side of the ODE. Haskell functions are boxed since they are to be used in the Haskell Runtime System, so one must wrap those functions in a C-FFI-wrapper, which produces a function pointer. This is implemented by the functions `wrapJacFun` and `wrapFFun`. These are not exported from the `LSODA` module, so the client code need not care, but the module code must.

Most work with pointers requires the IO monad. But since I allocate new memory for each call so `simpLsoda`, and never allow anyone access to that memory, I unwrap the `IO` by `System.IO.Unsafe.unsafePerformIO`. This should be fine since there should be no side effects. But be weary; if you print-debug in that code section, the output can become quite jumbeled.

# Alternative solutions

## ODE solving

1. `hmatrix-gsl` https://hackage.haskell.org/package/hmatrix-gsl 
seems to be a nice alternative, that provides adams/bdf solver for stiff problems.
There are two issues.
Firstly, it provides no switching solver and my explorations in numpy/scipy indicated that simple BDF was too inefficient for this problem.
Secondly, it needs gsl installed. On my windows machine, that seemed like a no-go. I did start the project on my windows machine.

2. `numeric-ode` https://hackage.haskell.org/package/numeric-ode 
This package is underdeveloped and clashes with modern versions of Base, so it is a no-go.
It would provide some implicit and implicit solvers, as well as symplectic euler and Störmer-Verlet,
and I think a symplectic solver could be valuable for working with rotation of the car while turning.
The lib is implemented in Haskell, so it is quite nice for inspiration.

## Plotting

Here, there were a couple of variants thinkable

1. Chart  https://hackage.haskell.org/package/Chart
However, you need some backend.It seems that the package Chart-cairo is the most versatile, but it needs cairo installed
I started the project on windows and that seems like a mess on a windows machine. see e.g. https://stackoverflow.com/questions/9526375/how-to-install-cairo-on-windows

2. Matplotlib https://hackage.haskell.org/package/matplotlib
I am familiar with matplotlib on python, but it seems that it is a pain to install with tons of dependencies.
So I would prefer to NOT use this.
It is also a quite stateful API, which seems terrible to port to haskell.

3. Gloss https://hackage.haskell.org/package/gloss 
I have fiddeled a little in Gloss, and it seemed like a a fun experience to implement a simple graphing library.
One variant could be to implement a Chart-backend in Gloss, so that I get all nice power of Chart, but with the dependency on Gloss instead, which
is a 'solved problem' for me.
But it is too much workto go this route, I worry.