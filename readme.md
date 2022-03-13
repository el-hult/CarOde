The content of the folder `odepack_numpy` is simply the source files from scipy https://github.com/scipy/scipy/tree/a06cc0da56df9741105eedcd160b77f6f08236e1/scipy/integrate/odepack 

I assume you have conda. So run 
`conda install -c conda-forge mingw lapack blas` 
to get gfortran, blas and lapack.

Compile the fortran code and put it in a static library.
```
mkdir bin
gci .\odepack_numpy\*.f -Exclude xset* | %{ gfortran -c $_.FullName -o "bin/$($_.BaseName).o" -g}  # compile the files with all debug flags
ar rcs bin\libodepack.a .\bin\*.o  # make a static library/archive. this is not really needed, but feels neat.
rm  .\bin\*.o
```
On non-windows, you might need `-fPIC` as well, but on windows it just gives a warning. See https://stackoverflow.com/questions/16708148/fpic-ignored-for-target-all-code-is-position-independent-useless-warning

Then double check the paths in the cabal-file so that the linked really finds the libraries needed.

After that, you should just 
```powershell
cabal run
```
and it should be all!

However, debugging in GHCi is impossible. 



# Compiling and linking LSODA as a shared lib

Making a SO file. Dont know if that is usable though. I managed to link an SO
but loading it with GHCi fails. With a very uninformative error message.

```powershell
rm bin -recurse -force
mkdir bin
gfortran -g -c odepack_numpy/lsoda.f -o bin/lsoda.o
gfortran -g -c odepack_numpy/vode.f -o bin/vode.o
gfortran -g -c odepack_numpy/srcma.f -o bin/srcma.o
gfortran -g -c odepack_numpy/xerrwv.f -o bin/xerrwv.o
ar rcs bin/libodepack.a bin/*.o
ld -shared ./bin/libodepack.a -o bin/libodepack.so -lgfortran -LC:/Users/Ludvig/Miniconda3/pkgs/m2w64-gcc-fortran-5.3.0-6/Library/mingw-w64/lib/gcc/x86_64-w64-mingw32/5.3.0 -lblas -LC:/Users/Ludvig/Miniconda3/Library/lib -llapack # notice adding paths to libgfortran, liblapack and libblas
rm bin/*.o
rm bin/*.a
```

# Compiling and linking LSODA as a static archive file

as above, but not linking anything. Let GHC do the linkning later
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

nm output tutorial
https://www.thegeekstuff.com/2012/03/linux-nm-command/