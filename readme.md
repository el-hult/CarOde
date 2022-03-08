The content of the folder `scipylsoda` is simply 

I assume you have conda.
`conda install -c conda-forge mingw lapack blas` 
to get gfortran and ld, as well as the lapack and blas libraries that the fortran code relies on.

I learned that these libraries was needed by attempting compilation, finding undefined symbols, and simply googling them.
it seems the source code is fortran 77 compatible with GFortran. 
the DECK statements can be handeled by GFortran, but not LFortran, so we need a specific compiler.

# Example with C
```powershell
mkdir bin
gci .\odepack_numpy\*.f -Exclude xset* | %{ gfortran -c $_.FullName -o "bin/$($_.BaseName).o" -g}  # compile the files with all debug flags
ar rcs bin\libodepack.a .\bin\*.o  # make a static library/archive. this is not really needed, but feels neat.
                                #  N.B. LAPACK and BLAS are still inlinked undefined symbols in this archive
rm  .\bin\*.o
nm .\bin\libodepack.a | sls lsoda  # things are here. but with appended underscore :/
nm .\bin\libodepack.a | sls daxpy  # this is a BLAS reference. undefined.
nm .\bin\libodepack.a | sls dgbtrf # this is a LAPACK reference. undefined.
nm .\bin\libodepack.a | sls _gfortran_transfer_real_write # libgfortran function. undefined
gcc .\runlsoda.c -g -o runlsoda.exe --std c99 -Wall -l:bin/libodepack.a -lblas -llapack -lgfortran -LC:\Users\Ludvig\Miniconda3\Library\lib # compile a C runner program, link against the archive file, and against libgfortran, BLAS and LAPACK enable debug info. :)
# the explicit addition to LD_PATH is to find BLAS and LAPACK. libgfortran is found automatically
./runlsoda.exe # run it!
rm runlsoda.exe
```

# Example with Haskell
As a next step, compile the Haskell code!
```powershell
mkdir bin
gci .\odepack_numpy\*.f -Exclude xset* | %{ gfortran -c $_.FullName -o "bin/$($_.BaseName).o" -g}  # compile the files with all debug flags
ar rcs bin\libodepack.a .\bin\*.o  # make a static library/archive. this is not really needed, but feels neat.
rm  .\bin\*.o
cabal run
```
there. it should be all!
It seems that my GCH installation links to libgfortran and BLAS and Lapack! nice!