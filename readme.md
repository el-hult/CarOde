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

Then double check the paths in the cabal-file so that the linked really finds the libraries needed.

After that, you should just 
```powershell
cabal run
```
and it should be all!