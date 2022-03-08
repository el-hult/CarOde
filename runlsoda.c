#include <stdlib.h>
#include <stdio.h>

// https://stackoverflow.com/questions/1985819/gfortran-dll-underscore

// compute the op norm of n x n matrix a,
// consistent with the weighted max-norm w
// remember that fortran is column-major!
// by some reason, the lib has underscores in the names. :/
// there is some name mangling going on...
double fnorm_(int *n, double *a, double *w);

// LSODA
//   subroutine lsoda (f, neq, y, t, tout, itol, rtol, , itask,
//  1            istate, iopt, rwork, lrw, iwork, liw, jac, jt)
//   external f, jac
//   integer neq, itol, itask, istate, iopt, lrw, iwork, liw, jt
//   double precision y, t, tout, rtol, , rwork,
//   dimension neq(1), y(1), rtol(1), , rwork(lrw), iwork(liw)
// double atol;
// double rwork[lrw];
// int neq; the number of ODE equations
void lsoda_(void *f, // subroutine for right-hand side vector f.
            int *neq, // number of first order ode-s.
            double *y, //array of initial values, of length neq.
            double *t, // the initial value of the independent variable.
            double *tout, // first point where output is desired (.ne. t).
            int *itol, // 1 or 2 according as atol (below) is a scalar or array.
            double *rtol, // relative tolerance parameter (scalar).
            double *atol, // absolute tolerance parameter (scalar or array).
            int *itask, // 1 for normal computation of output values of y at t = tout.
            int *istate, // integer flag (input and output).  set istate = 1.
            int *iopt, // 0 to indicate no optional inputs used.
            double *rwork, // real work array of length at least. 22 + neq * max(16, neq + 9).
            int *lrw, // declared length of rwork (in user-s dimension).
            int *iwork, // integer work array of length at least  20 + neq.
            int *liw, // declared length of iwork (in user-s dimension).
            void *jac, // name of subroutine for jacobian matrix. use a dummy name.  see also paragraph e below.
            int *jt // jacobian type indicator.  set jt = 2.
            );

void fex(int* neq, double* t, double* y,double* ydot) {
    ydot[0] = -0.4E0*y[0] + 1.0E4*y[1]*y[2];
    ydot[2] = 3.0E7*y[1]*y[1];
    ydot[1] = -ydot[0] - ydot[2];
};

int main(void)
{
    
    printf("fnorm example...\n");
    int n = 2;
    double w[2] = {1.0, 1.0};
    double a[2 * 2] = {1.0, 2.0, 3.0, 4.0};
    printf("fNorm = %f\n", fnorm_(&n, a, w));


    printf("Trying LSODA...\n");
    double rwork[70];
    int iwork[23];
    int neq = 3;
    double y[] = {1.0E0,0.0E0,0.0E0};
    double t = 0.0E0;
    double tout = 0.4E0;
    int itol = 2;
    double rtol = 1.0E-4;
    double atol[] = {1.0E-6,1.0E-10,1.0E-6};
    int itask = 1;
    int istate = 1;
    int iopt = 0;
    int lrw = 70;
    int liw = 23;
    int jt = 2;
    void* jacdummy = 0;
    for (int iout=1;iout<=12;++iout){
        lsoda_( &fex,&neq,(double*)&y,&t,&tout,&itol,&rtol,(double*)&atol,&itask,
                &istate,&iopt,(double*)&rwork,&lrw, (int*) &iwork, &liw,jacdummy, &jt );
        printf("at t = %12.4e  y = %14.6e  %14.6e  %14.6e\n",t,y[0],y[1],y[2]);
        if (istate < 0) {
            printf(" error halt.. istate =%d",istate);
            exit(1);
        } else{
            tout *= 10;
        }
    }
    printf("\n");
    printf(
        "no. steps =%4d  no. f-s =%4d  no. j-s =%3d\nmethod last used =%2d last switch was at t =%e\n",
        iwork[10],iwork[11],iwork[12],iwork[18],rwork[14]);

    return 0;
}
