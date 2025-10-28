/*
 *  $Id: lapack.h 143 2014-04-18 07:12:34Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_LAPACK_H
#define ZORGLIB_MATH_LAPACK_H

// config
#include <matlib_macros.h>

#ifdef MATLIB_USE_VECLIB

#include <Accelerate/Accelerate.h>
#define LAPACK_INTEGER __CLPK_integer
#define LAPACK_DOUBLE __CLPK_doublereal

#else

#define LAPACK_INTEGER int
#define LAPACK_DOUBLE double

extern "C" {
  void FORTRAN(dgeev)(char*,char*,int*,double*,int*,double*,double*,double*,
                      int*,double*,int*,double*,int*,int*);
  void FORTRAN(dgesv)(int*,int*,double*,int*,int*,double*,int*,int*);
  void FORTRAN(dgesvd)(char*,char*,int*,int*,double*,int*,double*,double*,int*,
                       double*,int*,double*,int*,int*);
  void FORTRAN(dgetrf)(int*,int*,double*,int*,int*,int*);
  void FORTRAN(dgetri)(int*,double*,int*,int*,double*,int*,int*);
  void FORTRAN(dspev)(char*,char*,int*,double*,double*,double*,int*,double*,int*);
  void FORTRAN(dsygv)(int*,char*,char*,int*,double*,int*,double*,int*,double*,
                      double*,int*,int*);
  void FORTRAN(dsysv)(char*,int*,int*,double*,int*,int*,double*,int*,
                      double*,int*,int*);
}

#endif

#endif
