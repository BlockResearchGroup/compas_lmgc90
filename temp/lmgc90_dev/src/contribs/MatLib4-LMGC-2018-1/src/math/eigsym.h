/*
 *  $Id: eigsym.h 143 2014-04-18 07:12:34Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_EIGSYM_H
#define ZORGLIB_MATH_EIGSYM_H

// config
#include <matlib_macros.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/*
 * Prototypes.
 */
int eigsym3(const double[],double[],double[][3]);
int eigsym2(const double[],double[],double[][2]);
int jacobi(int,int,double*,double*,double*,double*,double*);

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

