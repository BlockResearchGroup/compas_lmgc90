/*
 *  $Id: eigmat.h 143 2014-04-18 07:12:34Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_EIGMAT_H
#define ZORGLIB_MATH_EIGMAT_H

// config
#include <matlib_macros.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Prototypes.
 */
int eigenLR3(const double[],double[],double[][3],double[][3]);
int eigenLR2(const double[],double[],double[][2],double[][2]);
int eigen3(const double[],double[],double[][3]);
int eigen2(const double[],double[],double[][2]);
int eigen(int,int,double*,double*,double*,double*,int*,double*);

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

