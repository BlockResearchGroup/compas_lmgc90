/*
 *  $Id: Tensor4_1D.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_TENSOR4_1D_H
#define ZORGLIB_MATH_TENSOR4_1D_H

// config
#include <matlib_macros.h>

// local
#include <math/SymTensor4_1D.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class encapsulating 1D 4th-order tensor objects.
 */
typedef SymTensor4_1D Tensor4_1D;

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

