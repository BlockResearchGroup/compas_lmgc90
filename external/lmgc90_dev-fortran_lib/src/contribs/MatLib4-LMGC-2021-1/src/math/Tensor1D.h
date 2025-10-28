/*
 *  $Id: Tensor1D.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_TENSOR_1D_H
#define ZORGLIB_MATH_TENSOR_1D_H

// config
#include <matlib_macros.h>

// local
#include <math/SymTensor1D.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class encapsulating 1D tensor objects.
 */
typedef SymTensor1D Tensor1D;

// compute transposed
inline
Tensor1D transpose(const Tensor1D& A) {return A;}

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

