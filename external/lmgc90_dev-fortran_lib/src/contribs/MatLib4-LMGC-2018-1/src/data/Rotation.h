/*
 *  $Id: Rotation.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_ROTATION_H
#define ZORGLIB_DATA_ROTATION_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include <data/ShortArray.h>
#include <data/ShortSqrMatrix.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing a rotation operation.
 */
class Rotation {

 protected:

  // constructor
  Rotation() {}
  
 public:
  
  // destructor
  virtual ~Rotation() {}
  
  // export to matrix
  virtual void toMatrix(ShortSqrMatrix&) const = 0;

  // export to tensor
  virtual void toTensor(ShortArray&) const = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
