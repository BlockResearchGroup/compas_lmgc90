/*
 *  $Id: Rotation2D.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_ROTATION_2D_H
#define ZORGLIB_DATA_ROTATION_2D_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include <data/Rotation.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for 2D rotations.
 */
class Rotation2D : virtual public Rotation {
  
 private:
  
  // rotation angle
  double theta;
  
 public:
  
  // constructor (from angle)
  Rotation2D(double = 0.e0);
  
  // constructor (from rotation matrix)
  Rotation2D(const ShortSqrMatrix&);
  
  // copy constructor
  Rotation2D(const Rotation2D&);
  
  // destructor
  virtual ~Rotation2D() {}
  
  // export to matrix
  void toMatrix(ShortSqrMatrix&) const;
  
  // export to tensor
  void toTensor(ShortArray&) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
