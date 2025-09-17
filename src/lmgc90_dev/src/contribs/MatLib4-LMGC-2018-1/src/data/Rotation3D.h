/*
 *  $Id: Rotation3D.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_ROTATION_3D_H
#define ZORGLIB_DATA_ROTATION_3D_H

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
 * Class for 3D rotations.
 */
class Rotation3D : virtual public Rotation {

 public:

  // types of parameterization
  enum Type {BUNGE,KOCKS};

 private:
  
  // CRV representation (cf. Cardona&Geradin)
  double c0;
  ShortArray c;
  
 public:

  // constructor (from Euler angles)
  Rotation3D(double,double,double,Type = BUNGE);

  // constructor (from Euler parameters)
  Rotation3D(const ShortArray&);
  
  // constructor (from rotation matrix)
  Rotation3D(const ShortSqrMatrix&);

  // copy constructor
  Rotation3D(const Rotation3D&);
  
  // destructor
  virtual ~Rotation3D() {}
  
  // export to Euler angles
  void toEulerAngles(double&,double&,double&,Type = BUNGE) const;

  // export to matrix
  void toMatrix(ShortSqrMatrix&) const;
  
  // export to tensor
  void toTensor(ShortArray&) const;
  
  // from Euler parameters to CRV representation
  static void eul2crv(const ShortArray&,ShortArray&);

  // from matrix representation to Euler parameters
  static void euler(const ShortSqrMatrix&,ShortArray&);
  
  // from vector to matrix representation
  static void spin(const ShortArray&,ShortSqrMatrix&);
};
  
#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif
  
#endif
