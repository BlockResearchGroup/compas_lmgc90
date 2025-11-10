/*
 *  $Id: BravaisLattice.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_CRYSTAL_BRAVAIS_LATTICE_H
#define ZORGLIB_MATL_MECA_CRYSTAL_BRAVAIS_LATTICE_H

// config
#include <matlib_macros.h>

// STL
#include <vector>
// local
#include <math/Vector3D.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Simple class for atomic positions
 */
class Atom {
 public:
  Vector3D x; // atom coordinates
  double   r; // radial distance to origin
};


/**
 * Generic class descibing Bravais lattices.
 */
class BravaisLattice {

 protected:
  
  // lattice vectors
  Vector3D v[3];
  
  // Bravais cell volume
  double vol;
  
  // compute volume
  double computeVolume();

  // empty constructor
  BravaisLattice() {}

 public:

  // constructor
  BravaisLattice(double[],double[],double[]);

  // copy constructor
  BravaisLattice(const BravaisLattice&);

  // destructor
  virtual ~BravaisLattice() {}

  // get lattice vectors
  Vector3D vector(unsigned int i) const {return v[i];}
  
  // get volume
  double volume() const {return vol;}
  
  // generate a crystallite (crystal sample) of radius R centered at origin
  void genCrystal(double,std::vector<Atom>&) const;
};


/**
 * BCC Bravais lattice.
 */
class BCCBravaisLattice : public BravaisLattice {
  
 public:
  
  // constructor
  BCCBravaisLattice(double);
  
  // copy constructor
  BCCBravaisLattice(const BCCBravaisLattice& src) : BravaisLattice(src) {}
  
  // destructor
  virtual ~BCCBravaisLattice() {}
};


/**
 * FCC Bravais lattice.
 */
class FCCBravaisLattice : public BravaisLattice {
  
 public:

  // constructor
  FCCBravaisLattice(double);
  
  // copy constructor
  FCCBravaisLattice(const FCCBravaisLattice& src) : BravaisLattice(src) {}
  
  // destructor
  virtual ~FCCBravaisLattice() {}
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
