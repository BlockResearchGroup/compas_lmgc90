/*
 *  $Id: NeohookeanEOS.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_NEOHOOKEAN_WITH_EOS_H
#define ZORGLIB_MATL_MECA_HYPER_NEOHOOKEAN_WITH_EOS_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/eos/StandardEOS.h>
#include <matl/meca/hyper/NeohookeanPotential.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Implementations of the model.
 */
class NeohookeanEOS3D : public HyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NeohookeanEOS3D()
  : HyperElasticity<TensorAlgebra3D>(new NeohookeanPotential<TensorAlgebra3D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  NeohookeanEOS3D(const NeohookeanEOS3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NeohookeanEOS3D() {}
};
class NeohookeanEOS2D : public HyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  NeohookeanEOS2D()
  : HyperElasticity<TensorAlgebra2D>(new NeohookeanPotential<TensorAlgebra2D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  NeohookeanEOS2D(const NeohookeanEOS2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~NeohookeanEOS2D() {}
};
class NeohookeanEOS1D : public HyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  NeohookeanEOS1D()
  : HyperElasticity<TensorAlgebra1D>(new NeohookeanPotential<TensorAlgebra1D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  NeohookeanEOS1D(const NeohookeanEOS1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~NeohookeanEOS1D() {}
};

/**
 * The associated model builder
 */
class NeohookeanEOSBuilder : public ModelBuilder {

 private:
  
  // constructor
  NeohookeanEOSBuilder();

  // the instance
  static NeohookeanEOSBuilder const* BUILDER;

 public:
  
  // destructor
  virtual ~NeohookeanEOSBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
