/*
 *  $Id: IsotropicHypoElasticity.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_ISOTROPIC_HYPO_ELASTICITY_H
#define ZORGLIB_MATL_MECA_ISOTROPIC_HYPO_ELASTICITY_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/hypo/HypoElasticity.h>
#include <matl/meca/linear/IsotropicElasticPotential.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif


/**
 * Implementations of the isotropic hypoelasticity model.
 */
class IsotropicHypoElasticity3D : public HypoElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicHypoElasticity3D()
  : HypoElasticity<TensorAlgebra3D>(
          *(new IsotropicElasticPotential<TensorAlgebra3D>())) {}
  
  // copy constructor
  IsotropicHypoElasticity3D(const IsotropicHypoElasticity3D& src) 
  : HypoElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicHypoElasticity3D() {}
};
class IsotropicHypoElasticity2D : public HypoElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicHypoElasticity2D()
  : HypoElasticity<TensorAlgebra2D>(
          *(new IsotropicElasticPotential<TensorAlgebra2D>())) {}
  
  // copy constructor
  IsotropicHypoElasticity2D(const IsotropicHypoElasticity2D& src) 
  : HypoElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicHypoElasticity2D() {}
};
class IsotropicHypoElasticity1D : public HypoElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicHypoElasticity1D()
  : HypoElasticity<TensorAlgebra1D>(
          *(new IsotropicElasticPotential<TensorAlgebra1D>())) {}
  
  // copy constructor
  IsotropicHypoElasticity1D(const IsotropicHypoElasticity1D& src) 
  : HypoElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicHypoElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicHypoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicHypoElasticityBuilder();
  
  // the instance
  static IsotropicHypoElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicHypoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
