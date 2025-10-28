/*
 *  $Id: IsotropicThermoDependentElasticity.h 213 2016-09-26 17:05:10Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_THERMO_DEPENDENT_ELASTICITY_H
#define ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_THERMO_DEPENDENT_ELASTICITY_H

// config
#include <matlib_macros.h>

// local
#include <matl/thermomeca/linear/IsotropicThermoElasticity.h>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif


/**
 * Implementations of the model.
 */
class IsotropicThermoDependentElasticity3D : public Elasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicThermoDependentElasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicThermoDependentElasticity3D(const IsotropicThermoDilatantElasticity3D& src)
  : Elasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDependentElasticity3D() {}
};
class IsotropicThermoDependentElasticity2D : public Elasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicThermoDependentElasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicThermoDependentElasticity2D(const IsotropicThermoDilatantElasticity2D& src)
  : Elasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDependentElasticity2D() {}
};
class IsotropicThermoDependentElasticity1D : public Elasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicThermoDependentElasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicThermoDependentElasticity1D(const IsotropicThermoDilatantElasticity1D& src)
  : Elasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDependentElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicThermoDependentElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicThermoDependentElasticityBuilder();
  
  // the instance
  static IsotropicThermoDependentElasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~IsotropicThermoDependentElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
