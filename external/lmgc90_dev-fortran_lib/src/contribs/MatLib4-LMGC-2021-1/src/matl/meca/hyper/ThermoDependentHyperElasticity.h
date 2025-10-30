/*
 *  $Id$
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2021, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_DEPENDENT_HYPER_ELASTICITY_H
#define ZORGLIB_MATL_MECA_THERMO_DEPENDENT_HYPER_ELASTICITY_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/hyper/GeneralHenckyPotential.h>
#include <matl/thermomeca/linear/IsotropicThermoElasticity.h>
#include <matl/thermomeca/hyper/IsotropicThermoHEDilatancy.h>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif


/**
 * Implementations of the model.
 */
class IsotropicThermoDependentHyperElasticity3D : public HyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicThermoDependentHyperElasticity3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
          new GeneralHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra3D>())),
          eos,new IsotropicThermoHyperElasticDilatancy<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicThermoDependentHyperElasticity3D(const IsotropicThermoDependentHyperElasticity3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDependentHyperElasticity3D() {}
};
class IsotropicThermoDependentHyperElasticity2D : public HyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicThermoDependentHyperElasticity2D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra2D>(
          new GeneralHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra2D>())),
          eos,new IsotropicThermoHyperElasticDilatancy<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicThermoDependentHyperElasticity2D(const IsotropicThermoDependentHyperElasticity2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDependentHyperElasticity2D() {}
};
class IsotropicThermoDependentHyperElasticity1D : public HyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicThermoDependentHyperElasticity1D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra1D>(
          new GeneralHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra1D>())),
          eos,new IsotropicThermoHyperElasticDilatancy<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicThermoDependentHyperElasticity1D(const IsotropicThermoDilatantHyperElasticity1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDependentHyperElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicThermoDependentHyperElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicThermoDependentHyperElasticityBuilder();
  
  // the instance
  static IsotropicThermoDependentHyperElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicThermoDependentHyperElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
