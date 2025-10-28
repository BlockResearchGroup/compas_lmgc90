/*
 *  $Id: IsotropicConductionPotential.h 162 2015-03-24 09:00:40Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_THERMO_LINEAR_ISOTROPIC_CLASSICAL_CONDUCTION_POTENTIAL_H
#define ZORGLIB_MATL_THERMO_LINEAR_ISOTROPIC_CLASSICAL_CONDUCTION_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/thermo/linear/LinearConduction.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for classical isotropic conduction potentials.
 */
template <class ALG>
class IsotropicConductionPotential
: virtual public LinearConduction<ALG>::ConductionPotential {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor SYM_TENSOR;
  typedef typename ALG::Vector    VECTOR;

  // default constructor
  IsotropicConductionPotential() {}
  
  // copy constructor
  IsotropicConductionPotential(const IsotropicConductionPotential&) {}

  // destructor
  virtual ~IsotropicConductionPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic thermal conductivity***" << std::endl;

    // get conductivity coefficient
    double k;
    try {
      k = material.getDoubleProperty("THERMAL_CONDUCTIVITY_COEFFICIENT");
      if (k < 0.e0) {
        if (os) (*os) << "ERROR: thermal conductivity coefficient must be positive." << std::endl;
        throw InvalidPropertyException("thermal conductivity coefficient");
      }
    }
    catch (NoSuchPropertyException) {
      try {
        k = material.getDoubleProperty("CONDUCTIVITY_COEFFICIENT");
        if (k < 0.e0) {
          if (os) (*os) << "ERROR: conductivity coefficient must be positive." << std::endl;
          throw InvalidPropertyException("conductivity coefficient");
        }
        material.setProperty("THERMAL_CONDUCTIVITY_COEFFICIENT",k);
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: thermal conductivity coefficient is not defined." << std::endl;
        throw e;
      }
    }
    if (os) (*os) << "\n\tthermal conductivity coefficient = " << k << std::endl;
  }
  
  // compute 
  double diffusionEnergy(const MaterialProperties& material,
                         const ParameterSet& extPar,
                         const VECTOR& grad,VECTOR& flux,SYM_TENSOR& K,
                         bool computeFirst,bool computeSecond) {

    // get conductivity coefficient
    double k = material.getDoubleProperty("THERMAL_CONDUCTIVITY_COEFFICIENT");
    
    // compute diffusion energy
    double X;
    if (computeFirst) {
      flux = k*grad;
      X = 0.5*(grad*flux);
    }
    else
      X = 0.5*k*(grad*grad);
    
    if (computeSecond) K = k*SYM_TENSOR::identity();
    
    return X;
  }
};


/**
 * Implementations of the model.
 */
class IsotropicLinearConduction3D : public LinearConduction<StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicLinearConduction3D()
  : LinearConduction<StdTensorAlgebra3D>(
        new IsotropicConductionPotential<StdTensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicLinearConduction3D(const IsotropicLinearConduction3D& src) 
  : LinearConduction<StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicLinearConduction3D() {}
};
class IsotropicLinearConduction2D : public LinearConduction<StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicLinearConduction2D()
  : LinearConduction<StdTensorAlgebra2D>(
        new IsotropicConductionPotential<StdTensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicLinearConduction2D(const IsotropicLinearConduction2D& src) 
  : LinearConduction<StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicLinearConduction2D() {}
};
class IsotropicLinearConduction1D : public LinearConduction<StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicLinearConduction1D()
  : LinearConduction<StdTensorAlgebra1D>(
        new IsotropicConductionPotential<StdTensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicLinearConduction1D(const IsotropicLinearConduction1D& src) 
  : LinearConduction<StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicLinearConduction1D() {}
};

/**
 * The associated model builder
 */
class IsotropicLinearConductionBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicLinearConductionBuilder();
  
  // the instance
  static IsotropicLinearConductionBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicLinearConductionBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
