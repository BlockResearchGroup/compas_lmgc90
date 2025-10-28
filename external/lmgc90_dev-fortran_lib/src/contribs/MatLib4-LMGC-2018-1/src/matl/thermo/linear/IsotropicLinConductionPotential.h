/*
 *  $Id: IsotropicLinConductionPotential.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_THERMO_LINEAR_ISOTROPIC_CONDUCTION_POTENTIAL_H
#define ZORGLIB_MATL_THERMO_LINEAR_ISOTROPIC_CONDUCTION_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/thermo/linear/LinVariationalConduction.h>
#include <matl/thermo/linear/StdLinThermalCapacity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for (linearized) isotropic conduction potentials.
 */
template <class ALG>
class IsotropicLinConductionPotential
: virtual public LinVariationalConduction<ALG>::ConductionPotential {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor SYM_TENSOR;
  typedef typename ALG::Vector    VECTOR;

  // default constructor
  IsotropicLinConductionPotential() {}
  
  // copy constructor
  IsotropicLinConductionPotential(const IsotropicLinConductionPotential&) {}

  // destructor
  virtual ~IsotropicLinConductionPotential() {}
  
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
    
    // reference temperature
    try {
      double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
      if (TRef <= 0.e0) {
        if (os) (*os) << "ERROR: reference temperature must be strictly positive." << std::endl;
        throw InvalidPropertyException("reference temperature");
      }
      if (os) (*os) << "\n\treference temperature = " << TRef << std::endl;
    }
    catch (NoSuchPropertyException) {
      // use initial temperature
      try {
        double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
        if (T0 <= 0.e0) {
          if (os) (*os) << "ERROR: initial temperature must be strictly positive." << std::endl;
          throw InvalidPropertyException("initial temperature");
        }
        material.setProperty("REFERENCE_TEMPERATURE",T0);
        if (os) (*os) << "\n\treference temperature = " << T0 << std::endl;
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: reference temperature cannot be set." << std::endl;
        throw e;
      }
    }
  }
  
  // compute 
  double diffusionEnergy(const MaterialProperties& material,
                         const ParameterSet& extPar,
                         const VECTOR& grad,double Th,
                         VECTOR& flux,double& N,
                         SYM_TENSOR& K,VECTOR& S,double& C,
                         bool computeFirst,bool computeSecond) {

    // get conductivity coefficient and reference temperature
    double k = material.getDoubleProperty("THERMAL_CONDUCTIVITY_COEFFICIENT");
    double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    
    // compute diffusion energy
    double X;
    double coef = k*TRef;
    if (computeFirst) {
      flux = coef*grad;
      N = 0.0e0;
      X = 0.5*(grad*flux);
    }
    else
      X = 0.5*coef*(grad*grad);
    
    if (computeSecond) {
      K = coef*SYM_TENSOR::identity();
      S = 0.0e0;
      C = 0.0e0;
    }
    
    return X;
  }
};


/**
 * Implementations of the model.
 */
class IsotropicLinVariationalConduction3D : public LinVariationalConduction<StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicLinVariationalConduction3D()
  : LinVariationalConduction<StdTensorAlgebra3D>(new IsotropicLinConductionPotential<StdTensorAlgebra3D>(),
                                                 new StdLinThermalCapacity()) {}
  
  // copy constructor
  IsotropicLinVariationalConduction3D(const IsotropicLinVariationalConduction3D& src) 
  : LinVariationalConduction<StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicLinVariationalConduction3D() {}
};
class IsotropicLinVariationalConduction2D : public LinVariationalConduction<StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicLinVariationalConduction2D()
  : LinVariationalConduction<StdTensorAlgebra2D>(new IsotropicLinConductionPotential<StdTensorAlgebra2D>(),
                                                 new StdLinThermalCapacity()) {}
  
  // copy constructor
  IsotropicLinVariationalConduction2D(const IsotropicLinVariationalConduction2D& src) 
  : LinVariationalConduction<StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicLinVariationalConduction2D() {}
};
class IsotropicLinVariationalConduction1D : public LinVariationalConduction<StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicLinVariationalConduction1D()
  : LinVariationalConduction<StdTensorAlgebra1D>(new IsotropicLinConductionPotential<StdTensorAlgebra1D>(),
                                                 new StdLinThermalCapacity()) {}
  
  // copy constructor
  IsotropicLinVariationalConduction1D(const IsotropicLinVariationalConduction1D& src) 
  : LinVariationalConduction<StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicLinVariationalConduction1D() {}
};

/**
 * The associated model builder
 */
class IsotropicLinVariationalConductionBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicLinVariationalConductionBuilder();
  
  // the instance
  static IsotropicLinVariationalConductionBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicLinVariationalConductionBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
