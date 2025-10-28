/*
 *  $Id: IsotropicLinThermalDilatancy.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_THERMAL_DILATANCY_H
#define ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_THERMAL_DILATANCY_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/linear/IsotropicElasticPotential.h>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing isotropic thermal dilatancy models.
 */
template <class ALG>
class IsotropicLinThermalDilatancy : virtual public Elasticity<ALG>::Dilatancy {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  IsotropicLinThermalDilatancy() {}
  
  // copy constructor
  IsotropicLinThermalDilatancy(const IsotropicLinThermalDilatancy&) {}
  
  // destructor
  virtual ~IsotropicLinThermalDilatancy() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic thermal dilatancy***" << std::endl;

    double alpha,K,T0;
    // get dilatation coefficient
    try {
      alpha = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT");
      if (alpha < 0.e0) {
        if (os) (*os) << "ERROR: thermal dilatation coefficient must be positive." << std::endl;
        throw InvalidPropertyException("thermal dilatation coefficient");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: thermal dilatation coefficient is not defined." << std::endl;
      throw e;
    }
    // get bulk modulus
    try {
      K = material.getDoubleProperty("BULK_MODULUS");
      if (K < 0.0e0) {
        if (os) (*os) << "ERROR: bulk modulus must be positive." << std::endl;
        throw InvalidPropertyException("bulk modulus");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: bulk modulus is not defined." << std::endl;
      throw e;
    }
    // get initial temperature
    try {
      T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: initial temperature is not defined." << std::endl;
      throw e;
    }
    
    if (os) {
      (*os) << "\tthermal dilatation coefficient = " << alpha << std::endl;
      (*os) << "\tbulk modulus                   = " << K     << std::endl;
      (*os) << "\tinitial temperature            = " << T0    << std::endl;
    }
  }
  
  // compute coupling energy
  double couplingEnergy(const MaterialProperties& material,
                        const ParameterSet& extPar,
                        const SYM_TENSOR& eps,SYM_TENSOR& sig,
                        SYM_TENSOR4& M,bool first,bool second) {
    
    // check for temperature
    if (!extPar.count("TEMPERATURE")) {
      sig = 0.0e0;
      M = 0.0e0;
      return 0.e0;
    }
    double T = extPar.find("TEMPERATURE")->second;
    
    // get material parameters
    double alpha = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT");
    double K = material.getDoubleProperty("BULK_MODULUS");
    double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");

    // compute coupling energy
    double tr = trace(eps);
    double coef = -3*K*alpha*(T-T0);
    double W = coef*tr;
    if (first) {
      static const SYM_TENSOR delta = SYM_TENSOR::identity();
      sig = coef*delta;
    }
    if (second) M = 0.0e0;
    
    return W;
  }
};


/**
 * Implementations of the model.
 */
class IsotropicThermoDilatantElasticity3D : public Elasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicThermoDilatantElasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicThermoDilatantElasticity3D(const IsotropicThermoDilatantElasticity3D& src) 
  : Elasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDilatantElasticity3D() {}
};
class IsotropicThermoDilatantElasticity2D : public Elasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicThermoDilatantElasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicThermoDilatantElasticity2D(const IsotropicThermoDilatantElasticity2D& src) 
  : Elasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDilatantElasticity2D() {}
};
class IsotropicThermoDilatantElasticity1D : public Elasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicThermoDilatantElasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicThermoDilatantElasticity1D(const IsotropicThermoDilatantElasticity1D& src) 
  : Elasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDilatantElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicThermoDilatantElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicThermoDilatantElasticityBuilder();
  
  // the instance
  static IsotropicThermoDilatantElasticityBuilder const* BUILDER;
  
public:
    
  // destructor
  virtual ~IsotropicThermoDilatantElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
