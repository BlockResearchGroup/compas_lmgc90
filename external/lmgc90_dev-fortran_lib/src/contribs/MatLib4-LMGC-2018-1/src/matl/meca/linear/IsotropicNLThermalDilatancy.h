/*
 *  $Id: IsotropicNLThermalDilatancy.h 204 2016-06-29 13:42:20Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_NL_THERMAL_DILATANCY_H
#define ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_NL_THERMAL_DILATANCY_H

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
class IsotropicNLThermalDilatancy : virtual public Elasticity<ALG>::Dilatancy {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  IsotropicNLThermalDilatancy() {}
  
  // copy constructor
  IsotropicNLThermalDilatancy(const IsotropicNLThermalDilatancy&) {}
  
  // destructor
  virtual ~IsotropicNLThermalDilatancy() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic non-linear thermal dilatancy***" << std::endl;

    double alpha0,alpha1,alpha2,alpha3,K,T0;
    // get dilatation coefficients
    try {
      try {
        alpha0 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_0");
      }
      catch (NoSuchPropertyException) {
        alpha0 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT");
        material.setProperty("THERMAL_DILATATION_COEFFICIENT_0",alpha0);
      }
      if (alpha0 < 0.e0) {
        if (os) (*os) << "ERROR: thermal dilatation coefficient at initial temperature must be positive." << std::endl;
        throw InvalidPropertyException("thermal dilatation coefficient at reference temperature");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: thermal dilatation coefficient is not defined." << std::endl;
      throw e;
    }
    try {
      alpha1 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_1");
    }
    catch (NoSuchPropertyException) {
      alpha1 = 0.0e0;
      material.setProperty("THERMAL_DILATATION_COEFFICIENT_1",alpha1);
    }
    try {
      alpha2 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_2");
    }
    catch (NoSuchPropertyException) {
      alpha2 = 0.0e0;
      material.setProperty("THERMAL_DILATATION_COEFFICIENT_2",alpha2);
    }
    try {
      alpha3 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_3");
    }
    catch (NoSuchPropertyException) {
      alpha3 = 0.0e0;
      material.setProperty("THERMAL_DILATATION_COEFFICIENT_3",alpha3);
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
      (*os) << "\tthermal dilatation coefficient 0 = " << alpha0 << std::endl;
      (*os) << "\tthermal dilatation coefficient 1 = " << alpha1 << std::endl;
      (*os) << "\tthermal dilatation coefficient 2 = " << alpha2 << std::endl;
      (*os) << "\tthermal dilatation coefficient 3 = " << alpha3 << std::endl;
      (*os) << "\tbulk modulus                     = " << K      << std::endl;
      (*os) << "\tinitial temperature              = " << T0     << std::endl;
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
    double alpha0 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_0");
    double alpha1 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_1");
    double alpha2 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_2");
    double alpha3 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_3");
    double K = material.getDoubleProperty("BULK_MODULUS");
    double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
    double theta = T-T0;

    // compute coupling energy
    double tr = trace(eps);
    double coef = -3*K*(alpha0+(alpha1+(alpha2+alpha3*theta)*theta)*theta)*theta;
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
class IsotropicNLThermoDilatantElasticity3D : public Elasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicNLThermoDilatantElasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>(),
                                new IsotropicNLThermalDilatancy<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicNLThermoDilatantElasticity3D(const IsotropicNLThermoDilatantElasticity3D& src)
  : Elasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicNLThermoDilatantElasticity3D() {}
};
class IsotropicNLThermoDilatantElasticity2D : public Elasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicNLThermoDilatantElasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>(),
                                new IsotropicNLThermalDilatancy<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicNLThermoDilatantElasticity2D(const IsotropicNLThermoDilatantElasticity2D& src)
  : Elasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicNLThermoDilatantElasticity2D() {}
};
class IsotropicNLThermoDilatantElasticity1D : public Elasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicNLThermoDilatantElasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>(),
                                new IsotropicNLThermalDilatancy<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicNLThermoDilatantElasticity1D(const IsotropicNLThermoDilatantElasticity1D& src)
  : Elasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicNLThermoDilatantElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicNLThermoDilatantElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicNLThermoDilatantElasticityBuilder();
  
  // the instance
  static IsotropicNLThermoDilatantElasticityBuilder const* BUILDER;
  
public:
    
  // destructor
  virtual ~IsotropicNLThermoDilatantElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
