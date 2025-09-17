/*
 *  $Id: IsotropicStdThermalDilatancy.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_ISOTROPIC_THERMAL_DILATANCY_H
#define ZORGLIB_MATL_MECA_HYPER_ISOTROPIC_THERMAL_DILATANCY_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/hyper/HyperElasticity.h>
#include <matl/meca/hyper/GeneralHenckyPotential.h>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing (geometrically nonlinear) isotropic thermal dilatancy models.
 */
template <class ALG>
class IsotropicStdThermalDilatancy : virtual public HyperElasticity<ALG>::Dilatancy {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  IsotropicStdThermalDilatancy() {}
  
  // copy constructor
  IsotropicStdThermalDilatancy(const IsotropicStdThermalDilatancy&) {}
  
  // destructor
  virtual ~IsotropicStdThermalDilatancy() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic thermal dilatancy (finite strains)***" << std::endl;

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
                        const SYM_TENSOR& C,SYM_TENSOR& S,
                        SYM_TENSOR4& M,bool first,bool second) {
    
    // check for temperature
    if (!extPar.count("TEMPERATURE")) {
      S = 0.0e0;
      M = 0.0e0;
      return 0.e0;
    }
    double T = extPar.find("TEMPERATURE")->second;
    
    // get material parameters
    double alpha = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT");
    double K = material.getDoubleProperty("BULK_MODULUS");
    double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
    
    // compute determinant and inverse
    double detC;
    SYM_TENSOR Cinv;
    if (first || second) 
      Cinv = C.inverse(detC);
    else
      detC = determinant(C);
    
    // compute coupling energy
    double logJ = 0.5*std::log(detC);
    double coef = -3*K*alpha*(T-T0);
    double W = coef*logJ;
    if (first) S = coef*Cinv;
    if (second) {
      M = 0.0e0;
      M.addIJKL(-coef,Cinv);
    }
    
    return W;
  }
};


/**
 * Implementations of the model.
 */
class IsotropicThermoDilatantHyperElasticity3D : public HyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicThermoDilatantHyperElasticity3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
          new GeneralHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicElasticPotential<TensorAlgebra3D>())),
          eos,new IsotropicStdThermalDilatancy<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicThermoDilatantHyperElasticity3D(const IsotropicThermoDilatantHyperElasticity3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDilatantHyperElasticity3D() {}
};
class IsotropicThermoDilatantHyperElasticity2D : public HyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicThermoDilatantHyperElasticity2D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra2D>(
          new GeneralHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicElasticPotential<TensorAlgebra2D>())),
          eos,new IsotropicStdThermalDilatancy<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicThermoDilatantHyperElasticity2D(const IsotropicThermoDilatantHyperElasticity2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDilatantHyperElasticity2D() {}
};
class IsotropicThermoDilatantHyperElasticity1D : public HyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicThermoDilatantHyperElasticity1D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra1D>(
          new GeneralHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicElasticPotential<TensorAlgebra1D>())),
          eos,new IsotropicStdThermalDilatancy<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicThermoDilatantHyperElasticity1D(const IsotropicThermoDilatantHyperElasticity1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDilatantHyperElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicThermoDilatantHyperElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicThermoDilatantHyperElasticityBuilder();
  
  // the instance
  static IsotropicThermoDilatantHyperElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicThermoDilatantHyperElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
