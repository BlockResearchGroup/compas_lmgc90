/*
 *  $Id: IsotropicLinDiffusionPotential.h 207 2016-08-19 16:52:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFFUSION_LINEAR_ISOTROPIC_DIFFUSION_POTENTIAL_H
#define ZORGLIB_MATL_DIFFUSION_LINEAR_ISOTROPIC_DIFFUSION_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/diff/linear/LinVariationalDiffusion.h>
#include <matl/diff/linear/StdLinChemicalCapacity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for (linearized) isotropic diffusion potentials.
 */
template <class ALG>
class IsotropicLinDiffusionPotential
: virtual public LinVariationalDiffusion<ALG>::DiffusionPotential {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor SYM_TENSOR;
  typedef typename ALG::Vector    VECTOR;

  // default constructor
  IsotropicLinDiffusionPotential() {}
  
  // copy constructor
  IsotropicLinDiffusionPotential(const IsotropicLinDiffusionPotential&) {}

  // destructor
  virtual ~IsotropicLinDiffusionPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic linear diffusion***" << std::endl;
     
    // get mobility coefficient
    double k;
    try {
      k = material.getDoubleProperty("MOBILITY_COEFFICIENT");
      if (k < 0.e0) {
        if (os) (*os) << "ERROR: mobility coefficient must be positive." << std::endl;
        throw InvalidPropertyException("mobility coefficient");
      }
      if (os) (*os) << "\n\tmobility coefficient = " << k << std::endl;
    }
    catch (NoSuchPropertyException) {
      // compute from diffusion coefficient
      try {
        double d = material.getDoubleProperty("DIFFUSION_COEFFICIENT");
        if (d < 0.e0) {
          if (os) (*os) << "ERROR: diffusion coefficient must be positive." << std::endl;
          throw InvalidPropertyException("diffusion coefficient");
        }
        double R;
        try {
          R = material.getDoubleProperty("UNIVERSAL_GAS_CONSTANT");
          if (R <= 0.0e0) {
            if (os) (*os) << "ERROR: gas constant must be strictly positive." << std::endl;
            throw InvalidPropertyException("gas constant");
          }
        }
        catch (NoSuchPropertyException) {
          R = 8.31446; // default value in S.I. units (J.K^-1.mol^-1)
          material.setProperty("UNIVERSAL_GAS_CONSTANT",R);
        }
        double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
        if (T0 <= 0.0e0) {
          if (os) (*os) << "ERROR: reference temperature must be strictly positive." << std::endl;
          throw InvalidPropertyException("reference temperature");
        }
        double c0 = material.getDoubleProperty("REFERENCE_CONCENTRATION");
        if (c0 <= 0.0e0) {
          if (os) (*os) << "ERROR: reference concentration must be strictly positive." << std::endl;
          throw InvalidPropertyException("reference concentration");
        }
        k = d*c0/(R*T0);
        material.setProperty("MOBILITY_COEFFICIENT",k);
        if (os) {
          (*os) << "\n\tdiffusion coefficient   = " << d;
          (*os) << "\n\tuniversal gas constant  = " << R;
          (*os) << "\n\treference temperature   = " << T0;
          (*os) << "\n\treference concentration = " << c0;
          (*os) << "\n\tmobility coefficient    = " << k << std::endl;
        }
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: mobility coefficient cannot be defined." << std::endl;
        throw e;
      }
    }
  }
  
  // compute 
  double diffusionEnergy(const MaterialProperties& material,
                         const ParameterSet& extPar,
                         const VECTOR& grad,double mu,
                         VECTOR& flux,double& c,
                         SYM_TENSOR& K,VECTOR& S,double& C,
                         bool computeFirst,bool computeSecond) {
    
    // get mobility coefficient
    double k = material.getDoubleProperty("MOBILITY_COEFFICIENT");
    
    // compute diffusion energy
    double X;
    if (computeFirst) {
      flux = k*grad;
      c = 0.0e0;
      X = 0.5*(grad*flux);
    }
    else
      X = 0.5*k*(grad*grad);
    
    if (computeSecond) {
      K = k*SYM_TENSOR::identity();
      S = 0.0e0;
      C = 0.0e0;
    }
    
    return X;
  }
};


/**
 * Implementations of the model.
 */
class IsotropicLinVariationalDiffusion3D : public LinVariationalDiffusion<StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicLinVariationalDiffusion3D()
  : LinVariationalDiffusion<StdTensorAlgebra3D>(new IsotropicLinDiffusionPotential<StdTensorAlgebra3D>(),
                                                 new StdLinChemicalCapacity()) {}
  
  // copy constructor
  IsotropicLinVariationalDiffusion3D(const IsotropicLinVariationalDiffusion3D& src) 
  : LinVariationalDiffusion<StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicLinVariationalDiffusion3D() {}
};
class IsotropicLinVariationalDiffusion2D : public LinVariationalDiffusion<StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicLinVariationalDiffusion2D()
  : LinVariationalDiffusion<StdTensorAlgebra2D>(new IsotropicLinDiffusionPotential<StdTensorAlgebra2D>(),
                                                 new StdLinChemicalCapacity()) {}
  
  // copy constructor
  IsotropicLinVariationalDiffusion2D(const IsotropicLinVariationalDiffusion2D& src) 
  : LinVariationalDiffusion<StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicLinVariationalDiffusion2D() {}
};
class IsotropicLinVariationalDiffusion1D : public LinVariationalDiffusion<StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicLinVariationalDiffusion1D()
  : LinVariationalDiffusion<StdTensorAlgebra1D>(new IsotropicLinDiffusionPotential<StdTensorAlgebra1D>(),
                                                 new StdLinChemicalCapacity()) {}
  
  // copy constructor
  IsotropicLinVariationalDiffusion1D(const IsotropicLinVariationalDiffusion1D& src) 
  : LinVariationalDiffusion<StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicLinVariationalDiffusion1D() {}
};

/**
 * The associated model builder
 */
class IsotropicLinVariationalDiffusionBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicLinVariationalDiffusionBuilder();
  
  // the instance
  static IsotropicLinVariationalDiffusionBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicLinVariationalDiffusionBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
