/*
 *  $Id: IsotropicFickDiffusionPotential.h 207 2016-08-19 16:52:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFFUSION_NONLINEAR_ISOTROPIC_FICK_DIFFUSION_POTENTIAL_H
#define ZORGLIB_MATL_DIFFUSION_NONLINEAR_ISOTROPIC_FICK_DIFFUSION_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/diff/nonlinear/VariationalDiffusion.h>
#include <matl/diff/nonlinear/FickChemicalCapacity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for Fickian isotropic diffusion potentials.
 */
template <class ALG>
class IsotropicFickDiffusionPotential
: virtual public VariationalDiffusion<ALG>::DiffusionPotential {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor SYM_TENSOR;
  typedef typename ALG::Vector    VECTOR;

  // default constructor
  IsotropicFickDiffusionPotential() {}
  
  // copy constructor
  IsotropicFickDiffusionPotential(const IsotropicFickDiffusionPotential&) {}

  // destructor
  virtual ~IsotropicFickDiffusionPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic Fickean diffusion***" << std::endl;

    // get reference concentration
    double c0 = material.getDoubleProperty("REFERENCE_CONCENTRATION");
    if (c0 <= 0.0e0) {
      if (os) (*os) << "ERROR: reference concentration must be strictly positive." << std::endl;
      throw InvalidPropertyException("reference concentration");
    }
    if (os) (*os) << "\n\treference concentration = " << c0;

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
        k = d*c0/(R*T0);
        material.setProperty("MOBILITY_COEFFICIENT",k);
        if (os) {
          (*os) << "\n\tdiffusion coefficient   = " << d;
          (*os) << "\n\tuniversal gas constant  = " << R;
          (*os) << "\n\treference temperature   = " << T0;
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
                         const VECTOR& grad,double dc,
                         VECTOR& flux,double& mu,
                         SYM_TENSOR& K,VECTOR& S,double& C,
                         bool computeFirst,bool computeSecond) {

    // get reference concentration
    double c0 = material.getDoubleProperty("REFERENCE_CONCENTRATION");
    double c = c0+dc;

    // get mobility coefficient
    double k = material.getDoubleProperty("MOBILITY_COEFFICIENT");
    
    // compute diffusion energy
    double coef = k*c/c0;
    double X;
    if (computeFirst) {
      flux = coef*grad;
      X = 0.5*(grad*flux);
      mu = X/c;
    }
    else
      X = 0.5*coef*(grad*grad);
    
    if (computeSecond) {
      K = coef*SYM_TENSOR::identity();
      S = (k/c0)*grad;
      C = 0.0e0;
    }
    
    return X;
  }
};


/**
 * Implementations of the model.
 */
class IsotropicFickVariationalDiffusion3D : public VariationalDiffusion<StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicFickVariationalDiffusion3D()
  : VariationalDiffusion<StdTensorAlgebra3D>(new IsotropicFickDiffusionPotential<StdTensorAlgebra3D>(),
                                             new FickChemicalCapacity()) {}
  
  // copy constructor
  IsotropicFickVariationalDiffusion3D(const IsotropicFickVariationalDiffusion3D& src)
  : VariationalDiffusion<StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicFickVariationalDiffusion3D() {}
};
class IsotropicFickVariationalDiffusion2D : public VariationalDiffusion<StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicFickVariationalDiffusion2D()
  : VariationalDiffusion<StdTensorAlgebra2D>(new IsotropicFickDiffusionPotential<StdTensorAlgebra2D>(),
                                             new FickChemicalCapacity()) {}
  
  // copy constructor
  IsotropicFickVariationalDiffusion2D(const IsotropicFickVariationalDiffusion2D& src)
  : VariationalDiffusion<StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicFickVariationalDiffusion2D() {}
};
class IsotropicFickVariationalDiffusion1D : public VariationalDiffusion<StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicFickVariationalDiffusion1D()
  : VariationalDiffusion<StdTensorAlgebra1D>(new IsotropicFickDiffusionPotential<StdTensorAlgebra1D>(),
                                             new FickChemicalCapacity()) {}
  
  // copy constructor
  IsotropicFickVariationalDiffusion1D(const IsotropicFickVariationalDiffusion1D& src)
  : VariationalDiffusion<StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicFickVariationalDiffusion1D() {}
};

/**
 * The associated model builder
 */
class IsotropicFickVariationalDiffusionBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicFickVariationalDiffusionBuilder();
  
  // the instance
  static IsotropicFickVariationalDiffusionBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicFickVariationalDiffusionBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
