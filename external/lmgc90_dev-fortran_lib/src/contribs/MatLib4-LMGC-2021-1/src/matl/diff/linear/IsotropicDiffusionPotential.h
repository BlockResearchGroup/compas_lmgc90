/*
 *  $Id: IsotropicDiffusionPotential.h 163 2015-03-24 10:09:04Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_THERMO_LINEAR_ISOTROPIC_CLASSICAL_DIFFUSION_POTENTIAL_H
#define ZORGLIB_MATL_THERMO_LINEAR_ISOTROPIC_CLASSICAL_DIFFUSION_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/diff/linear/LinearDiffusion.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for classical isotropic diffusion potentials.
 */
template <class ALG>
class IsotropicDiffusionPotential
: virtual public LinearDiffusion<ALG>::DiffusionPotential {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor SYM_TENSOR;
  typedef typename ALG::Vector    VECTOR;

  // default constructor
  IsotropicDiffusionPotential() {}
  
  // copy constructor
  IsotropicDiffusionPotential(const IsotropicDiffusionPotential&) {}

  // destructor
  virtual ~IsotropicDiffusionPotential() {}
  
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
                         const VECTOR& grad,VECTOR& flux,SYM_TENSOR& K,
                         bool computeFirst,bool computeSecond) {

    // get mobility coefficient
    double k = material.getDoubleProperty("MOBILITY_COEFFICIENT");
    
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
class IsotropicLinearDiffusion3D : public LinearDiffusion<StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicLinearDiffusion3D()
  : LinearDiffusion<StdTensorAlgebra3D>(
        new IsotropicDiffusionPotential<StdTensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicLinearDiffusion3D(const IsotropicLinearDiffusion3D& src)
  : LinearDiffusion<StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicLinearDiffusion3D() {}
};
class IsotropicLinearDiffusion2D : public LinearDiffusion<StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicLinearDiffusion2D()
  : LinearDiffusion<StdTensorAlgebra2D>(
        new IsotropicDiffusionPotential<StdTensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicLinearDiffusion2D(const IsotropicLinearDiffusion2D& src)
  : LinearDiffusion<StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicLinearDiffusion2D() {}
};
class IsotropicLinearDiffusion1D : public LinearDiffusion<StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicLinearDiffusion1D()
  : LinearDiffusion<StdTensorAlgebra1D>(
        new IsotropicDiffusionPotential<StdTensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicLinearDiffusion1D(const IsotropicLinearDiffusion1D& src)
  : LinearDiffusion<StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicLinearDiffusion1D() {}
};

/**
 * The associated model builder
 */
class IsotropicLinearDiffusionBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicLinearDiffusionBuilder();
  
  // the instance
  static IsotropicLinearDiffusionBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicLinearDiffusionBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
