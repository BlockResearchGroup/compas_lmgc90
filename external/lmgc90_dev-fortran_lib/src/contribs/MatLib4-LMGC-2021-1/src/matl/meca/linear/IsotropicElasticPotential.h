/*
 *  $Id: IsotropicElasticPotential.h 172 2015-08-24 14:44:39Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_ELASTIC_POTENTIAL_H
#define ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_ELASTIC_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/meca/linear/Elasticity.h>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing elastic isotropic potentials.
 */
template <class ALG>
class IsotropicElasticPotential : virtual public Elasticity<ALG>::Potential {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
    
  // constructor
  IsotropicElasticPotential() {}
  
  // copy constructor
  IsotropicElasticPotential(const IsotropicElasticPotential&) {}
  
  // destructor
  virtual ~IsotropicElasticPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic elastic potential***" << std::endl;

    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;

    double E,K,lambda,mu,nu;
    try {
      // get Young's modulus
      E = material.getDoubleProperty("YOUNG_MODULUS");
      if (E < 0.e0) {
        if (os) (*os) << "ERROR: Young's modulus must be positive." << std::endl;
        throw InvalidPropertyException("Young's modulus");
      }

      // get Poisson's coefficient
      nu = material.getDoubleProperty("POISSON_COEFFICIENT");
      if (nu < -1.0e0 || nu > 0.5e0) {
        if (os) (*os) << "ERROR: Poisson's coefficient must be in [-1.0,0.5]." << std::endl;
        throw InvalidPropertyException("Poisson's coefficient");
      }

      // compute other properties
      mu = 0.5*E/(1.+nu);
      K = ONE_THIRD*E/(1.-2*nu);
      lambda = K-TWO_THIRD*mu;

      material.setProperty("BULK_MODULUS",K);
      material.setProperty("SHEAR_MODULUS",mu);
      material.setProperty("1ST_LAME_CONSTANT",lambda);
      material.setProperty("2ND_LAME_CONSTANT",mu);
    }
    catch (NoSuchPropertyException) {
      // get second Lame constant (a.k.a. shear modulus)
      try {
        mu = material.getDoubleProperty("2ND_LAME_CONSTANT");
        if (mu < 0.0e0) {
          if (os) (*os) << "ERROR: second Lame constant must be positive." << std::endl;
          throw InvalidPropertyException("second Lame constant");
        }
      }
      catch (NoSuchPropertyException) {
        try {
          mu = material.getDoubleProperty("SHEAR_MODULUS");
          if (mu < 0.0e0) {
            if (os) (*os) << "ERROR: shear modulus must be positive." << std::endl;
            throw InvalidPropertyException("shear modulus");
          }
          material.setProperty("2ND_LAME_CONSTANT",mu);
        }
        catch (NoSuchPropertyException e) {
          if (os) (*os) << "ERROR: second Lame constant is not defined." << std::endl;
          throw e;
        }
      }

      // get first Lame constant
      try {
        lambda = material.getDoubleProperty("1ST_LAME_CONSTANT");
        K = lambda+TWO_THIRD*mu;
        if (K < 0.0e0) {
          if (os) (*os) << "ERROR: bulk modulus must be positive." << std::endl;
          throw InvalidPropertyException("first Lame constant");
        }
        material.setProperty("BULK_MODULUS",K);
      }
      catch (NoSuchPropertyException) {
        try {
          K = material.getDoubleProperty("BULK_MODULUS");
          if (K < 0.0e0) {
            if (os) (*os) << "ERROR: bulk modulus must be positive." << std::endl;
            throw InvalidPropertyException("bulk modulus");
          }
        }
        catch (NoSuchPropertyException) {
          if (os) (*os) << "WARNING: bulk modulus set to zero." << std::endl;
          K = 0.0e0;
          material.setProperty("BULK_MODULUS",K);
        }
        lambda = K-TWO_THIRD*mu;
        material.setProperty("1ST_LAME_CONSTANT",lambda);
      }

      // compute other properties
      nu = (3*K-2*mu)/(6*K+2*mu);
      E = 2*mu*(1.+nu);

      material.setProperty("YOUNG_MODULUS",E);
      material.setProperty("POISSON_COEFFICIENT",nu);
    }

    if (os) {
      (*os) << "\tYoung's modulus       = " << E << std::endl;
      (*os) << "\tPoisson's coefficient = " << nu << std::endl;
      (*os) << "\tbulk modulus          = " << K << std::endl;
      (*os) << "\t1st Lame constant     = " << lambda << std::endl;
      (*os) << "\t2nd Lame constant     = " << mu << std::endl;
    }
    
    // compute dilatational elastic wave speed
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      double c = std::sqrt((lambda+2*mu)/rho);
      material.setProperty("CELERITY",c);
      if (os) (*os) << "\n\tcelerity              = " << c << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tcelerity is not defined" << std::endl;
    }
  }
  
  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& gam,SYM_TENSOR& sig,
                      SYM_TENSOR4& M,bool first,bool second) {

    // get elastic constants
    double lambda = material.getDoubleProperty("1ST_LAME_CONSTANT");
    double mu     = material.getDoubleProperty("2ND_LAME_CONSTANT");
    
    // transform engineering strains
    SYM_TENSOR eps = contravariant(gam);

    // potential
    double tr = trace(eps);
    double W = 0.5*lambda*tr*tr + mu*innerProd2(eps,eps);
    if (!first && !second) return W;
    
    // stress
    double mu2 = mu+mu;
    if (first) {
      static SYM_TENSOR delta = SYM_TENSOR::identity();
      sig = (lambda*tr)*delta + mu2*eps;
    }
    
    // tangent
    if (second) {
      static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
      static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
      M = mu2*I+(3*lambda)*K;
    }

    return W;
  }
      
  // compute stored energy (deviatoric part: eps = dev)
  double storedEnergyDev(const MaterialProperties& material,
                         const ParameterSet& extPar,
                         const SYM_TENSOR& gam,SYM_TENSOR& sig,
                         SYM_TENSOR4& M,bool first,bool second) {

    // get shear modulus
    double mu = material.getDoubleProperty("SHEAR_MODULUS");
        
    // transform engineering strains
    SYM_TENSOR eps = contravariant(gam);
        
    // potential (strains are supposed to be deviatoric)
    double W = mu*innerProd2(eps,eps);
    if (!first && !second) return W;
        
    // stress
    double mu2 = mu+mu;
    if (first) sig = mu2*eps;
    
    // tangent
    if (second) {
      static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
      M = mu2*I;
    }
        
    return W;
  }
      
  // compute stored energy (volumic part: eps = trace)
  double storedEnergyVol(const MaterialProperties& material,
                         const ParameterSet& extPar,
                         double eps,double& sig,
                         double M,bool first,bool second) {
        
    // get bulk modulus
    double K = material.getDoubleProperty("BULK_MODULUS");
    
    // potential
    double W = 0.5*K*eps*eps;
    if (!first && !second) return W;

    // stress
    if (first) sig = K*eps;
    
    // tangent
    if (second) M = K;
    
    return W;
  }

  // compute material stiffness (Hooke) tensor
  void computeStiffness(const MaterialProperties& material,
                        const ParameterSet& extPar,SYM_TENSOR4& M) {
    
    // get elastic constants
    double lambda = material.getDoubleProperty("1ST_LAME_CONSTANT");
    double mu2 = 2*material.getDoubleProperty("2ND_LAME_CONSTANT");
    
    // stiffness
    static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
    static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
    M = mu2*I+(3*lambda)*K;
  }
};


/**
 * Implementations of the model.
 */
class IsotropicElasticity3D : public Elasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicElasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicElasticity3D(const IsotropicElasticity3D& src) 
  : Elasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicElasticity3D() {}
};
class IsotropicElasticity2D : public Elasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicElasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicElasticity2D(const IsotropicElasticity2D& src) 
  : Elasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicElasticity2D() {}
};
class IsotropicElasticity1D : public Elasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicElasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicElasticity1D(const IsotropicElasticity1D& src) 
  : Elasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicElasticityBuilder();
  
  // the instance
  static IsotropicElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
