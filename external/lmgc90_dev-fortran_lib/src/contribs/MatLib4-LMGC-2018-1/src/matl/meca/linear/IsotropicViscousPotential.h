/*
 *  $Id: IsotropicViscousPotential.h 252 2018-05-18 11:58:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2018, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_VISCOUS_POTENTIAL_H
#define ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_VISCOUS_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/meca/linear/IsotropicElasticPotential.h>
#include <matl/meca/linear/IsotropicLinThermalDilatancy.h>
#include <matl/meca/linear/ViscoElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing isotropic viscous potentials.
 */
template <class ALG>
class IsotropicViscousPotential : virtual public ViscoElasticity<ALG>::ViscousPotential {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  IsotropicViscousPotential() {}
  
  // copy constructor
  IsotropicViscousPotential(const IsotropicViscousPotential&) {}
  
  // destructor
  virtual ~IsotropicViscousPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic viscous potential***" << std::endl;
    
    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;
    
    double E,K,lambda,mu,nu;
    try {
      // get Young's modulus
      E = material.getDoubleProperty("VISCOUS_YOUNG_MODULUS");
      if (E < 0.e0) {
        if (os) (*os) << "ERROR: viscous Young's modulus must be positive." << std::endl;
        throw InvalidPropertyException("viscous Young's modulus");
      }
      
      // get Poisson's coefficient
      nu = material.getDoubleProperty("VISCOUS_POISSON_COEFFICIENT");
      if (nu < -1.0e0 || nu > 0.5e0) {
        if (os) (*os) << "ERROR: viscous Poisson's coefficient must be in [-1.0,0.5]." << std::endl;
        throw InvalidPropertyException("viscous Poisson's coefficient");
      }
      
      // compute other properties
      mu = 0.5*E/(1.+nu);
      K = ONE_THIRD*E/(1.-2*nu);
      lambda = K-TWO_THIRD*mu;
      
      material.setProperty("VISCOUS_BULK_MODULUS",K);
      material.setProperty("VISCOUS_SHEAR_MODULUS",mu);
      material.setProperty("VISCOUS_1ST_LAME_CONSTANT",lambda);
      material.setProperty("VISCOUS_2ND_LAME_CONSTANT",mu);
    }
    catch (NoSuchPropertyException) {
      // get second Lame constant (a.k.a. shear modulus)
      try {
        mu = material.getDoubleProperty("VISCOUS_2ND_LAME_CONSTANT");
        if (mu < 0.0e0) {
          if (os) (*os) << "ERROR: viscous Lame constants must be positive." << std::endl;
          throw InvalidPropertyException("viscous second Lame constant");
        }
      }
      catch (NoSuchPropertyException) {
        try {
          mu = material.getDoubleProperty("VISCOUS_SHEAR_MODULUS");
          if (mu < 0.0e0) {
            if (os) (*os) << "ERROR: viscous shear modulus must be positive." << std::endl;
            throw InvalidPropertyException("viscous shear modulus");
          }
          material.setProperty("VISCOUS_2ND_LAME_CONSTANT",mu);
        }
        catch (NoSuchPropertyException e) {
          if (os) (*os) << "ERROR: viscous second Lame constant is not defined." << std::endl;
          throw e;
        }
      }
      
      // get first Lame constant
      try {
        lambda = material.getDoubleProperty("VISCOUS_1ST_LAME_CONSTANT");
        if (lambda < 0.0e0) {
          if (os) (*os) << "ERROR: viscous Lame constants must be positive." << std::endl;
          throw InvalidPropertyException("viscous first Lame constant");
        }
        K = lambda+TWO_THIRD*mu;
        material.setProperty("VISCOUS_BULK_MODULUS",K);
      }
      catch (NoSuchPropertyException) {
        try {
          K = material.getDoubleProperty("VISCOUS_BULK_MODULUS");
          if (K < 0.0e0) {
            if (os) (*os) << "ERROR: viscous bulk modulus must be positive." << std::endl;
            throw InvalidPropertyException("viscous bulk modulus");
          }
        }
        catch (NoSuchPropertyException) {
          if (os) (*os) << "WARNING: viscous bulk modulus set to zero." << std::endl;
          K = 0.0e0;
          material.setProperty("VISCOUS_BULK_MODULUS",K);
        }
        lambda = K-TWO_THIRD*mu;
        material.setProperty("VISCOUS_1ST_LAME_CONSTANT",lambda);
      }
      
      // compute other properties
      nu = (3*K-2*mu)/(6*K+2*mu);
      E = 2*mu*(1.+nu);
      
      material.setProperty("VISCOUS_YOUNG_MODULUS",E);
      material.setProperty("VISCOUS_POISSON_COEFFICIENT",nu);
    }
    
    if (os) {
      (*os) << "\tviscous Young's modulus       = " << E << std::endl;
      (*os) << "\tviscous Poisson's coefficient = " << nu << std::endl;
      (*os) << "\tviscous bulk modulus          = " << K << std::endl;
      (*os) << "\tviscous 1st Lame constant     = " << lambda << std::endl;
      (*os) << "\tviscous 2nd Lame constant     = " << mu << std::endl;
    }
  }
  
  // compute stored energy
  double dissipatedEnergy(const MaterialProperties& material,const ParameterSet& extPar,
                          const SYM_TENSOR& gam,const SYM_TENSOR& gamDot,
                          SYM_TENSOR& sig1,SYM_TENSOR& sig2,
                          SYM_TENSOR4& M11,SYM_TENSOR4& M22,
                          SYM_TENSOR4& M12,double dTime,bool first,bool second) {
    
    // get elastic constants
    double lambda = material.getDoubleProperty("VISCOUS_1ST_LAME_CONSTANT");
    double mu     = material.getDoubleProperty("VISCOUS_2ND_LAME_CONSTANT");
    
    // transform engineering strains
    SYM_TENSOR epsDot = contravariant(gamDot);
    
    // potential
    double tr = trace(epsDot);
    double Wv = 0.5*lambda*tr*tr + mu*innerProd2(epsDot,epsDot);
    if (!first && !second) return Wv;
    
    // stress
    double mu2 = mu+mu;
    if (first) {
      static SYM_TENSOR delta = SYM_TENSOR::identity();
      sig1 = 0.0e0;
      sig2 = (lambda*tr)*delta + mu2*epsDot;
    }
    
    // tangent
    if (second) {
      static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
      static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
      M11 = 0.0e0;
      M12 = 0.0e0;
      M22 = mu2*I+(3*lambda)*K;
    }
    
    return Wv;
  }
};


/**
 * Implementations of the model.
 */
class IsotropicKelvinViscoElasticity3D : public ViscoElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicKelvinViscoElasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>()),
    ViscoElasticity<TensorAlgebra3D>(new IsotropicViscousPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicKelvinViscoElasticity3D(const IsotropicKelvinViscoElasticity3D& src) 
  : Elasticity<TensorAlgebra3D>(src), ViscoElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicKelvinViscoElasticity3D() {}
};
class IsotropicKelvinViscoElasticity2D : public ViscoElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicKelvinViscoElasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>()),
    ViscoElasticity<TensorAlgebra2D>(new IsotropicViscousPotential<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicKelvinViscoElasticity2D(const IsotropicKelvinViscoElasticity2D& src) 
  : Elasticity<TensorAlgebra2D>(src), ViscoElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicKelvinViscoElasticity2D() {}
};
class IsotropicKelvinViscoElasticity1D : public ViscoElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicKelvinViscoElasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>()),
    ViscoElasticity<TensorAlgebra1D>(new IsotropicViscousPotential<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicKelvinViscoElasticity1D(const IsotropicKelvinViscoElasticity1D& src) 
  : Elasticity<TensorAlgebra1D>(src), ViscoElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicKelvinViscoElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicKelvinViscoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicKelvinViscoElasticityBuilder();
  
  // the instance
  static IsotropicKelvinViscoElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicKelvinViscoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

      
/**
 * Implementations of the model.
 */
class IsotropicThermoDilatantKelvinViscoElasticity3D : public ViscoElasticity<TensorAlgebra3D> {
  
 public:

  // constructor
  IsotropicThermoDilatantKelvinViscoElasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra3D>()),
    ViscoElasticity<TensorAlgebra3D>(new IsotropicViscousPotential<TensorAlgebra3D>()) {}

  // copy constructor
  IsotropicThermoDilatantKelvinViscoElasticity3D(const IsotropicThermoDilatantKelvinViscoElasticity3D& src)
  : Elasticity<TensorAlgebra3D>(src), ViscoElasticity<TensorAlgebra3D>(src) {}

  // destructor
  virtual ~IsotropicThermoDilatantKelvinViscoElasticity3D() {}
};
class IsotropicThermoDilatantKelvinViscoElasticity2D : public ViscoElasticity<TensorAlgebra2D> {
        
 public:
  
  // constructor
  IsotropicThermoDilatantKelvinViscoElasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra2D>()),
    ViscoElasticity<TensorAlgebra2D>(new IsotropicViscousPotential<TensorAlgebra2D>()) {}

  // copy constructor
  IsotropicThermoDilatantKelvinViscoElasticity2D(const IsotropicThermoDilatantKelvinViscoElasticity2D& src)
  : Elasticity<TensorAlgebra2D>(src), ViscoElasticity<TensorAlgebra2D>(src) {}

  // destructor
  virtual ~IsotropicThermoDilatantKelvinViscoElasticity2D() {}
};
class IsotropicThermoDilatantKelvinViscoElasticity1D : public ViscoElasticity<TensorAlgebra1D> {
        
 public:

  // constructor
  IsotropicThermoDilatantKelvinViscoElasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra1D>()),
    ViscoElasticity<TensorAlgebra1D>(new IsotropicViscousPotential<TensorAlgebra1D>()) {}

  // copy constructor
  IsotropicThermoDilatantKelvinViscoElasticity1D(const IsotropicThermoDilatantKelvinViscoElasticity1D& src)
  : Elasticity<TensorAlgebra1D>(src), ViscoElasticity<TensorAlgebra1D>(src) {}

  // destructor
  virtual ~IsotropicThermoDilatantKelvinViscoElasticity1D() {}
};
      
/**
 * The associated model builder
 */
class IsotropicThermoDilatantKelvinViscoElasticityBuilder : public ModelBuilder {
        
 private:

  // constructor
  IsotropicThermoDilatantKelvinViscoElasticityBuilder();

  // the instance
  static IsotropicThermoDilatantKelvinViscoElasticityBuilder const* BUILDER;
        
 public:

  // destructor
  virtual ~IsotropicThermoDilatantKelvinViscoElasticityBuilder() {}
        
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
