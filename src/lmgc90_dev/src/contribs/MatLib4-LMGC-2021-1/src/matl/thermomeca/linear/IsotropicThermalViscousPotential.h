/*
 *  $Id: IsotropicThermalViscousPotential.h 269 2020-04-09 17:36:12Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_LINEAR_ISOTROPIC_VISCOUS_POTENTIAL_H
#define ZORGLIB_MATL_MECA_THERMO_LINEAR_ISOTROPIC_VISCOUS_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/thermomeca/linear/IsotropicThermoElasticity.h>
#include <matl/thermomeca/linear/ThermoViscoElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing isotropic thermal viscous potentials.
 */
template <class ALG>
class IsotropicThermalViscousPotential : virtual public ThermoViscoElasticity<ALG>::ViscousPotential {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  IsotropicThermalViscousPotential() {}
  
  // copy constructor
  IsotropicThermalViscousPotential(const IsotropicThermalViscousPotential&) {}
  
  // destructor
  virtual ~IsotropicThermalViscousPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic temperature-dependent viscous potential***" << std::endl;
    
    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;
        
    // reference temperature
    double T0;
    try {
      T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
      if (T0 <= 0.e0) {
        if (os) (*os) << "ERROR: reference temperature must be strictly positive." << std::endl;
        throw InvalidPropertyException("reference temperature");
      }
    }
    catch (NoSuchPropertyException) {
      // use initial temperature
      try {
        T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
        if (T0 <= 0.e0) {
          if (os) (*os) << "ERROR: initial temperature must be strictly positive." << std::endl;
          throw InvalidPropertyException("initial temperature");
        }
        material.setProperty("REFERENCE_TEMPERATURE",T0);
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: reference temperature cannot be set." << std::endl;
        throw e;
      }
    }

    double E,K,lambda,mu,nu;
    // get Young's modulus
    try {
      try {
        Function& fctE = material.getFunctionProperty("VISCOUS_YOUNG_MODULUS_EVOLUTION");
        E = fctE.value(T0);
        material.setProperty("VISCOUS_YOUNG_MODULUS",E);
        if (os) {
          (*os) << "\n\tviscous Young's modulus temperature dependence: ";
          (*os) << fctE << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        E = material.getDoubleProperty("VISCOUS_YOUNG_MODULUS");
      }
      if (E < 0.e0) {
        if (os) (*os) << "ERROR: viscous Young's modulus must be positive." << std::endl;
        throw InvalidPropertyException("viscous Young's modulus");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: viscous Young's modulus is not defined." << std::endl;
      throw e;
    }

    // get Poisson's coefficient
    try {
      try {
        Function& fctN = material.getFunctionProperty("VISCOUS_POISSON_COEFFICIENT_EVOLUTION");
        nu = fctN.value(T0);
        material.setProperty("VISCOUS_POISSON_COEFFICIENT",nu);
        if (os) {
          (*os) << "\n\tviscous Poisson's coefficient temperature dependence: ";
          (*os) << fctN << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        nu = material.getDoubleProperty("VISCOUS_POISSON_COEFFICIENT");
      }
      if (nu < -1.0e0 || nu > 0.5e0) {
        if (os) (*os) << "ERROR: viscous Poisson's coefficient must be in [-1.0,0.5]." << std::endl;
        throw InvalidPropertyException("viscous Poisson's coefficient");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: viscous Poisson's coefficient is not defined." << std::endl;
      throw e;
    }

    // compute other properties
    mu = 0.5*E/(1.+nu);
    K = ONE_THIRD*E/(1.-2*nu);
    lambda = K-TWO_THIRD*mu;

    material.setProperty("VISCOUS_BULK_MODULUS",K);
    material.setProperty("VISCOUS_SHEAR_MODULUS",mu);
    material.setProperty("VISCOUS_1ST_LAME_CONSTANT",lambda);
    material.setProperty("VISCOUS_2ND_LAME_CONSTANT",mu);
    
    if (os) {
      (*os) << "\n\tAt reference temperature (T = " << T0 << "):" << std::endl;
      (*os) << "\tviscous Young's modulus       = " << E << std::endl;
      (*os) << "\tviscous Poisson's coefficient = " << nu << std::endl;
      (*os) << "\tviscous bulk modulus          = " << K << std::endl;
      (*os) << "\tviscous 1st Lame constant     = " << lambda << std::endl;
      (*os) << "\tviscous 2nd Lame constant     = " << mu << std::endl;
    }
  }
    
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& material,const ParameterSet& extPar) {
    
    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;

    if (!extPar.count("TEMPERATURE")) return;
    double T = extPar.find("TEMPERATURE")->second;
    
    double E,K,lambda,mu,nu;
    // get Young's modulus
    try {
      Function& fctE = material.getFunctionProperty("VISCOUS_YOUNG_MODULUS_EVOLUTION");
      E = fctE.value(T);
      material.setProperty("VISCOUS_YOUNG_MODULUS",E);
    }
    catch (NoSuchPropertyException) {
      E = material.getDoubleProperty("VISCOUS_YOUNG_MODULUS");
    }
    if (E < 0.e0) throw InvalidPropertyException("viscous Young's modulus");
    
    // get Poisson's coefficient
    try {
      Function& fctN = material.getFunctionProperty("VISCOUS_POISSON_COEFFICIENT_EVOLUTION");
      nu = fctN.value(T);
      material.setProperty("VISCOUS_POISSON_COEFFICIENT",nu);
    }
    catch (NoSuchPropertyException) {
      nu = material.getDoubleProperty("VISCOUS_POISSON_COEFFICIENT");
    }
    if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException("viscous Poisson's coefficient");
    
    // compute other properties
    mu = 0.5*E/(1.+nu);
    K = ONE_THIRD*E/(1.-2*nu);
    lambda = K-TWO_THIRD*mu;
    
    material.setProperty("VISCOUS_BULK_MODULUS",K);
    material.setProperty("VISCOUS_SHEAR_MODULUS",mu);
    material.setProperty("VISCOUS_1ST_LAME_CONSTANT",lambda);
    material.setProperty("VISCOUS_2ND_LAME_CONSTANT",mu);
  }
          
  // compute stored energy
  double dissipatedEnergy(const MaterialProperties& material,const ParameterSet& extPar,
                          const SYM_TENSOR& gam,const SYM_TENSOR& gamDot,
                          SYM_TENSOR& sig1,SYM_TENSOR& sig2,
                          SYM_TENSOR4& M11,SYM_TENSOR4& M22,SYM_TENSOR4& M12,
                          double dTime,bool first,bool second) {
    double lambda,mu;
    
    // check for temperature
    if (!extPar.count("TEMPERATURE")) {
      // get elastic constants
      lambda = material.getDoubleProperty("1ST_LAME_CONSTANT");
      mu     = material.getDoubleProperty("2ND_LAME_CONSTANT");
    }
    else {
      double T = extPar.find("TEMPERATURE")->second;
      
      // get viscous constants
      double E,nu;
      try { // get viscous Young's modulus
        Function& fctE = material.getFunctionProperty("VISCOUS_YOUNG_MODULUS_EVOLUTION");
        E = fctE.value(T);
      }
      catch (NoSuchPropertyException) {
        E = material.getDoubleProperty("VISCOUS_YOUNG_MODULUS");
      }
      if (E < 0.e0) throw InvalidPropertyException("viscous Young's modulus");
            
      try { // get viscous Poisson's coefficient
        Function& fctN = material.getFunctionProperty("VISCOUS_POISSON_COEFFICIENT_EVOLUTION");
        nu = fctN.value(T);
      }
      catch (NoSuchPropertyException) {
        nu = material.getDoubleProperty("VISCOUS_POISSON_COEFFICIENT");
      }
      if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException("viscous Poisson's coefficient");
            
      // compute other properties
      mu = 0.5*E/(1.+nu);
      lambda = 2*mu*nu/(1.-2*nu);
    }

    // transform engineering strains
    SYM_TENSOR epsDot = contravariant(gamDot);

    // potential
    double tr = trace(epsDot);
    double norm = innerProd2(epsDot,epsDot);
    double Wv = 0.5*lambda*tr*tr + mu*norm;
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

  // compute stored energy
  double dissipatedThMEnergy(const MaterialProperties& material,const ParameterSet& extPar,
                             const SYM_TENSOR& gam,const SYM_TENSOR& gamDot,double Th,
                             SYM_TENSOR& sig1,SYM_TENSOR& sig2,double& N,
                             SYM_TENSOR4& M11,SYM_TENSOR4& M22,SYM_TENSOR4& M12,
                             SYM_TENSOR& dSig1,SYM_TENSOR& dSig2,double& C,double dTime,
                             bool first,bool second) {
        
    // temperature
    double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    double T = T0+Th;

    // get elastic constants
    double E,nu,lambda,mu;
    double dE,dnu,dlambda,dmu;

    try { // get viscous Young's modulus
      Function& fctE = material.getFunctionProperty("VISCOUS_YOUNG_MODULUS_EVOLUTION");
      E = fctE.value(T,dE);
    }
    catch (NoSuchPropertyException) {
      E = material.getDoubleProperty("VISCOUS_YOUNG_MODULUS");
      dE = 0.0e0;
    }
    if (E < 0.e0) throw InvalidPropertyException("viscous Young's modulus");

    try { // get viscous Poisson's coefficient
      Function& fctN = material.getFunctionProperty("VISCOUS_POISSON_COEFFICIENT_EVOLUTION");
      nu = fctN.value(T,dnu);
    }
    catch (NoSuchPropertyException) {
      nu = material.getDoubleProperty("VISCOUS_POISSON_COEFFICIENT");
      dnu = 0.0e0;
    }
    if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException("viscous Poisson's coefficient");
    
    // compute other properties
    mu = 0.5*E/(1.+nu);
    lambda = 2*mu*nu/(1.-2*nu);
    dmu = 0.5*(dE-E*dnu/(1.+nu))/(1.+nu);
    dlambda = 2*(dmu*nu+mu*dnu/(1.-2*nu))/(1.-2*nu);
    
    // transform engineering strains
    SYM_TENSOR epsDot = contravariant(gamDot);
    
    // potential
    double tr = trace(epsDot);
    double norm = innerProd2(epsDot,epsDot);
    double Wv = 0.5*lambda*tr*tr + mu*norm;
    if (!first && !second) return Wv;
    
    // stress
    static SYM_TENSOR delta = SYM_TENSOR::identity();
    double mu2 = mu+mu;
    if (first) {
      sig1 = 0.0e0;
      sig2 = (lambda*tr)*delta + mu2*epsDot;
      N = 0.5*dlambda*tr*tr + dmu*norm;
    }
    
    // tangent
    if (second) {
      static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
      static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
      double dmu2 = dmu+dmu;
      M11 = 0.0e0;
      M12 = 0.0e0;
      M22 = mu2*I+(3*lambda)*K;
      dSig1 = 0.0e0;
      dSig2 = (dlambda*tr)*delta + dmu2*epsDot;
      C = 0.0e0;
    }
    
    return Wv;
  }
};


/**
 * Implementations of the model.
 */
class IsotropicKelvinThermoViscoElasticity3D : public ThermoViscoElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicKelvinThermoViscoElasticity3D()
  : ThermoElasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra3D>()),
    ThermoViscoElasticity<TensorAlgebra3D>(new IsotropicThermalViscousPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicKelvinThermoViscoElasticity3D(const IsotropicKelvinThermoViscoElasticity3D& src) 
  : ThermoElasticity<TensorAlgebra3D>(src), ThermoViscoElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicKelvinThermoViscoElasticity3D() {}
};
class IsotropicKelvinThermoViscoElasticity2D : public ThermoViscoElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicKelvinThermoViscoElasticity2D()
  : ThermoElasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra2D>()),
    ThermoViscoElasticity<TensorAlgebra2D>(new IsotropicThermalViscousPotential<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicKelvinThermoViscoElasticity2D(const IsotropicKelvinThermoViscoElasticity2D& src) 
  : ThermoElasticity<TensorAlgebra2D>(src), ThermoViscoElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicKelvinThermoViscoElasticity2D() {}
};
class IsotropicKelvinThermoViscoElasticity1D : public ThermoViscoElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicKelvinThermoViscoElasticity1D()
  : ThermoElasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra1D>()),
    ThermoViscoElasticity<TensorAlgebra1D>(new IsotropicThermalViscousPotential<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicKelvinThermoViscoElasticity1D(const IsotropicKelvinThermoViscoElasticity1D& src) 
  : ThermoElasticity<TensorAlgebra1D>(src), ThermoViscoElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicKelvinThermoViscoElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicKelvinThermoViscoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicKelvinThermoViscoElasticityBuilder();
  
  // the instance
  static IsotropicKelvinThermoViscoElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicKelvinThermoViscoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
