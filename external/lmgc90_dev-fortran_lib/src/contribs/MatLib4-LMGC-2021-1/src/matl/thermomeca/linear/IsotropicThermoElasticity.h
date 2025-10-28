/*
 *  $Id: IsotropicThermoElasticity.h 215 2016-10-06 20:46:04Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_LINEAR_ELASTICITY_ISOTROPIC_H
#define ZORGLIB_MATL_MECA_THERMO_LINEAR_ELASTICITY_ISOTROPIC_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/linear/IsotropicElasticPotential.h>
#include <matl/meca/linear/IsotropicLinThermalDilatancy.h>
#include <matl/thermo/linear/StdLinThermalCapacity.h>
#include <matl/thermomeca/linear/ThermoElasticity.h>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing thermoelastic isotropic potentials.
 */
template <class ALG>
class IsotropicThermoElasticPotential 
: virtual public ThermoElasticity<ALG>::Potential,
  virtual public IsotropicElasticPotential<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  IsotropicThermoElasticPotential() {}
  
  // copy constructor
  IsotropicThermoElasticPotential(const IsotropicThermoElasticPotential&) {}
  
  // destructor
  virtual ~IsotropicThermoElasticPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic thermoelastic potential***" << std::endl;
      
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
        Function& fctE = material.getFunctionProperty("YOUNG_MODULUS_EVOLUTION");
        E = fctE.value(T0);
        material.setProperty("YOUNG_MODULUS",E);
        if (os) {
          (*os) << "\n\tYoung's modulus temperature dependence: ";
          (*os) << fctE << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        E = material.getDoubleProperty("YOUNG_MODULUS");
      }
      if (E < 0.e0) {
        if (os) (*os) << "ERROR: Young's modulus must be positive." << std::endl;
        throw InvalidPropertyException("Young's modulus");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: Young's modulus is not defined." << std::endl;
      throw e;
    }
      
    // get Poisson's coefficient
    try {
      try {
        Function& fctN = material.getFunctionProperty("POISSON_COEFFICIENT_EVOLUTION");
        nu = fctN.value(T0);
        material.setProperty("POISSON_COEFFICIENT",nu);
        if (os) {
          (*os) << "\n\tPoisson's coefficient temperature dependence: ";
          (*os) << fctN << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        nu = material.getDoubleProperty("POISSON_COEFFICIENT");
      }
      if (nu < -1.0e0 || nu > 0.5e0) {
        if (os) (*os) << "ERROR: Poisson's coefficient must be in [-1.0,0.5]." << std::endl;
        throw InvalidPropertyException("Poisson's coefficient");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: Poisson's coefficient is not defined." << std::endl;
      throw e;
    }
        
    // compute other properties
    mu = 0.5*E/(1.+nu);
    K = ONE_THIRD*E/(1.-2*nu);
    lambda = K-TWO_THIRD*mu;

    material.setProperty("BULK_MODULUS",K);
    material.setProperty("SHEAR_MODULUS",mu);
    material.setProperty("1ST_LAME_CONSTANT",lambda);
    material.setProperty("2ND_LAME_CONSTANT",mu);
      
    if (os) {
      (*os) << "\n\tAt reference temperature (T = " << T0 << "):" << std::endl;
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
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& material,const ParameterSet& extPar) {
    
    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;

    if (!extPar.count("TEMPERATURE")) return;
    double T = extPar.find("TEMPERATURE")->second;
    
    double E,K,lambda,mu,nu;
    // get Young's modulus
    try {
      Function& fctE = material.getFunctionProperty("YOUNG_MODULUS_EVOLUTION");
      E = fctE.value(T);
      material.setProperty("YOUNG_MODULUS",E);
    }
    catch (NoSuchPropertyException) {
      E = material.getDoubleProperty("YOUNG_MODULUS");
    }
    if (E < 0.e0) throw InvalidPropertyException("Young's modulus");
    
    // get Poisson's coefficient
    try {
      Function& fctN = material.getFunctionProperty("POISSON_COEFFICIENT_EVOLUTION");
      nu = fctN.value(T);
      material.setProperty("POISSON_COEFFICIENT",nu);
    }
    catch (NoSuchPropertyException) {
      nu = material.getDoubleProperty("POISSON_COEFFICIENT");
    }
    if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException("Poisson's coefficient");
    
    // compute other properties
    mu = 0.5*E/(1.+nu);
    K = ONE_THIRD*E/(1.-2*nu);
    lambda = K-TWO_THIRD*mu;
    
    material.setProperty("BULK_MODULUS",K);
    material.setProperty("SHEAR_MODULUS",mu);
    material.setProperty("1ST_LAME_CONSTANT",lambda);
    material.setProperty("2ND_LAME_CONSTANT",mu);
    
    // compute dilatational elastic wave speed
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      double c = std::sqrt((lambda+2*mu)/rho);
      material.setProperty("CELERITY",c);
    }
    catch (NoSuchPropertyException) {
      // nothing to do!
    }
  }
          
  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& gam,SYM_TENSOR& sig,
                      SYM_TENSOR4& M,bool first,bool second) {
    double lambda,mu;

    // check for temperature
    if (!extPar.count("TEMPERATURE")) {
      // get elastic constants
      lambda = material.getDoubleProperty("1ST_LAME_CONSTANT");
      mu     = material.getDoubleProperty("2ND_LAME_CONSTANT");
    }
    else {
      double T = extPar.find("TEMPERATURE")->second;
      
      double E,nu;
      try { // get Young's modulus
        Function& fctE = material.getFunctionProperty("YOUNG_MODULUS_EVOLUTION");
        E = fctE.value(T);
      }
      catch (NoSuchPropertyException) {
        E = material.getDoubleProperty("YOUNG_MODULUS");
      }
      if (E < 0.e0) throw InvalidPropertyException("Young's modulus");
      
      try { // get Poisson's coefficient
        Function& fctN = material.getFunctionProperty("POISSON_COEFFICIENT_EVOLUTION");
        nu = fctN.value(T);
      }
      catch (NoSuchPropertyException) {
        nu = material.getDoubleProperty("POISSON_COEFFICIENT");
      }
      if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException("Poisson's coefficient");
      
      // compute other properties
      mu = 0.5*E/(1.+nu);
      lambda = 2*mu*nu/(1.-2*nu);
    }
  
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

  // compute stored energy
  double storedThMEnergy(const MaterialProperties& material,
                         const ParameterSet& extPar,
                         const SYM_TENSOR& gam,double Th,SYM_TENSOR& sig,double& N,
                         SYM_TENSOR4& M,SYM_TENSOR& dSig,double& C,
                         bool first,bool second) {
    
    // temperature
    double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    double T = T0+Th;
    
    // get elastic constants
    double E,nu,lambda,mu;
    double dE,dnu,dlambda,dmu;

    try { // get Young's modulus
      Function& fctE = material.getFunctionProperty("YOUNG_MODULUS_EVOLUTION");
      E = fctE.value(T,dE);
    }
    catch (NoSuchPropertyException) {
      E = material.getDoubleProperty("YOUNG_MODULUS");
      dE = 0.0e0;
    }
    if (E < 0.e0) throw InvalidPropertyException("Young's modulus");

    try { // get Poisson's coefficient
      Function& fctN = material.getFunctionProperty("POISSON_COEFFICIENT_EVOLUTION");
      nu = fctN.value(T,dnu);
    }
    catch (NoSuchPropertyException) {
      nu = material.getDoubleProperty("POISSON_COEFFICIENT");
      dnu = 0.0e0;
    }
    if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException("Poisson's coefficient");
    
    // compute other properties
    mu = 0.5*E/(1.+nu);
    lambda = 2*mu*nu/(1.-2*nu);
    dmu = 0.5*(dE-E*dnu/(1.+nu))/(1.+nu);
    dlambda = 2*(dmu*nu+mu*dnu/(1.-2*nu))/(1.-2*nu);
    
    // transform engineering strains
    SYM_TENSOR eps = contravariant(gam);
    
    // potential
    double tr = trace(eps);
    double norm = innerProd2(eps,eps);
    double W = 0.5*lambda*tr*tr + mu*norm;
    if (!first && !second) return W;
    
    // stress
    static SYM_TENSOR delta = SYM_TENSOR::identity();
    double mu2 = mu+mu;
    if (first) {
      sig = (lambda*tr)*delta + mu2*eps;
      N = 0.5*dlambda*tr*tr + dmu*norm;
    }
    
    // tangent
    if (second) {
      static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
      static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
      double dmu2 = dmu+dmu;
      M = mu2*I+(3*lambda)*K;
      dSig = (dlambda*tr)*delta + dmu2*eps;
      C = 0.0e0;
    }
    
    return W;
  }
          
  // compute material stiffness (Hooke) tensor
  void computeStiffness(const MaterialProperties& material,
                        const ParameterSet& extPar,SYM_TENSOR4& M) {
    double lambda,mu;
    
    // check for temperature
    if (!extPar.count("TEMPERATURE")) {
      // get elastic constants
      lambda = material.getDoubleProperty("1ST_LAME_CONSTANT");
      mu     = material.getDoubleProperty("2ND_LAME_CONSTANT");
    }
    else {
      double T = extPar.find("TEMPERATURE")->second;
      
      double E,nu;
      try { // get Young's modulus
        Function& fctE = material.getFunctionProperty("YOUNG_MODULUS_EVOLUTION");
        E = fctE.value(T);
      }
      catch (NoSuchPropertyException) {
        E = material.getDoubleProperty("YOUNG_MODULUS");
      }
      if (E < 0.e0) throw InvalidPropertyException("Young's modulus");
      
      try { // get Poisson's coefficient
        Function& fctN = material.getFunctionProperty("POISSON_COEFFICIENT_EVOLUTION");
        nu = fctN.value(T);
      }
      catch (NoSuchPropertyException) {
        nu = material.getDoubleProperty("POISSON_COEFFICIENT");
      }
      if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException("Poisson's coefficient");
      
      // compute other properties
      mu = 0.5*E/(1.+nu);
      lambda = 2*mu*nu/(1.-2*nu);
    }
    
    // stiffness
    static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
    static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
    M = (mu+mu)*I+(3*lambda)*K;
  }
};

/**
 * Class describing isotropic thermoelastic dilatancy models.
 */
template <class ALG>
class IsotropicThermoElasticDilatancy 
: virtual public ThermoElasticity<ALG>::Dilatancy,
  virtual public IsotropicLinThermalDilatancy<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  IsotropicThermoElasticDilatancy() {}
  
  // copy constructor
  IsotropicThermoElasticDilatancy(const IsotropicThermoElasticDilatancy&) {}
  
  // destructor
  virtual ~IsotropicThermoElasticDilatancy() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic thermoelastic dilatancy***" << std::endl;

    double alpha,K,T0,TRef;

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

    // get initial temperature
    try {
      T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: initial temperature is not defined." << std::endl;
      throw e;
    }
    
    // get reference temperature
    try {
      TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    }
    catch (NoSuchPropertyException) {
      // use initial temperature
      TRef = T0;
      material.setProperty("REFERENCE_TEMPERATURE",TRef);
    }
    

    // get bulk modulus
    try {
      try {
        Function& fctK = material.getFunctionProperty("BULK_MODULUS_EVOLUTION");
        K = fctK.value(TRef);
        material.setProperty("BULK_MODULUS",K);
        if (os) {
          (*os) << "\n\tbulk modulus temperature dependence: ";
          (*os) << fctK << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        K = material.getDoubleProperty("BULK_MODULUS");
      }
      if (K < 0.0e0) {
        if (os) (*os) << "ERROR: bulk modulus must be positive." << std::endl;
        throw InvalidPropertyException("bulk modulus");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: bulk modulus is not defined." << std::endl;
      throw e;
    }

    if (os) {
      (*os) << "\tthermal dilatation coefficient = " << alpha << std::endl;
      (*os) << "\tinitial temperature            = " << T0    << std::endl;
      (*os) << "\treference temperature          = " << TRef  << std::endl;
      (*os) << "\tbulk modulus                   = " << K     << std::endl;
    }
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& material,const ParameterSet& extPar) {
    
    if (!extPar.count("TEMPERATURE")) return;
    double T = extPar.find("TEMPERATURE")->second;
    
    // get bulk modulus
    double K;
    try {
      Function& fctK = material.getFunctionProperty("BULK_MODULUS_EVOLUTION");
      K = fctK.value(T);
      material.setProperty("BULK_MODULUS",K);
    }
    catch (NoSuchPropertyException) {
      K = material.getDoubleProperty("BULK_MODULUS");
    }
    if (K < 0.e0) throw InvalidPropertyException("bulk modulus");
  }
    
  // compute coupling energy
  double couplingThMEnergy(const MaterialProperties& material,
                           const ParameterSet& extPar,
                           const SYM_TENSOR& eps,double Th,SYM_TENSOR& sig,double& N,
                           SYM_TENSOR4& M,SYM_TENSOR& dSig,double& C,
                           bool first,bool second) {
    
    static const double ONE_THIRD = 1.e0/3.e0;
    
    // temperature
    double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
    double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    double T = TRef+Th;
    
    // get material parameters
    double alpha = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT");
    double K,dK;
    try {
      Function& fctK = material.getFunctionProperty("BULK_MODULUS_EVOLUTION");
      K = fctK.value(T,dK);
    }
    catch (NoSuchPropertyException) {
      try {
        double E,dE;
        try { // get Young's modulus
          Function& fctE = material.getFunctionProperty("YOUNG_MODULUS_EVOLUTION");
          E = fctE.value(T,dE);
        }
        catch (NoSuchPropertyException) {
          E = material.getDoubleProperty("YOUNG_MODULUS");
          dE = 0.0e0;
        }
        if (E < 0.0e0) throw InvalidPropertyException("Young's modulus");

        double nu,dnu;
        try { // get Poisson's coefficient
          Function& fctN = material.getFunctionProperty("POISSON_COEFFICIENT_EVOLUTION");
          nu = fctN.value(T,dnu);
        }
        catch (NoSuchPropertyException) {
          nu = material.getDoubleProperty("POISSON_COEFFICIENT");
          dnu = 0.0e0;
        }
        if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException("Poisson's coefficient");

        K = ONE_THIRD*E/(1.-2*nu);
        dK = ONE_THIRD*(dE+2*E*dnu/(1.-2*nu))/(1.-2*nu);
      }
      catch (NoSuchPropertyException) {
        K = material.getDoubleProperty("BULK_MODULUS");
        dK = 0.0e0;
      }
    }
    if (K < 0.0e0) throw InvalidPropertyException("bulk modulus");
    
    // compute coupling energy
    static const SYM_TENSOR delta = SYM_TENSOR::identity();
    double tr = trace(eps);
    double val = -3*alpha;
    double dT = T-T0;
    double coef = val*K*dT;
    double W = coef*tr;
    if (first) {
      sig = coef*delta;
      N = val*tr*(K+dK*dT);
    }
    if (second) {
      M = 0.0e0;
      dSig = val*(K+dK*dT)*delta;
      C = val*tr*dK;
    }
    
    return W;
  }
};


/**
 * Implementations of the model.
 */
class IsotropicThermoElasticity3D : public ThermoElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicThermoElasticity3D()
  : ThermoElasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicThermoElasticity3D(const IsotropicThermoElasticity3D& src) 
  : ThermoElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoElasticity3D() {}
};
class IsotropicThermoElasticity2D : public ThermoElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicThermoElasticity2D()
  : ThermoElasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicThermoElasticity2D(const IsotropicThermoElasticity2D& src) 
  : ThermoElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoElasticity2D() {}
};
class IsotropicThermoElasticity1D : public ThermoElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicThermoElasticity1D()
  : ThermoElasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicThermoElasticity1D(const IsotropicThermoElasticity1D& src) 
  : ThermoElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicThermoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicThermoElasticityBuilder();
  
  // the instance
  static IsotropicThermoElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicThermoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
