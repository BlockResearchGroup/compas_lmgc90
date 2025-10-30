/*
 *  $Id: IsotropicThermoViscoElasticMultiPotential.h 269 2020-04-09 17:36:12Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2020, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_THERMOMECA_LINEAR_ISOTROPIC_THERMO_VISCO_ELASTIC_MULTI_POTENTIAL_H
#define ZORGLIB_MATL_THERMOMECA_LINEAR_ISOTROPIC_THERMO_VISCO_ELASTIC_MULTI_POTENTIAL_H

// std C library
#include <cstdio>
// local
#include <matl/meca/linear/IsotropicViscoElasticMultiPotential.h>
#include <matl/thermomeca/linear/IsotropicThermalViscousPotential.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing isotropic thermo-visco-elasticity models:
 * elastic part.
 */
template <class ALG>
class IsotropicThermoElasticMultiPotential : virtual public ThermoElasticity<ALG>::Potential,
                                             virtual public IsotropicElasticMultiPotential<ALG> {

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

  // constructor
  IsotropicThermoElasticMultiPotential(unsigned int r,bool i)
  : IsotropicElasticMultiPotential<ALG>(r,i) {}

  // copy constructor
  IsotropicThermoElasticMultiPotential(const IsotropicThermoElasticMultiPotential& src)
  : IsotropicElasticMultiPotential<ALG>(src) {}

  // destructor
  virtual ~IsotropicThermoElasticMultiPotential() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) {
      (*os) << "\n\t***Isotropic thermoviscoelastic branch #";
      (*os) << this->rank << " (elastic part)***" << std::endl;
    }

    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;

    char str[64],str1[64],str2[64];
    double E,K=0.0e0,lambda=0.0e0,mu,nu;

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

    // get Young's modulus
    try {
      std::sprintf(str1,"YOUNG_MODULUS_%u",this->rank);
      std::sprintf(str2,"YOUNG_MODULUS_EVOLUTION_%u",this->rank);
      try {
        Function& fctE = material.getFunctionProperty(str2);
        E = fctE.value(T0);
        material.setProperty(str1,E);
        if (os) {
          (*os) << "\n\tYoung's modulus temperature dependence: ";
          (*os) << fctE << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        E = material.getDoubleProperty(str1);
      }
      if (E < 0.e0) {
        if (os) (*os) << "ERROR: Young's modulus must be positive." << std::endl;
        throw InvalidPropertyException(str1);
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: Young's modulus is not defined." << std::endl;
      throw e;
    }

    // get Poisson's coefficient
    try {
      std::sprintf(str1,"POISSON_COEFFICIENT_%u",this->rank);
      std::sprintf(str2,"POISSON_COEFFICIENT_EVOLUTION_%u",this->rank);
      try {
        Function& fctN = material.getFunctionProperty(str2);
        nu = fctN.value(T0);
        material.setProperty(str1,nu);
        if (os) {
          (*os) << "\n\tPoisson's coefficient temperature dependence: ";
          (*os) << fctN << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        nu = material.getDoubleProperty(str1);
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

    std::sprintf(str,"BULK_MODULUS_%u",this->rank);
    material.setProperty(str,K);
    std::sprintf(str,"SHEAR_MODULUS_%u",this->rank);
    material.setProperty(str,mu);
    std::sprintf(str,"1ST_LAME_CONSTANT_%u",this->rank);
    material.setProperty(str,lambda);
    std::sprintf(str,"2ND_LAME_CONSTANT_%u",this->rank);
    material.setProperty(str,mu);

    if (os) {
      (*os) << "\n\tAt reference temperature (T = " << T0 << "):" << std::endl;
      (*os) << "\tYoung's modulus       = " << E << std::endl;
      (*os) << "\tPoisson's coefficient = " << nu << std::endl;
      if (this->isochoric) {
        (*os) << "\tshear modulus         = " << mu << std::endl;
      }
      else {
        (*os) << "\tbulk modulus          = " << K << std::endl;
        (*os) << "\t1st Lame constant     = " << lambda << std::endl;
        (*os) << "\t2nd Lame constant     = " << mu << std::endl;
      }
    }
      
    // compute dilatational elastic wave speed
    try {
      std::sprintf(str,"MASS_DENSITY_%u",this->rank);
      double rho = material.getDoubleProperty(str);
      double c = std::sqrt((lambda+2*mu)/rho);
      std::sprintf(str,"CELERITY_%u",this->rank);
      material.setProperty(str,c);
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
    char str[64],str1[64],str2[64];

    // get Young's modulus
    try {
      std::sprintf(str1,"YOUNG_MODULUS_%u",this->rank);
      std::sprintf(str2,"YOUNG_MODULUS_EVOLUTION_%u",this->rank);
      Function& fctE = material.getFunctionProperty(str2);
      E = fctE.value(T);
      material.setProperty(str1,E);
    }
    catch (NoSuchPropertyException) {
      E = material.getDoubleProperty(str1);
    }
    if (E < 0.e0) throw InvalidPropertyException(str1);

    // get Poisson's coefficient
    try {
      std::sprintf(str1,"POISSON_COEFFICIENT_%u",this->rank);
      std::sprintf(str2,"POISSON_COEFFICIENT_EVOLUTION_%u",this->rank);
      Function& fctN = material.getFunctionProperty(str2);
      nu = fctN.value(T);
      material.setProperty(str1,nu);
    }
    catch (NoSuchPropertyException) {
      nu = material.getDoubleProperty(str1);
    }
    if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException("Poisson's coefficient");

    // compute other properties
    mu = 0.5*E/(1.+nu);
    K = ONE_THIRD*E/(1.-2*nu);
    lambda = K-TWO_THIRD*mu;

    std::sprintf(str,"BULK_MODULUS_%u",this->rank);
    material.setProperty(str,K);
    std::sprintf(str,"SHEAR_MODULUS_%u",this->rank);
    material.setProperty(str,mu);
    std::sprintf(str,"1ST_LAME_CONSTANT_%u",this->rank);
    material.setProperty(str,lambda);
    std::sprintf(str,"2ND_LAME_CONSTANT_%u",this->rank);
    material.setProperty(str,mu);

    // compute dilatational elastic wave speed
    try {
      std::sprintf(str,"MASS_DENSITY");
      double rho = material.getDoubleProperty(str);
      double c = std::sqrt((lambda+2*mu)/rho);
      std::sprintf(str,"CELERITY");
      material.setProperty(str,c);
    }
    catch (NoSuchPropertyException) {
      // nothing to do
    }
  }

  // compute stored energy
  double storedEnergy(const MaterialProperties& material,const ParameterSet& extPar,
                      const SYM_TENSOR& gam,SYM_TENSOR& sig,SYM_TENSOR4& M,
                      bool first,bool second) {

    // get elastic constants
    char str1[64],str2[64];
    double E,nu;

    // check for temperature
    if (!extPar.count("TEMPERATURE")) {
      // Young modulus
      std::sprintf(str1,"YOUNG_MODULUS_%u",this->rank);
      E = material.getDoubleProperty(str1);
      if (E < 0.e0) throw InvalidPropertyException(str1);
      // Poisson's coefficient
      std::sprintf(str1,"POISSON_COEFFICIENT_%u",this->rank);
      nu = material.getDoubleProperty(str1);
      if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException(str1);
    }
    else {
      double T = extPar.find("TEMPERATURE")->second;

      try { // get Young's modulus
        std::sprintf(str1,"YOUNG_MODULUS_%u",this->rank);
        std::sprintf(str2,"YOUNG_MODULUS_EVOLUTION_%u",this->rank);
        Function& fctE = material.getFunctionProperty(str2);
        E = fctE.value(T);
      }
      catch (NoSuchPropertyException) {
        E = material.getDoubleProperty(str1);
      }
      if (E < 0.e0) throw InvalidPropertyException(str1);

      try { // get Poisson's coefficient
        std::sprintf(str1,"POISSON_COEFFICIENT_%u",this->rank);
        std::sprintf(str2,"POISSON_COEFFICIENT_EVOLUTION_%u",this->rank);
        Function& fctN = material.getFunctionProperty(str2);
        nu = fctN.value(T);
      }
      catch (NoSuchPropertyException) {
        nu = material.getDoubleProperty(str1);
      }
      if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException(str1);
    }

    // compute other properties
    double mu = 0.5*E/(1.+nu);
    double mu2 = mu+mu;
    double lambda = 2*mu*nu/(1.-2*nu);

    // transform engineering strains
    SYM_TENSOR eps = contravariant(gam);

    // compute deviatoric part of energy
    double W = mu*innerProd2(eps,eps);
    if (first) {
      sig = mu2*eps;
    }
    if (second) {
      static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
      M = mu2*I;
    }

    if (!this->isochoric) {
      // compute volumic part of energy
      double tr = trace(eps);
      W += 0.5*lambda*tr*tr;
      if (first) {
        static SYM_TENSOR delta = SYM_TENSOR::identity();
        sig += (lambda*tr)*delta;
      }
      if (second) {
        static SYM_TENSOR delta = SYM_TENSOR::identity();
        static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
        M += (3*lambda)*K;
      }
    }

    return W;
  }
      
  // compute stored energy
  double storedThMEnergy(const MaterialProperties& material,const ParameterSet& extPar,
                         const SYM_TENSOR& gam,double Th,SYM_TENSOR& sig,double& N,
                         SYM_TENSOR4& M,SYM_TENSOR& dSig,double& C,bool first,bool second) {
        
    // temperature
    double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    double T = T0+Th;
        
    // get elastic constants
    char str1[64],str2[64];
    double E,nu;
    double dE,dnu;
        
    try { // get Young's modulus
      std::sprintf(str1,"YOUNG_MODULUS_%u",this->rank);
      std::sprintf(str2,"YOUNG_MODULUS_EVOLUTION_%u",this->rank);
      Function& fctE = material.getFunctionProperty(str2);
      E = fctE.value(T,dE);
    }
    catch (NoSuchPropertyException) {
      E = material.getDoubleProperty(str1);
      dE = 0.0e0;
    }
    if (E < 0.e0) throw InvalidPropertyException(str1);
        
    try { // get Poisson's coefficient
      std::sprintf(str1,"POISSON_COEFFICIENT_%u",this->rank);
      std::sprintf(str2,"POISSON_COEFFICIENT_EVOLUTION_%u",this->rank);
      Function& fctN = material.getFunctionProperty(str2);
      nu = fctN.value(T,dnu);
    }
    catch (NoSuchPropertyException) {
      nu = material.getDoubleProperty(str1);
      dnu = 0.0e0;
    }
    if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException(str1);
        
    // compute other properties
    double mu = 0.5*E/(1.+nu);
    double lambda = 2*mu*nu/(1.-2*nu);
    double dmu = 0.5*(dE-E*dnu/(1.+nu))/(1.+nu);
    double dlambda = 2*(dmu*nu+mu*dnu/(1.-2*nu))/(1.-2*nu);
        
    double mu2 = mu+mu;
    double dmu2 = dmu+dmu;
        
    // transform engineering strains
    SYM_TENSOR eps = contravariant(gam);
        
    // compute deviatoric part of energy
    double W = mu*innerProd2(eps,eps);
    if (first) {
      sig = mu2*eps;
      N = dmu*innerProd2(eps,eps);
    }
    if (second) {
      static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
      M = mu2*I;
      dSig = dmu2*eps;
      C = 0.0e0;
    }
        
    if (!this->isochoric) {
      // compute volumic part of energy
      double tr = trace(eps);
      W += 0.5*lambda*tr*tr;
      if (first) {
        static SYM_TENSOR delta = SYM_TENSOR::identity();
        sig += (lambda*tr)*delta;
        N += 0.5*dlambda*tr*tr;
      }
      if (second) {
        static SYM_TENSOR delta = SYM_TENSOR::identity();
        static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
        M += (3*lambda)*K;
        dSig += (dlambda*tr)*delta;
        C += 0.0e0;
      }
    }
        
    return W;
  }
      
  // compute material stiffness (Hooke) tensor
  void computeStiffness(const MaterialProperties& material,
                        const ParameterSet& extPar,SYM_TENSOR4& M) {
    
    // get elastic constants
    char str1[64],str2[64];
    double E,nu;
    
    // check for temperature
    if (!extPar.count("TEMPERATURE")) {
      // Young modulus
      std::sprintf(str1,"YOUNG_MODULUS_%u",this->rank);
      E = material.getDoubleProperty(str1);
      if (E < 0.e0) throw InvalidPropertyException(str1);
      // Poisson's coefficient
      std::sprintf(str1,"POISSON_COEFFICIENT_%u",this->rank);
      nu = material.getDoubleProperty(str1);
      if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException(str1);
    }
    else {
      double T = extPar.find("TEMPERATURE")->second;
      
      try { // get Young's modulus
        std::sprintf(str1,"YOUNG_MODULUS_%u",this->rank);
        std::sprintf(str2,"YOUNG_MODULUS_EVOLUTION_%u",this->rank);
        Function& fctE = material.getFunctionProperty(str2);
        E = fctE.value(T);
      }
      catch (NoSuchPropertyException) {
        E = material.getDoubleProperty(str1);
      }
      if (E < 0.e0) throw InvalidPropertyException(str1);
      
      try { // get Poisson's coefficient
        std::sprintf(str1,"POISSON_COEFFICIENT_%u",this->rank);
        std::sprintf(str2,"POISSON_COEFFICIENT_EVOLUTION_%u",this->rank);
        Function& fctN = material.getFunctionProperty(str2);
        nu = fctN.value(T);
      }
      catch (NoSuchPropertyException) {
        nu = material.getDoubleProperty(str1);
      }
      if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException(str1);
    }
    
    // compute other properties
    double mu = 0.5*E/(1.+nu);
    double mu2 = mu+mu;
    double lambda = 2*mu*nu/(1.-2*nu);
    
    // stiffness
    static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
    static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
    M = mu2*I+(3*lambda)*K;
  }
};
      
      
/**
 * Class describing isotropic themo-visco-elasticity models:
 * viscous part.
 */
template <class ALG>
class IsotropicThermoViscousMultiPotential
: virtual public ThermoViscoElasticity<ALG>::ViscousPotential {

 protected:

  // rank of viscoelastic module
  unsigned int rank;

  // isochoric?
  bool isochoric;

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

  // constructor
  IsotropicThermoViscousMultiPotential(unsigned int r,bool i) {
    rank = r;
    isochoric = i;
  }

  // copy constructor
  IsotropicThermoViscousMultiPotential(const IsotropicThermoViscousMultiPotential& src) {
    rank = src.rank;
    isochoric = src.isochoric;
  }

  // destructor
  virtual ~IsotropicThermoViscousMultiPotential() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {

    if (os) {
      (*os) << "\n\t***Isotropic thermo-viscoelastic branch #";
      (*os) << rank << " (viscous part)***" << std::endl;
    }

    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;

    char str[64],str1[64],str2[64];
    double E,K=0.0e0,lambda=0.0e0,mu,nu;

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

    // get viscous Young's modulus
    try {
      std::sprintf(str1,"VISCOUS_YOUNG_MODULUS_%u",rank);
      std::sprintf(str2,"VISCOUS_YOUNG_MODULUS_EVOLUTION_%u",rank);
      try {
        Function& fctE = material.getFunctionProperty(str2);
        E = fctE.value(T0);
        material.setProperty(str1,E);
        if (os) {
          (*os) << "\n\tViscous Young's modulus temperature dependence: ";
          (*os) << fctE << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        E = material.getDoubleProperty(str1);
      }
      if (E < 0.e0) {
        if (os) (*os) << "ERROR: viscous Young's modulus must be positive." << std::endl;
        throw InvalidPropertyException(str1);
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: viscous Young's modulus is not defined." << std::endl;
      throw e;
    }

    // get viscous Poisson's coefficient
    try {
      std::sprintf(str1,"VISCOUS_POISSON_COEFFICIENT_%u",rank);
      std::sprintf(str2,"VISCOUS_POISSON_COEFFICIENT_EVOLUTION_%u",rank);
      try {
        Function& fctN = material.getFunctionProperty(str2);
        nu = fctN.value(T0);
        material.setProperty(str1,nu);
        if (os) {
          (*os) << "\n\tViscous Poisson's coefficient temperature dependence: ";
          (*os) << fctN << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        nu = material.getDoubleProperty(str1);
      }
      if (nu < -1.0e0 || nu > 0.5e0) {
        if (os) (*os) << "ERROR: viscous Poisson's coefficient must be in [-1.0,0.5]." << std::endl;
        throw InvalidPropertyException("Poisson's coefficient");
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

    std::sprintf(str,"VISCOUS_BULK_MODULUS_%u",rank);
    material.setProperty(str,K);
    std::sprintf(str,"VISCOUS_SHEAR_MODULUS_%u",rank);
    material.setProperty(str,mu);
    std::sprintf(str,"VISCOUS_1ST_LAME_CONSTANT_%u",rank);
    material.setProperty(str,lambda);
    std::sprintf(str,"VISCOUS_2ND_LAME_CONSTANT_%u",rank);
    material.setProperty(str,mu);

    if (os) {
      (*os) << "\tviscous Young's modulus       = " << E << std::endl;
      (*os) << "\tviscous Poisson's coefficient = " << nu << std::endl;
      if (isochoric) {
        (*os) << "\tviscous shear modulus         = " << mu << std::endl;
      }
      else {
        (*os) << "\tviscous bulk modulus          = " << K << std::endl;
        (*os) << "\tviscous 1st Lame constant     = " << lambda << std::endl;
        (*os) << "\tviscous 2nd Lame constant     = " << mu << std::endl;
      }
    }
  }
  
  // compute dissipated energy
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
      char str1[64],str2[64];
      double E,nu;
      try { // get viscous Young's modulus
        std::sprintf(str1,"VISCOUS_YOUNG_MODULUS_%u",rank);
        std::sprintf(str2,"VISCOUS_YOUNG_MODULUS_EVOLUTION_%u",rank);
        Function& fctE = material.getFunctionProperty(str2);
        E = fctE.value(T);
      }
      catch (NoSuchPropertyException) {
        E = material.getDoubleProperty(str1);
      }
      if (E < 0.e0) throw InvalidPropertyException(str1);
    
      try { // get viscous Poisson's coefficient
        std::sprintf(str1,"VISCOUS_POISSON_COEFFICIENT_%u",rank);
        std::sprintf(str2,"VISCOUS_POISSON_COEFFICIENT_EVOLUTION_%u",rank);
        Function& fctN = material.getFunctionProperty(str2);
        nu = fctN.value(T);
      }
      catch (NoSuchPropertyException) {
        nu = material.getDoubleProperty(str1);
      }
      if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException(str1);
    
      // compute other properties
      mu = 0.5*E/(1.+nu);
      lambda = 2*mu*nu/(1.-2*nu);
    }
    
    // transform engineering strains
    SYM_TENSOR epsDot = contravariant(gamDot);
    
    // compute deviatoric part of energy
    double Wv = mu*innerProd2(epsDot,epsDot);
    double mu2 = mu+mu;
    if (first) {
      sig1 = 0.0e0;
      sig2 = mu2*epsDot;
    }
    if (second) {
      static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
      M11 = 0.0e0;
      M12 = 0.0e0;
      M22 = mu2*I;
    }
    
    if (!isochoric) {
      // compute volumic part of energy
      double tr = trace(epsDot);
      Wv += 0.5*lambda*tr*tr;
      if (first) {
        static SYM_TENSOR delta = SYM_TENSOR::identity();
        sig2 += (lambda*tr)*delta;
      }
      if (second) {
        static SYM_TENSOR delta = SYM_TENSOR::identity();
        static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
        M22 += (3*lambda)*K;
      }
    }
    
    return Wv;
  }
  
  // compute dissipated energy
  double dissipatedThMEnergy(const MaterialProperties& material,const ParameterSet& extPar,
                             const SYM_TENSOR& gam,const SYM_TENSOR& gamDot,double Th,
                             SYM_TENSOR& sig1,SYM_TENSOR& sig2,double& N,
                             SYM_TENSOR4& M11,SYM_TENSOR4& M22,SYM_TENSOR4& M12,
                             SYM_TENSOR& dSig1,SYM_TENSOR& dSig2,double& C,double dTime,
                             bool first,bool second) {

    // temperature
    double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    double T = T0+Th;

    // get viscous constants
    char str1[64],str2[64];
    double E,nu;
    double dE,dnu;
    try { // get viscous Young's modulus
      std::sprintf(str1,"VISCOUS_YOUNG_MODULUS_%u",rank);
      std::sprintf(str2,"VISCOUS_YOUNG_MODULUS_EVOLUTION_%u",rank);
      Function& fctE = material.getFunctionProperty(str2);
      E = fctE.value(T,dE);
    }
    catch (NoSuchPropertyException) {
      E = material.getDoubleProperty(str1);
      dE = 0.0e0;
    }
    if (E < 0.e0) throw InvalidPropertyException(str1);

    try { // get viscous Poisson's coefficient
      std::sprintf(str1,"VISCOUS_POISSON_COEFFICIENT_%u",rank);
      std::sprintf(str2,"VISCOUS_POISSON_COEFFICIENT_EVOLUTION_%u",rank);
      Function& fctN = material.getFunctionProperty(str2);
      nu = fctN.value(T,dnu);
    }
    catch (NoSuchPropertyException) {
      nu = material.getDoubleProperty(str1);
      dnu = 0.0e0;
    }
    if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException(str1);

    // compute other properties
    double mu = 0.5*E/(1.+nu);
    double lambda = 2*mu*nu/(1.-2*nu);
    double dmu = 0.5*(dE-E*dnu/(1.+nu))/(1.+nu);
    double dlambda = 2*(dmu*nu+mu*dnu/(1.-2*nu))/(1.-2*nu);

    // transform engineering strains
    SYM_TENSOR epsDot = contravariant(gamDot);

    // compute deviatoric part of energy
    double Wv = mu*innerProd2(epsDot,epsDot);
    double mu2 = mu+mu;
    if (first) {
      sig1 = 0.0e0;
      sig2 = mu2*epsDot;
      N = dmu*innerProd2(epsDot,epsDot);
    }
    if (second) {
      static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
      double dmu2 = dmu+dmu;
      M11 = 0.0e0;
      M12 = 0.0e0;
      M22 = mu2*I;
      dSig1 = 0.0e0;
      dSig2 = dmu2*epsDot;
      C = 0.0e0;
    }

    if (!isochoric) {
      // compute volumic part of energy
      double tr = trace(epsDot);
      Wv += 0.5*lambda*tr*tr;
      if (first) {
        static SYM_TENSOR delta = SYM_TENSOR::identity();
        sig2 += (lambda*tr)*delta;
        N +=  0.5*dlambda*tr*tr;
      }
      if (second) {
        static SYM_TENSOR delta = SYM_TENSOR::identity();
        static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
        M22 += (3*lambda)*K;
        dSig2 += (dlambda*tr)*delta;
      }
    }

    return Wv;
  }
};


/**
 * Class for standard (linear) isotropic thermo-viscoelastic model (Maxwell branch).
 */
template <class ALG>
class IsotropicMaxwellThermoViscoElasticity
: virtual public StdMaxwellThermoViscoElasticity<ALG> {

 public:

  // constructor
  IsotropicMaxwellThermoViscoElasticity(unsigned int r,bool i = false)
  : StdMaxwellThermoViscoElasticity<ALG>(*(new IsotropicThermoElasticMultiPotential<ALG>(r,i)),
                                         *(new IsotropicThermoViscousMultiPotential<ALG>(r,i)),
                                         i) {}

  // copy constructor
  IsotropicMaxwellThermoViscoElasticity(const IsotropicMaxwellThermoViscoElasticity& src)
  : StdMaxwellThermoViscoElasticity<ALG>(src) {}
  
  // destructor
  virtual ~IsotropicMaxwellThermoViscoElasticity() {}
};


/**
 * Implementations of the model : Maxwell.
 */
class IsotropicMaxwellThermoViscoElasticity3D : public ThermoViscoElasticity<TensorAlgebra3D> {

 public:

  // constructor
  IsotropicMaxwellThermoViscoElasticity3D()
  : ThermoElasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra3D>()) {}

  // copy constructor
  IsotropicMaxwellThermoViscoElasticity3D(const IsotropicMaxwellThermoViscoElasticity3D& src)
  : ThermoElasticity<TensorAlgebra3D>(src), ThermoViscoElasticity<TensorAlgebra3D>(src) {}

  // destructor
  virtual ~IsotropicMaxwellThermoViscoElasticity3D() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {

    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellThermoViscoElasticity<TensorAlgebra3D>(i+1)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellThermoViscoElasticity<TensorAlgebra3D>(1)));
    }

    // check properties
    ThermoViscoElasticity<TensorAlgebra3D>::checkProperties(material,os);
  }
};
class IsotropicMaxwellThermoViscoElasticity2D : public ThermoViscoElasticity<TensorAlgebra2D> {

 public:

  // constructor
  IsotropicMaxwellThermoViscoElasticity2D()
  : ThermoElasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra2D>()) {}

  // copy constructor
  IsotropicMaxwellThermoViscoElasticity2D(const IsotropicMaxwellThermoViscoElasticity2D& src)
  : ThermoElasticity<TensorAlgebra2D>(src), ThermoViscoElasticity<TensorAlgebra2D>(src) {}

  // destructor
  virtual ~IsotropicMaxwellThermoViscoElasticity2D() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {

    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellThermoViscoElasticity<TensorAlgebra2D>(i+1)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellThermoViscoElasticity<TensorAlgebra2D>(1)));
    }

    // check properties
    ThermoViscoElasticity<TensorAlgebra2D>::checkProperties(material,os);
  }
};
class IsotropicMaxwellThermoViscoElasticity1D : public ThermoViscoElasticity<TensorAlgebra1D> {

 public:

  // constructor
  IsotropicMaxwellThermoViscoElasticity1D()
  : ThermoElasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra1D>()) {}

  // copy constructor
  IsotropicMaxwellThermoViscoElasticity1D(const IsotropicMaxwellThermoViscoElasticity1D& src)
  : ThermoElasticity<TensorAlgebra1D>(src), ThermoViscoElasticity<TensorAlgebra1D>(src) {}

  // destructor
  virtual ~IsotropicMaxwellThermoViscoElasticity1D() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {

    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellThermoViscoElasticity<TensorAlgebra1D>(i+1)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellThermoViscoElasticity<TensorAlgebra1D>(1)));
    }

    // check properties
    ThermoViscoElasticity<TensorAlgebra1D>::checkProperties(material,os);
  }
};

/**
 * The associated model builder
 */
class IsotropicMaxwellThermoViscoElasticityBuilder : public ModelBuilder {

 private:

  // constructor
  IsotropicMaxwellThermoViscoElasticityBuilder();

  // the instance
  static IsotropicMaxwellThermoViscoElasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~IsotropicMaxwellThermoViscoElasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementations of the model : general viscoelasticity model (Kelvin+Maxwell).
 */
class IsotropicThermoViscoElasticity3D : public ThermoViscoElasticity<TensorAlgebra3D> {

 public:

  // constructor
  IsotropicThermoViscoElasticity3D()
  : ThermoElasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra3D>()),
    ThermoViscoElasticity<TensorAlgebra3D>(new IsotropicThermalViscousPotential<TensorAlgebra3D>()) {}

  // copy constructor
  IsotropicThermoViscoElasticity3D(const IsotropicThermoViscoElasticity3D& src)
  : ThermoElasticity<TensorAlgebra3D>(src), ThermoViscoElasticity<TensorAlgebra3D>(src) {}

  // destructor
  virtual ~IsotropicThermoViscoElasticity3D() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException) {

    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellThermoViscoElasticity<TensorAlgebra3D>(i+1)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellThermoViscoElasticity<TensorAlgebra3D>(1)));
    }

    // check properties
    ThermoViscoElasticity<TensorAlgebra3D>::checkProperties(material,os);
  }
};
class IsotropicThermoViscoElasticity2D : public ThermoViscoElasticity<TensorAlgebra2D> {

 public:

  // constructor
  IsotropicThermoViscoElasticity2D()
  : ThermoElasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra2D>()),
    ThermoViscoElasticity<TensorAlgebra2D>(new IsotropicThermalViscousPotential<TensorAlgebra2D>()) {}

  // copy constructor
  IsotropicThermoViscoElasticity2D(const IsotropicThermoViscoElasticity2D& src)
  : ThermoElasticity<TensorAlgebra2D>(src), ThermoViscoElasticity<TensorAlgebra2D>(src) {}

  // destructor
  virtual ~IsotropicThermoViscoElasticity2D() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException) {

    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellThermoViscoElasticity<TensorAlgebra2D>(i+1)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellThermoViscoElasticity<TensorAlgebra2D>(1)));
    }

    // check properties
    ThermoViscoElasticity<TensorAlgebra2D>::checkProperties(material,os);
  }
};
class IsotropicThermoViscoElasticity1D : public ThermoViscoElasticity<TensorAlgebra1D> {

 public:

  // constructor
  IsotropicThermoViscoElasticity1D()
  : ThermoElasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra1D>()),
    ThermoViscoElasticity<TensorAlgebra1D>(new IsotropicThermalViscousPotential<TensorAlgebra1D>()) {}

  // copy constructor
  IsotropicThermoViscoElasticity1D(const IsotropicThermoViscoElasticity1D& src)
  : ThermoElasticity<TensorAlgebra1D>(src), ThermoViscoElasticity<TensorAlgebra1D>(src) {}

  // destructor
  virtual ~IsotropicThermoViscoElasticity1D() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException) {

    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellThermoViscoElasticity<TensorAlgebra1D>(i+1)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellThermoViscoElasticity<TensorAlgebra1D>(1)));
    }

    // check properties
    ThermoViscoElasticity<TensorAlgebra1D>::checkProperties(material,os);
  }
};

/**
 * The associated model builder
 */
class IsotropicThermoViscoElasticityBuilder : public ModelBuilder {

 private:

  // constructor
  IsotropicThermoViscoElasticityBuilder();

  // the instance
  static IsotropicThermoViscoElasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~IsotropicThermoViscoElasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};


#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
