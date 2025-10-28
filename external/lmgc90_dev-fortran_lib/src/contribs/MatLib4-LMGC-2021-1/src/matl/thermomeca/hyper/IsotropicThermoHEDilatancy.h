/*
 *  $Id: IsotropicThermoHEDilatancy.h 142 2014-02-07 12:51:54Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_HYPER_ISOTROPIC_DILATANCY_H
#define ZORGLIB_MATL_MECA_THERMO_HYPER_ISOTROPIC_DILATANCY_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/hyper/IsotropicStdThermalDilatancy.h>
#include <matl/thermomeca/hyper/ThermoHyperElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing isotropic thermohyperelastic dilatancy models.
 */
template <class ALG>
class IsotropicThermoHyperElasticDilatancy 
: virtual public ThermoHyperElasticity<ALG>::Dilatancy,
  virtual public IsotropicStdThermalDilatancy<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  IsotropicThermoHyperElasticDilatancy() {}
  
  // copy constructor
  IsotropicThermoHyperElasticDilatancy(const IsotropicThermoHyperElasticDilatancy&) {}
  
  // destructor
  virtual ~IsotropicThermoHyperElasticDilatancy() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException) {
      if (os) (*os) << "\n\t***Isotropic thermohyperelastic dilatancy***" << std::endl;
      
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
      
      // get initial temperature
      try {
        T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: initial temperature is not defined." << std::endl;
        throw e;
      }
      
      // get bulk modulus
      try {
        try {
          Function& fctK = material.getFunctionProperty("BULK_MODULUS_EVOLUTION");
          K = fctK.value(T0);
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
                           const SYM_TENSOR& C,double T,SYM_TENSOR& S,double& N,
                           SYM_TENSOR4& M,SYM_TENSOR& dS,double& Cm,
                           bool first,bool second) {
    
    static const double ONE_THIRD = 1.e0/3.e0;
    
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
    double val = -3*alpha;
    double Th = T-T0;
    double coef = val*K*Th;
    double W = coef*logJ;
    if (first) {
      S = coef*Cinv;
      N = val*(K+dK*Th)*logJ;
    }
    if (second) {
      M = 0.0e0;
      M.addIJKL(-coef,Cinv);
      dS = (val*(K+dK*Th))*Cinv;
      Cm = 2*val*dK*logJ;
    }
    
    return W;
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
