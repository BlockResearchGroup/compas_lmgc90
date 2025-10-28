/*
 *  $Id: ThermalHardeningModels.cpp 147 2014-08-01 14:46:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "ThermalHardeningModels.h"

// std C library
#include <cmath>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class ThermalLinearIsotropicHardeningModel.
 */

// check consistency of material properties
void ThermalLinearIsotropicHardeningModel::checkProperties(MaterialProperties& material,
                                                           std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Linear isotropic hardening model (temperature-dependent)***" << std::endl;
   
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
   
  /*
   * stored part
   */
  double sig0,H0;
  try {
    // initial yield stress
    try {
      Function& fct = material.getFunctionProperty("INITIAL_YIELD_STRESS_STORED_EVOLUTION");
      sig0 = fct.value(T0);
      material.setProperty("INITIAL_YIELD_STRESS_STORED",sig0);
      if (os) {
        (*os) << "\n\tinitial yield stress (stored part) temperature dependence:";
        (*os) << "\n\t" << fct << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
    }
    
    // hardening modulus
    try {
      Function& fct = material.getFunctionProperty("HARDENING_MODULUS_STORED_EVOLUTION");
      H0 = fct.value(T0);
      material.setProperty("HARDENING_MODULUS_STORED",H0);
      if (os) {
        (*os) << "\n\thardening modulus (stored part) temperature dependence:";
        (*os) << "\n\t" << fct << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      try {
        H0 = material.getDoubleProperty("HARDENING_MODULUS_STORED");
      }
      catch (NoSuchPropertyException) {
        H0 = 0.0e0;
        material.setProperty("HARDENING_MODULUS_STORED",H0);
      }
    }
  }
  catch (NoSuchPropertyException) {
    // initial yield stress
    try {
      sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS");
    }
    catch (NoSuchPropertyException) {
      try {
        sig0 = material.getDoubleProperty("YIELD_IN_TENSION");
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: initial yield stress is not defined." << std::endl;
        throw e;
      }
    }
    material.setProperty("INITIAL_YIELD_STRESS_STORED",sig0);
    
    // hardening modulus
    try {
      H0 = material.getDoubleProperty("HARDENING_MODULUS");
    }
    catch (NoSuchPropertyException) {
      H0 = 0.0e0;
      material.setProperty("HARDENING_MODULUS",H0);
    }
    material.setProperty("HARDENING_MODULUS_STORED",H0);
  }

  /*
   * dissipated part
   */
  double sig1,H1;
  // initial yield stress
  try {
    try {
      Function& fct = material.getFunctionProperty("INITIAL_YIELD_STRESS_DISSIPATED_EVOLUTION");
      sig1 = fct.value(T0);
      material.setProperty("INITIAL_YIELD_STRESS_DISSIPATED",sig1);
      if (os) {
        (*os) << "\n\tinitial yield stress (dissipated part) temperature dependence:";
        (*os) << "\n\t" << fct << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      sig1 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
    }
  }
  catch (NoSuchPropertyException) {
    sig1 = 0.0e0;
    material.setProperty("INITIAL_YIELD_STRESS_DISSIPATED",sig1);
  }

  // hardening modulus
  try {
    try {
      Function& fct = material.getFunctionProperty("HARDENING_MODULUS_DISSIPATED_EVOLUTION");
      H1 = fct.value(T0);
      material.setProperty("HARDENING_MODULUS_DISSIPATED",H1);
      if (os) {
        (*os) << "\n\thardening modulus (dissipated part) temperature dependence:";
        (*os) << "\n\t" << fct << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      H1 = material.getDoubleProperty("HARDENING_MODULUS_DISSIPATED");
    }
  }
  catch (NoSuchPropertyException) {
    H1 = 0.0e0;
    material.setProperty("HARDENING_MODULUS_DISSIPATED",H1);
  }
    
  /*
   * check values
   */
  double sig = sig0+sig1;
  if (sig < 0.0e0) {
    if (os) (*os) << "ERROR: initial yield stress must be positive." << std::endl;
    throw InvalidPropertyException("initial yield stress");
  }
  double H = H0+H1;
  double mu = material.getDoubleProperty("SHEAR_MODULUS");
  if (H < -3*mu) {
    if (os) (*os) << "ERROR: hardening modulus must be larger than -3 times the shear modulus." << std::endl;
    throw InvalidPropertyException("hardening modulus");
  }
  
  /*
   * print-out
   */
  if (os) {
    (*os) << "\n\tAt reference temperature (T = " << T0 << "):" << std::endl;
    (*os) << "\tinitial yield stress (stored part)     = " << sig0   << std::endl;
    (*os) << "\thardening modulus (stored part)        = " <<   H0   << std::endl;
    (*os) << "\tinitial yield stress (dissipated part) = " << sig1   << std::endl;
    (*os) << "\thardening modulus (dissipated part)    = " <<   H1   << std::endl;
  }
}

// update properties in function of external parameters
void ThermalLinearIsotropicHardeningModel::updateProperties(MaterialProperties& material,
                                                            const ParameterSet& extPar) {
  if (!extPar.count("TEMPERATURE")) return;
  double T = extPar.find("TEMPERATURE")->second;
  
  /*
   * stored part
   */
  double sig0,H0;
  // initial yield stress
  try {
    Function& fct = material.getFunctionProperty("INITIAL_YIELD_STRESS_STORED_EVOLUTION");
    sig0 = fct.value(T);
    material.setProperty("INITIAL_YIELD_STRESS_STORED",sig0);
  }
  catch (NoSuchPropertyException) {
    sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
  }
    
  // hardening modulus
  try {
    Function& fct = material.getFunctionProperty("HARDENING_MODULUS_STORED_EVOLUTION");
    H0 = fct.value(T);
    material.setProperty("HARDENING_MODULUS_STORED",H0);
  }
  catch (NoSuchPropertyException) {
    H0 = material.getDoubleProperty("HARDENING_MODULUS_STORED");
  }
  
  /*
   * dissipated part
   */
  double sig1,H1;
  // initial yield stress
  try {
    Function& fct = material.getFunctionProperty("INITIAL_YIELD_STRESS_DISSIPATED_EVOLUTION");
    sig1 = fct.value(T);
    material.setProperty("INITIAL_YIELD_STRESS_DISSIPATED",sig1);
  }
  catch (NoSuchPropertyException) {
    sig1 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  }
  
  // hardening modulus
  try {
    Function& fct = material.getFunctionProperty("HARDENING_MODULUS_DISSIPATED_EVOLUTION");
    H1 = fct.value(T);
    material.setProperty("HARDENING_MODULUS_DISSIPATED",H1);
  }
  catch (NoSuchPropertyException) {
    H1 = material.getDoubleProperty("HARDENING_MODULUS_DISSIPATED");
  }
  
  /*
   * check values
   */
  double sig = sig0+sig1;
  if (sig < 0.e0) throw InvalidPropertyException("initial yield stress");
  double H = H0+H1;
  double mu = material.getDoubleProperty("SHEAR_MODULUS");
  if (H < -3*mu) throw InvalidPropertyException("hardening modulus");
}

// plastically stored energy
double ThermalLinearIsotropicHardeningModel::storedThMEnergy(const MaterialProperties& material,
                                                             const ParameterSet& extPar,
                                                             const MatLibArray& intPar0,
                                                             MatLibArray& intPar1,double Wp0,
                                                             double epsPl0,double epsPl1,
                                                             double Th,double& sig,double& N,
                                                             double& h,double& dSig,double& C,
                                                             bool first,bool second) {
  // temperature
  double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
  double T = T0+Th;

  // initial yield stress
  double sig0,dSig0;
  try {
    Function& fct = material.getFunctionProperty("INITIAL_YIELD_STRESS_STORED_EVOLUTION");
    sig0 = fct.value(T,dSig0);
  }
  catch (NoSuchPropertyException) {
    sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
    dSig0 = 0.0e0;
  }
  
  // hardening modulus
  double H,dH;
  try {
    Function& fct = material.getFunctionProperty("HARDENING_MODULUS_STORED_EVOLUTION");
    H = fct.value(T,dH);
  }
  catch (NoSuchPropertyException) {
    H = material.getDoubleProperty("HARDENING_MODULUS_STORED");
    dH = 0.0e0;
  }
  
  // compute plastic potential
  double Wp;
  if (first) {
    double sig1 = H*epsPl1;
    sig = sig0+sig1;
    N = (dSig0+0.5*dH*epsPl1)*epsPl1;
    Wp = (sig0+0.5*sig1)*epsPl1;
  }
  else
    Wp = (sig0+0.5*H*epsPl1)*epsPl1;
  
  if (second) {
    h = H;
    dSig = dSig0+dH*epsPl1;
    C = 0.0e0;
  }

  return Wp;  
}

// yield stress
double ThermalLinearIsotropicHardeningModel::yieldThMStress(const MaterialProperties& material,
                                                            const ParameterSet& extPar,
                                                            const MatLibArray&intPar0,
                                                            MatLibArray& intPar1,
                                                            double epsPl,double Th,double& h,
                                                            double& dSig,bool second) {
  // temperature
  double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
  double T = T0+Th;
  
  // initial yield stress
  double sig0,dSig0;
  try {
    Function& fct = material.getFunctionProperty("INITIAL_YIELD_STRESS_DISSIPATED_EVOLUTION");
    sig0 = fct.value(T,dSig0);
  }
  catch (NoSuchPropertyException) {
    sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
    dSig0 = 0.0e0;
  }
  
  // hardening modulus
  double H,dH;
  try {
    Function& fct = material.getFunctionProperty("HARDENING_MODULUS_DISSIPATED_EVOLUTION");
    H = fct.value(T,dH);
  }
  catch (NoSuchPropertyException) {
    H = material.getDoubleProperty("HARDENING_MODULUS_DISSIPATED");
    dH = 0.0e0;
  }
  
  // compute yield stress
  double sig = sig0+H*epsPl;
  if (second) {
    h = H;
    dSig = dSig0+dH*epsPl;
  }
  
  return sig;
}


/*
 * Methods for class ThermalNonLinearIsotropicHardeningModel.
 */

// check consistency of material properties
void ThermalNonLinearIsotropicHardeningModel::checkProperties(MaterialProperties& material,
                                                              std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Nonlinear isotropic hardening model (temperature-dependent)***" << std::endl;
      
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
   
  /*
   * stored part - power law
   */
  double sig0,b0,n0=1.0e0;
  // initial yield stress
  try {
    try {
      Function& fct = material.getFunctionProperty("INITIAL_YIELD_STRESS_STORED_EVOLUTION");
      sig0 = fct.value(T0);
      material.setProperty("INITIAL_YIELD_STRESS_STORED",sig0);
      if (os) {
        (*os) << "\n\tinitial yield stress (stored part) temperature dependence:";
        (*os) << "\n\t" << fct << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
    }
  }
  catch (NoSuchPropertyException) {
    try {
      sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS");
    }
    catch (NoSuchPropertyException) {
      try {
        sig0 = material.getDoubleProperty("YIELD_IN_TENSION");
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: initial yield stress is not defined." << std::endl;
        throw e;
      }
    }
    material.setProperty("INITIAL_YIELD_STRESS_STORED",sig0);
  }
  // hardening coefficient
  try {
    try {
      Function& fct = material.getFunctionProperty("HARDENING_COEFFICIENT_STORED_EVOLUTION");
      b0 = fct.value(T0);
      material.setProperty("HARDENING_COEFFICIENT_STORED",b0);
      if (os) {
        (*os) << "\n\thardening coefficient (stored part) temperature dependence:";
        (*os) << "\n\t" << fct << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      b0 = material.getDoubleProperty("HARDENING_COEFFICIENT_STORED");
    }
  }
  catch (NoSuchPropertyException) {
    try {
      b0 = material.getDoubleProperty("HARDENING_COEFFICIENT");
    }
    catch (NoSuchPropertyException) {
      b0 = 0.0e0;
    }
    material.setProperty("HARDENING_COEFFICIENT_STORED",b0);
  }
  // hardening exponent
  if (b0 != 0.0e0) {
    try {
      n0 = material.getDoubleProperty("HARDENING_EXPONENT_STORED");
    }
    catch (NoSuchPropertyException) {
      try {
        n0 = material.getDoubleProperty("HARDENING_EXPONENT");
      }
      catch (NoSuchPropertyException) {
        n0 = 1.0e0;
      }
      material.setProperty("HARDENING_EXPONENT_STORED",n0);
    }
    if (n0 < 1.0e0) {
      if (os) (*os) << "ERROR: hardening exponent (stored part) must be >= 1." << std::endl;
      throw InvalidPropertyException("hardening exponent (stored)");
    }
  }
  
  /*
   * stored part - saturation law
   */
  double dSig0,d0=0.0e0;
  // saturation yield stress
  try {
    try {
      Function& fct = material.getFunctionProperty("SATURATION_YIELD_STRESS_STORED_EVOLUTION");
      dSig0 = fct.value(T0);
      material.setProperty("SATURATION_YIELD_STRESS_STORED",dSig0);
      if (os) {
        (*os) << "\n\tsaturation yield stress (stored part) temperature dependence:";
        (*os) << "\n\t" << fct << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      dSig0 = material.getDoubleProperty("SATURATION_YIELD_STRESS_STORED");
    }
  }
  catch (NoSuchPropertyException) {
    try {
      dSig0 = material.getDoubleProperty("SATURATION_YIELD_STRESS");
    }
    catch (NoSuchPropertyException) {
      dSig0 = 0.0e0;
    }
    material.setProperty("SATURATION_YIELD_STRESS_STORED",dSig0);
  }
  // saturation coefficient
  if (dSig0 != 0.0e0) {
    try {
      try {
        try {
          Function& fct = material.getFunctionProperty("HARDENING_SATURATION_COEFFICIENT_STORED_EVOLUTION");
          d0 = fct.value(T0);
          material.setProperty("HARDENING_SATURATION_COEFFICIENT_STORED",d0);
          if (os) {
            (*os) << "\n\thardening saturation coefficient (stored part) temperature dependence:";
            (*os) << "\n\t" << fct << std::endl;
          }
        }
        catch (NoSuchPropertyException) {
          d0 = material.getDoubleProperty("HARDENING_SATURATION_COEFFICIENT_STORED");
        }
      }
      catch (NoSuchPropertyException) {
        d0 = material.getDoubleProperty("HARDENING_SATURATION_COEFFICIENT");
        material.setProperty("HARDENING_SATURATION_COEFFICIENT_STORED",d0);
      }
      if (d0 <= 0.0e0) {
        if (os) (*os) << "ERROR: hardening saturation coefficient (stored part) must be strictly positive." << std::endl;
        throw InvalidPropertyException("hardening saturation coefficient (stored)");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: hardening saturation coefficient (stored part) is not defined." << std::endl;
      throw e;
    }
  }
  
  /*
   * dissipated part - power law
   */
  double sig1,b1,n1=1.0e0;
  // initial yield stress
  try {
    try {
      Function& fct = material.getFunctionProperty("INITIAL_YIELD_STRESS_DISSIPATED_EVOLUTION");
      sig1 = fct.value(T0);
      material.setProperty("INITIAL_YIELD_STRESS_DISSIPATED",sig1);
      if (os) {
        (*os) << "\n\tinitial yield stress (dissipated part) temperature dependence:";
        (*os) << "\n\t" << fct << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      sig1 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
    }
  }
  catch (NoSuchPropertyException) {
    sig1 = 0.0e0;
    material.setProperty("INITIAL_YIELD_STRESS_DISSIPATED",sig1);
  }
  // hardening coefficient
  try {
    try {
      Function& fct = material.getFunctionProperty("HARDENING_COEFFICIENT_DISSIPATED_EVOLUTION");
      b1 = fct.value(T0);
      material.setProperty("HARDENING_COEFFICIENT_DISSIPATED",b1);
      if (os) {
        (*os) << "\n\thardening coefficient (dissipated part) temperature dependence:";
        (*os) << "\n\t" << fct << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      b1 = material.getDoubleProperty("HARDENING_COEFFICIENT_DISSIPATED");
    }
  }
  catch (NoSuchPropertyException) {
    b1 = 0.0e0;
    material.setProperty("HARDENING_COEFFICIENT_DISSIPATED",b1);
  }
  // hardening exponent
  if (b1 != 0.0e0) {
    try {
      n1 = material.getDoubleProperty("HARDENING_EXPONENT_DISSIPATED");
    }
    catch (NoSuchPropertyException) {
      n1 = 1.0e0;
      material.setProperty("HARDENING_EXPONENT_DISSIPATED",n1);
    }
    if (n1 < 1.0e0) {
      if (os) (*os) << "ERROR: hardening exponent (dissipated part) must be >= 1." << std::endl;
      throw InvalidPropertyException("hardening exponent (dissipated)");
    }
  }
  
  /*
   * dissipated part - saturation law
   */
  double dSig1,d1=0.0e0;
  // saturation yield stress
  try {
    try {
      Function& fct = material.getFunctionProperty("SATURATION_YIELD_STRESS_DISSIPATED_EVOLUTION");
      dSig1 = fct.value(T0);
      material.setProperty("SATURATION_YIELD_STRESS_DISSIPATED",dSig1);
      if (os) {
        (*os) << "\n\tsaturation yield stress (dissipated part) temperature dependence:";
        (*os) << "\n\t" << fct << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      dSig1 = material.getDoubleProperty("SATURATION_YIELD_STRESS_DISSIPATED");
    }
  }
  catch (NoSuchPropertyException) {
    dSig1 = 0.0e0;
    material.setProperty("SATURATION_YIELD_STRESS_DISSIPATED",dSig1);
  }
  // saturation coefficient
  if (dSig1 != 0.0e0) {
    try {
      try {
        Function& fct = material.getFunctionProperty("HARDENING_SATURATION_COEFFICIENT_DISSIPATED_EVOLUTION");
        d1 = fct.value(T0);
        material.setProperty("HARDENING_SATURATION_COEFFICIENT_DISSIPATED",d1);
        if (os) {
          (*os) << "\n\thardening saturation coefficient (dissipated part) temperature dependence:";
          (*os) << "\n\t" << fct << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        d1 = material.getDoubleProperty("HARDENING_SATURATION_COEFFICIENT_DISSIPATED");
      }
      if (d1 <= 0.0e0) {
        if (os) (*os) << "ERROR: hardening saturation coefficient (dissipated part) must be strictly positive." << std::endl;
        throw InvalidPropertyException("hardening saturation coefficient (dissipated)");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: hardening saturation coefficient (dissipated part) is not defined." << std::endl;
      throw e;
    }
  }
  
  /*
   * check values
   */
  double sig = sig0+sig1;
  if (sig < 0.e0) {
    if (os) (*os) << "ERROR: initial yield stress must be positive." << std::endl;
    throw InvalidPropertyException("initial yield stress");
  }
  double dSig = dSig0+dSig1;
  if (dSig < -sig) {
    if (os) (*os) << "ERROR: saturation yield stress cannot lead to negative yield stress." << std::endl;
    throw InvalidPropertyException("saturation yield stress");
  }
  
  /*
   * print-out
   */
  if (os) {
    (*os) << "\n\tAt reference temperature (T = " << T0 << "):" << std::endl;
    (*os) << "\tinitial yield stress (stored part)                 = " <<  sig0 << std::endl;
    (*os) << "\thardening coefficient (stored part)                = " <<    b0 << std::endl;
    (*os) << "\thardening exponent (stored part)                   = " <<    n0 << std::endl;
    (*os) << "\tsaturation yield stress (stored part)              = " << dSig0 << std::endl;
    (*os) << "\thardening saturation coefficient (stored part)     = " <<    d0 << std::endl;
    (*os) << "\tinitial yield stress (dissipated part)             = " <<  sig1 << std::endl;
    (*os) << "\thardening coefficient (dissipated part)            = " <<    b1 << std::endl;
    (*os) << "\thardening exponent (dissipated part)               = " <<    n1 << std::endl;
    (*os) << "\tsaturation yield stress (dissipated part)          = " << dSig1 << std::endl;
    (*os) << "\thardening saturation coefficient (dissipated part) = " <<    d1 << std::endl;
  }
}

// update properties in function of external parameters
void ThermalNonLinearIsotropicHardeningModel::updateProperties(MaterialProperties& material,
                                                               const ParameterSet& extPar) {
  if (!extPar.count("TEMPERATURE")) return;
  double T = extPar.find("TEMPERATURE")->second;

  /*
   * stored part - power law
   */
  double sig0,b0;
  // initial yield stress
  try {
    Function& fct = material.getFunctionProperty("INITIAL_YIELD_STRESS_STORED_EVOLUTION");
    sig0 = fct.value(T);
    material.setProperty("INITIAL_YIELD_STRESS_STORED",sig0);
  }
  catch (NoSuchPropertyException) {
    sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
  }
  // hardening coefficient
  try {
    Function& fct = material.getFunctionProperty("HARDENING_COEFFICIENT_STORED_EVOLUTION");
    b0 = fct.value(T);
    material.setProperty("HARDENING_COEFFICIENT_STORED",b0);
  }
  catch (NoSuchPropertyException) {
    b0 = material.getDoubleProperty("HARDENING_COEFFICIENT_STORED");
  }
  
  /*
   * stored part - saturation law
   */
  double dSig0,d0;
  // saturation yield stress
  try {
    Function& fct = material.getFunctionProperty("SATURATION_YIELD_STRESS_STORED_EVOLUTION");
    dSig0 = fct.value(T);
    material.setProperty("SATURATION_YIELD_STRESS_STORED",dSig0);
  }
  catch (NoSuchPropertyException) {
    dSig0 = material.getDoubleProperty("SATURATION_YIELD_STRESS_STORED");
  }
  // saturation coefficient
  if (dSig0 != 0.0e0) {
    try {
      Function& fct = material.getFunctionProperty("HARDENING_SATURATION_COEFFICIENT_STORED_EVOLUTION");
      d0 = fct.value(T);
      material.setProperty("HARDENING_SATURATION_COEFFICIENT_STORED",d0);
    }
    catch (NoSuchPropertyException) {
      d0 = material.getDoubleProperty("HARDENING_SATURATION_COEFFICIENT_STORED");
    }
    if (d0 <= 0.0e0) {
      throw InvalidPropertyException("hardening saturation coefficient (stored)");
    }
  }
  
  /*
   * dissipated part - power law
   */
  double sig1,b1;
  // initial yield stress
  try {
    Function& fct = material.getFunctionProperty("INITIAL_YIELD_STRESS_DISSIPATED_EVOLUTION");
    sig1 = fct.value(T);
    material.setProperty("INITIAL_YIELD_STRESS_DISSIPATED",sig1);
  }
  catch (NoSuchPropertyException) {
    sig1 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  }
  // hardening coefficient
  try {
    Function& fct = material.getFunctionProperty("HARDENING_COEFFICIENT_DISSIPATED_EVOLUTION");
    b1 = fct.value(T);
    material.setProperty("HARDENING_COEFFICIENT_DISSIPATED",b1);
  }
  catch (NoSuchPropertyException) {
    b1 = material.getDoubleProperty("HARDENING_COEFFICIENT_DISSIPATED");
  }
  
  /*
   * dissipated part - saturation law
   */
  double dSig1,d1;
  // saturation yield stress
  try {
    Function& fct = material.getFunctionProperty("SATURATION_YIELD_STRESS_DISSIPATED_EVOLUTION");
    dSig1 = fct.value(T);
    material.setProperty("SATURATION_YIELD_STRESS_DISSIPATED",dSig1);
  }
  catch (NoSuchPropertyException) {
    dSig1 = material.getDoubleProperty("SATURATION_YIELD_STRESS_DISSIPATED");
  }
  // saturation coefficient
  if (dSig1 != 0.0e0) {
    try {
      Function& fct = material.getFunctionProperty("HARDENING_SATURATION_COEFFICIENT_DISSIPATED_EVOLUTION");
      d1 = fct.value(T);
      material.setProperty("HARDENING_SATURATION_COEFFICIENT_DISSIPATED",d1);
    }
    catch (NoSuchPropertyException) {
      d1 = material.getDoubleProperty("HARDENING_SATURATION_COEFFICIENT_DISSIPATED");
    }
    if (d1 <= 0.0e0) {
      throw InvalidPropertyException("hardening saturation coefficient (dissipated)");
    }
  }
  
  /*
   * check values
   */
  double sig = sig0+sig1;
  if (sig < 0.e0) {
    throw InvalidPropertyException("initial yield stress");
  }
  double dSig = dSig0+dSig1;
  if (dSig < -sig) {
    throw InvalidPropertyException("saturation yield stress");
  }
}
  
// plastically stored energy
double ThermalNonLinearIsotropicHardeningModel::storedThMEnergy(const MaterialProperties& material,
                                                                const ParameterSet& extPar,
                                                                const MatLibArray& intPar0,
                                                                MatLibArray& intPar1,double,
                                                                double epsPl0,double epsPl1,
                                                                double Th,double& sig,double& N,
                                                                double& h,double& dSig,double& C,
                                                                bool first,bool second) {
  static const double PRECISION = 1.0e-16;

  // temperature
  double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
  double T = T0+Th;
  
  // power-law part
  double Wp,sig0,dSig0;
  try {
    Function& fct = material.getFunctionProperty("INITIAL_YIELD_STRESS_STORED_EVOLUTION");
    sig0 = fct.value(T,dSig0);
  }
  catch (NoSuchPropertyException) {
    sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
    dSig0 = 0.0e0;
  }
  double b,db;
  try {
    Function& fct = material.getFunctionProperty("HARDENING_COEFFICIENT_STORED_EVOLUTION");
    b = fct.value(T,db);
  }
  catch (NoSuchPropertyException) {
    b = material.getDoubleProperty("HARDENING_COEFFICIENT_STORED");
    db = 0.0e0;
  }
  if (b != 0.0e0) {
    double n = material.getDoubleProperty("HARDENING_EXPONENT_STORED");
    double val = 1.0e0+b*epsPl1;
    if (val < PRECISION) val = 0.0e0;
    double expo = 1.0e0/n;
    if (first) {
      double val1 = std::pow(val,expo);
      double val2 = val*val1-1.0e0;
      sig = sig0*val1;
      Wp = sig0*val2/(b*(expo+1));
      N = (dSig0-sig0*db/b)/(b*(expo+1))*val2+sig0*epsPl1*db/b*val1;
    }
    else {
      double val1 = std::pow(val,expo+1)-1.0e0;
      Wp = sig0*val1/(b*(expo+1));
    }
    if (second) {
      double val1;
      if (val > PRECISION)
	      val1 = std::pow(val,expo-1);
      else
	      val1 = 0.0e0;
      double val2 = val*val1;
      double val3 = val*val2-1.0e0;
      h = expo*b*sig0*val1;
      dSig = (dSig0*val+sig0*db*expo*epsPl1)*val1;
      double coef1 = sig0*db/b-dSig0;
      double coef2 = db*epsPl1;
      C = (2*coef1*db/b/(expo+1)*val3 - 2*coef1*coef2*val2 + sig0*coef2*coef2*val1)/b;
    }
  }
  else {
    Wp = sig0*epsPl1;
    if (first) {
      sig = sig0;
      N = dSig0*epsPl1;
    }
    if (second) {
      h = 0.0e0;
      dSig = dSig0;
      C = 0.0e0;
    }
  }

  // saturation part
  double dSig1,d2Sig1;
  try {
    Function& fct = material.getFunctionProperty("SATURATION_YIELD_STRESS_STORED_EVOLUTION");
    dSig1 = fct.value(T,d2Sig1);
  }
  catch (NoSuchPropertyException) {
    dSig1 = material.getDoubleProperty("SATURATION_YIELD_STRESS_STORED");
    d2Sig1 = 0.0e0;
  }
  if (dSig1 != 0.0e0) {
    double d,dd;
    try {
      Function& fct = material.getFunctionProperty("HARDENING_SATURATION_COEFFICIENT_STORED_EVOLUTION");
      d = fct.value(T,dd);
    }
    catch (NoSuchPropertyException) {
      d = material.getDoubleProperty("HARDENING_SATURATION_COEFFICIENT_STORED");
      dd = 0.0e0;
    }
    double val = std::exp(-d*epsPl1);
    double val1 = val-1.0e0;
    if (first) {
      sig += dSig1*(1.-val);
      N += d2Sig1*(epsPl1+val1/d)-dSig1*dd*(val1+d*epsPl1*val)/(d*d);
    }
    if (second) {
      h += dSig1*d*val;
      dSig += d2Sig1*(1.-val)+dSig1*dd*epsPl1*val;
      double coef1 = d2Sig1-dSig1*dd/d;
      C += (-2*coef1*val1-(2*coef1-dSig1*epsPl1*dd)*epsPl1*val)*dd/d;
    }
    Wp += dSig1*(epsPl1+val1/d);
  }
  
  return Wp;
}

// yield stress
double ThermalNonLinearIsotropicHardeningModel::yieldThMStress(const MaterialProperties& material,
                                                               const ParameterSet& extPar,
                                                               const MatLibArray&intPar0,
                                                               MatLibArray& intPar1,
                                                               double epsPl,double Th,double& h,
                                                               double& dSig,bool second) {
  static const double PRECISION = 1.0e-16;

  // temperature
  double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
  double T = T0+Th;

  // initialize
  double sig = 0.0e0;

  // power-law part
  double sig0,dSig0;
  try {
    Function& fct = material.getFunctionProperty("INITIAL_YIELD_STRESS_DISSIPATED_EVOLUTION");
    sig0 = fct.value(T,dSig0);
  }
  catch (NoSuchPropertyException) {
    sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
    dSig0 = 0.0e0;
  }
  double b,db;
  try {
    Function& fct = material.getFunctionProperty("HARDENING_COEFFICIENT_DISSIPATED_EVOLUTION");
    b = fct.value(T,db);
  }
  catch (NoSuchPropertyException) {
    b = material.getDoubleProperty("HARDENING_COEFFICIENT_DISSIPATED");
    db = 0.0e0;
  }
  if (b != 0.0e0) {
    double n = material.getDoubleProperty("HARDENING_EXPONENT_DISSIPATED");
    double val = 1.0e0+b*epsPl;
    if (val < PRECISION) val = 0.0e0;
    double expo = 1.0e0/n;
    sig = sig0*std::pow(val,expo);
    if (second) {
      double val1;
      if (val > PRECISION)
	      val1 = std::pow(val,expo-1);
      else
	      val1 = 0.0e0;
      h = expo*b*sig0*val1;
      dSig = (dSig0*val+sig0*db*expo*epsPl)*val1;
    }
  }
  else {
    sig = sig0;
    if (second) {
      h = 0.0e0;
      dSig = dSig0;
    }
  }
  
  // saturation part
  double dSig1,d2Sig1;
  try {
    Function& fct = material.getFunctionProperty("SATURATION_YIELD_STRESS_DISSIPATED_EVOLUTION");
    dSig1 = fct.value(T,d2Sig1);
  }
  catch (NoSuchPropertyException) {
    dSig1 = material.getDoubleProperty("SATURATION_YIELD_STRESS_DISSIPATED");
    d2Sig1 = 0.0e0;
  }
  if (dSig1 != 0.0e0) {
    double d,dd;
    try {
      Function& fct = material.getFunctionProperty("HARDENING_SATURATION_COEFFICIENT_DISSIPATED_EVOLUTION");
      d = fct.value(T,dd);
    }
    catch (NoSuchPropertyException) {
      d = material.getDoubleProperty("HARDENING_SATURATION_COEFFICIENT_DISSIPATED");
      dd = 0.0e0;
    }
    double val = std::exp(-d*epsPl);
    sig += dSig1*(1.-val);
    if (second) {
      h += dSig1*d*val;
      dSig += d2Sig1*(1.-val)+dSig1*dd*epsPl*val; 
    }
  }
  
  return sig;
}

