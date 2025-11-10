/*
 *  $Id: HardeningModels.cpp 147 2014-08-01 14:46:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "HardeningModels.h"

// std C library
#include <cmath>
// std C++ library
#include <limits>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class LinearIsotropicHardeningModel.
 */

// check consistency of material properties
void LinearIsotropicHardeningModel::checkProperties(MaterialProperties& material,
                                                    std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Linear isotropic hardening model***" << std::endl;
  
  /*
   * stored part
   */
  double sig0,H0;
  try {
    // initial yield stress
    sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
    
    // hardening modulus
    try {
      H0 = material.getDoubleProperty("HARDENING_MODULUS_STORED");
    }
    catch (NoSuchPropertyException) {
      H0 = 0.0e0;
      material.setProperty("HARDENING_MODULUS_STORED",H0);
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
    sig1 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  }
  catch (NoSuchPropertyException) {
    sig1 = 0.0e0;
    material.setProperty("INITIAL_YIELD_STRESS_DISSIPATED",sig1);
  }
  
  // hardening modulus
  try {
    H1 = material.getDoubleProperty("HARDENING_MODULUS_DISSIPATED");
  }
  catch (NoSuchPropertyException) {
    H1 = 0.0e0;
    material.setProperty("HARDENING_MODULUS_DISSIPATED",H1);
  }
    
  /*
   * check values
   */
  double sig = sig0+sig1;
  if (sig < 0.e0) {
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
    (*os) << "\tinitial yield stress (stored part)     = " << sig0   << std::endl;
    (*os) << "\thardening modulus (stored part)        = " <<   H0   << std::endl;
    (*os) << "\tinitial yield stress (dissipated part) = " << sig1   << std::endl;
    (*os) << "\thardening modulus (dissipated part)    = " <<   H1   << std::endl;
  }
}

// plastically stored energy
double LinearIsotropicHardeningModel::storedEnergy(const MaterialProperties& material,
                                                   const ParameterSet& extPar,
                                                   const MatLibArray& intPar0,
                                                   MatLibArray& intPar1,
                                                   double Wp0,double epsPl0,double epsPl1,
                                                   double& sig,double& h,bool first,bool second) {
  // initial yield stress
  double sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
  
  // hardening modulus
  h = material.getDoubleProperty("HARDENING_MODULUS_STORED");
  
  // compute plastic potential
  double Wp;
  if (first) {
    double dSig = h*epsPl1;
    sig = sig0+dSig;
    Wp = (sig0+0.5*dSig)*epsPl1;
  }
  else
    Wp = (sig0+0.5*h*epsPl1)*epsPl1;
  
  return Wp;  
}

// yield stress
double LinearIsotropicHardeningModel::yieldStress(const MaterialProperties& material,
                                                  const ParameterSet& extPar,
                                                  const MatLibArray&intPar0,
                                                  MatLibArray& intPar1,
                                                  double epsPl,double& h,bool second) {
  // initial yield stress
  double sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  
  // hardening modulus
  h = material.getDoubleProperty("HARDENING_MODULUS_DISSIPATED");
  
  // compute yield stress
  double sig = sig0+h*epsPl;
  
  return sig;
}


/*
 * Methods for class NonLinearIsotropicHardeningModel.
 */

// check consistency of material properties
void NonLinearIsotropicHardeningModel::checkProperties(MaterialProperties& material,
                                                       std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Nonlinear isotropic hardening model***" << std::endl;
  
  /*
   * stored part - power law
   */
  double sig0,b0,n0=1.0e0;
  // initial yield stress
  try {
    sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
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
    b0 = material.getDoubleProperty("HARDENING_COEFFICIENT_STORED");
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
	  dSig0 = material.getDoubleProperty("SATURATION_YIELD_STRESS_STORED");
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
        d0 = material.getDoubleProperty("HARDENING_SATURATION_COEFFICIENT_STORED");
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
    sig1 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  }
  catch (NoSuchPropertyException) {
    sig1 = 0.0e0;
    material.setProperty("INITIAL_YIELD_STRESS_DISSIPATED",sig1);
  }
  // hardening coefficient
  try {
    b1 = material.getDoubleProperty("HARDENING_COEFFICIENT_DISSIPATED");
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
    dSig1 = material.getDoubleProperty("SATURATION_YIELD_STRESS_DISSIPATED");
  }
  catch (NoSuchPropertyException) {
    dSig1 = 0.0e0;
    material.setProperty("SATURATION_YIELD_STRESS_DISSIPATED",dSig1);
  }
  // saturation coefficient
  if (dSig1 != 0.0e0) {
    try {
      d1 = material.getDoubleProperty("HARDENING_SATURATION_COEFFICIENT_DISSIPATED");
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

// plastically stored energy
double NonLinearIsotropicHardeningModel::storedEnergy(const MaterialProperties& material,
                                                      const ParameterSet& extPar,
                                                      const MatLibArray& intPar0,
                                                      MatLibArray& intPar1,
                                                      double Wp0,double epsPl0,double epsPl1,
                                                      double& sig,double& h,bool first,bool second) {
  static const double PRECISION = 1.0e-16;

  // initialize
  double Wp = 0.e0;
  sig = 0.0e0;
  h = 0.0e0;

  // power-law part (Swift)
  double sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
  double b = material.getDoubleProperty("HARDENING_COEFFICIENT_STORED");
  if (b != 0.0e0) {
    double n = material.getDoubleProperty("HARDENING_EXPONENT_STORED");
    double val = 1.0e0+b*epsPl1;
    if (val < PRECISION) val = 0.0e0;
    double expo = 1.0e0/n;
    if (first) {
      double val1 = std::pow(val,expo);
      sig = sig0*val1;
      Wp = sig0*(val*val1-1.0e0)/(b*(expo+1));
    }
    else {
      double val1 = std::pow(val,expo+1)-1.0e0;
      Wp = sig0*val1/(b*(expo+1));
    }
    if (second || val > PRECISION) {
      h = expo*b*sig0*std::pow(val,expo-1);
    }
  }
  else {
    if (first) sig = sig0;
    Wp = sig0*epsPl1;
  }
  
  // saturation part (Voce)
  double dSig = material.getDoubleProperty("SATURATION_YIELD_STRESS_STORED");
  if (dSig != 0.0e0) {
    double d = material.getDoubleProperty("HARDENING_SATURATION_COEFFICIENT_STORED");
    double val = std::exp(-d*epsPl1);
    if (first) sig += dSig*(1.-val);
    if (second) h += dSig*d*val;
    Wp += dSig*(epsPl1+(val-1.0e0)/d);
  }
  
  return Wp;
}

// yield stress
double NonLinearIsotropicHardeningModel::yieldStress(const MaterialProperties& material,
                                                     const ParameterSet& extPar,
                                                     const MatLibArray&intPar0,
                                                     MatLibArray& intPar1,
                                                     double epsPl,double& h,bool second) {
  static const double PRECISION = 1.0e-16;

  // initialize
  double sig = 0.0e0;
  h = 0.0e0;

  // power-law part (Swift)
  double sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  double b = material.getDoubleProperty("HARDENING_COEFFICIENT_DISSIPATED");
  if (b != 0.0e0) {
    double n = material.getDoubleProperty("HARDENING_EXPONENT_DISSIPATED");
    double val = 1.0e0+b*epsPl;
    if (val < PRECISION) val = 0.0e0;
    double expo = 1.0e0/n;
    sig = sig0*std::pow(val,expo);
    if (second && val > PRECISION) {
      h = expo*b*sig0*std::pow(val,expo-1);
    }
  }
  else {
    sig = sig0;
  }
  
  // saturation part (Voce)
  double dSig = material.getDoubleProperty("SATURATION_YIELD_STRESS_DISSIPATED");
  if (dSig != 0.0e0) {
    double d = material.getDoubleProperty("HARDENING_SATURATION_COEFFICIENT_DISSIPATED");
    double val = std::exp(-d*epsPl);
    sig += dSig*(1.-val);
    if (second) h += dSig*d*val;
  }
  
  return sig;
}


/*
 * Methods for class LudwikIsotropicHardeningModel.
 */

// check consistency of material properties
void LudwikIsotropicHardeningModel::checkProperties(MaterialProperties& material,
                                                       std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Ludwik isotropic hardening model***" << std::endl;
  
  /*
   * stored part
   */
  double sig0,b0,n0=1.0e0;
  // initial yield stress
  try {
    sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
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
    b0 = material.getDoubleProperty("HARDENING_COEFFICIENT_STORED");
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
   * dissipated part
   */
  double sig1,b1,n1=1.0e0;
  // initial yield stress
  try {
    sig1 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  }
  catch (NoSuchPropertyException) {
    sig1 = 0.0e0;
    material.setProperty("INITIAL_YIELD_STRESS_DISSIPATED",sig1);
  }
  // hardening coefficient
  try {
    b1 = material.getDoubleProperty("HARDENING_COEFFICIENT_DISSIPATED");
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
   * check values
   */
  double sig = sig0+sig1;
  if (sig < 0.e0) {
    if (os) (*os) << "ERROR: initial yield stress must be positive." << std::endl;
    throw InvalidPropertyException("initial yield stress");
  }
  
  /*
   * print-out
   */
  if (os) {
    (*os) << "\tinitial yield stress (stored part)                 = " <<  sig0 << std::endl;
    (*os) << "\thardening coefficient (stored part)                = " <<    b0 << std::endl;
    (*os) << "\thardening exponent (stored part)                   = " <<    n0 << std::endl;
    (*os) << "\tinitial yield stress (dissipated part)             = " <<  sig1 << std::endl;
    (*os) << "\thardening coefficient (dissipated part)            = " <<    b1 << std::endl;
    (*os) << "\thardening exponent (dissipated part)               = " <<    n1 << std::endl;
  }
}

// plastically stored energy
double LudwikIsotropicHardeningModel::storedEnergy(const MaterialProperties& material,
                                                   const ParameterSet& extPar,
                                                   const MatLibArray& intPar0,
                                                   MatLibArray& intPar1,
                                                   double Wp0,double epsPl0,double epsPl1,
                                                   double& sig,double& h,bool first,bool second) {
  static const double PRECISION = 1.0e-16;
  
  // initialize
  double Wp = 0.e0;
  sig = 0.0e0;
  h = 0.0e0;
  
  // power-law (Ludwik)
  double sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
  double b = material.getDoubleProperty("HARDENING_COEFFICIENT_STORED");
  if (b != 0.0e0 && epsPl1 > PRECISION) {
    double n = material.getDoubleProperty("HARDENING_EXPONENT_STORED");
    double expo = 1.0e0/n;
    if (first) {
      double val = std::pow(epsPl1,expo);
      sig = sig0+b*val;
      Wp = (sig0+b*val/(expo+1))*epsPl1;
    }
    else {
      double val = std::pow(epsPl1,expo+1);
      Wp = sig0*epsPl1+b*val/(expo+1);
    }
    if (second) {
      h = expo*b*std::pow(epsPl1,expo-1);
    }
  }
  else {
    Wp = sig0*epsPl1;
    if (first) sig = sig0;
    if (second && b != 0.0)
      h = std::numeric_limits<double>::max();
  }
  
  return Wp;
}

// yield stress
double LudwikIsotropicHardeningModel::yieldStress(const MaterialProperties& material,
                                                  const ParameterSet& extPar,
                                                  const MatLibArray&intPar0,
                                                  MatLibArray& intPar1,
                                                  double epsPl,double& h,bool second) {
  static const double PRECISION = 1.0e-16;
  
  // initialize
  double sig = 0.0e0;
  h = 0.0e0;
  
  // power-law (Ludwik)
  double sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  double b = material.getDoubleProperty("HARDENING_COEFFICIENT_DISSIPATED");
  if (b != 0.0e0 && epsPl > PRECISION) {
    double n = material.getDoubleProperty("HARDENING_EXPONENT_DISSIPATED");
    double expo = 1.0e0/n;
    double val = b*std::pow(epsPl,expo);
    sig = sig0+val;
    if (second)
      h = expo*val/epsPl;
  }
  else {
    sig = sig0;
    if (second && b != 0.0e0)
      h = std::numeric_limits<double>::max();
  }
  
  return sig;
}
