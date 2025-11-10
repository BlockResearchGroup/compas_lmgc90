/*
 *  $Id: ThermalRateDependencyModels.cpp 153 2014-10-03 09:12:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "ThermalRateDependencyModels.h"

// std C library
#include <cmath>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class ThermalPowerLawRateDependencyModel.
 */

// check consistency of material properties
void ThermalPowerLawRateDependencyModel::checkProperties(MaterialProperties& material,
                                                  std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Power-law rate dependency model (temperature-dependent)***" << std::endl;
   
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

  // reference stress
  double sig0;
  try {
    Function& fct = material.getFunctionProperty("REFERENCE_STRESS_EVOLUTION");
    sig0 = fct.value(T0);
    material.setProperty("REFERENCE_STRESS",sig0);
    if (os) {
      (*os) << "\n\treference stress temperature dependence: ";
      (*os) << fct << std::endl;
    }
  }
  catch (NoSuchPropertyException) {
    try {
      sig0 = material.getDoubleProperty("REFERENCE_STRESS");
    }
    catch (NoSuchPropertyException) {
      sig0 = 0.0e0;
      material.setProperty("REFERENCE_STRESS",0.0e0);
    }
  }
  if (sig0 == 0.0e0) {
    if (os) (*os) << "\tnot defined (REFERENCE_STRESS = 0.0e0)" << std::endl;
    return;
  }
  if (sig0 < 0.0e0) {
    if (os) (*os) << "ERROR: reference stress must be positive or zero." << std::endl;
    throw InvalidPropertyException("reference stress");
  }
  
  // reference strain-rate
  double epsDot0;
  try {
    try {
      Function& fct = material.getFunctionProperty("REFERENCE_STRAIN_RATE_EVOLUTION");
      epsDot0 = fct.value(T0);
      material.setProperty("REFERENCE_STRAIN_RATE",epsDot0);
      if (os) {
        (*os) << "\n\treference strain rate temperature dependence: ";
        (*os) << fct << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      epsDot0 = material.getDoubleProperty("REFERENCE_STRAIN_RATE");
    }
    if (epsDot0 <= 0.0e0) {
      if (os) (*os) << "ERROR: reference strain rate must be strictly positive." << std::endl;
      throw InvalidPropertyException("reference strain rate");
    }
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: reference strain rate is not defined." << std::endl;
    throw e;
  }
  
  // rate-dependency exponent
  double m;
  try {
    m = material.getDoubleProperty("RATE_DEPENDENCY_EXPONENT");
    if (m <= 1.0e0) {
      if (os) (*os) << "ERROR: rate dependency exponent must be > 1." << std::endl;
      throw InvalidPropertyException("rate dependency exponent");
    }
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: rate dependency exponent is not defined." << std::endl;
    throw e;
  }
  
  // print-out properties
  if (os) {
    (*os) << "\n\tAt reference temperature (T = " << T0 << "):" << std::endl;
    (*os) << "\treference stress               = " << sig0    << std::endl;
    (*os) << "\treference strain rate          = " << epsDot0 << std::endl;
    (*os) << "\trate dependency exponent       = " <<   m     << std::endl;
  }
}

// update properties in function of external parameters
void ThermalPowerLawRateDependencyModel::updateProperties(MaterialProperties& material,
                                                          const ParameterSet& extPar) {
  if (!extPar.count("TEMPERATURE")) return;
  double T = extPar.find("TEMPERATURE")->second;
  
  // reference stress
  double sig0;
  try {
    Function& fct = material.getFunctionProperty("REFERENCE_STRESS_EVOLUTION");
    sig0 = fct.value(T);
    material.setProperty("REFERENCE_STRESS",sig0);
  }
  catch (NoSuchPropertyException) {
    sig0 = material.getDoubleProperty("REFERENCE_STRESS");
  }
  if (sig0 == 0.0e0) return;
  if (sig0 < 0.0e0) throw InvalidPropertyException("reference stress");
  
  // reference strain-rate
  double epsDot0;
  try {
    Function& fct = material.getFunctionProperty("REFERENCE_STRAIN_RATE_EVOLUTION");
    epsDot0 = fct.value(T);
    material.setProperty("REFERENCE_STRAIN_RATE",epsDot0);
  }
  catch (NoSuchPropertyException) {
    epsDot0 = material.getDoubleProperty("REFERENCE_STRAIN_RATE");
  }
  if (epsDot0 <= 0.0e0) throw InvalidPropertyException("reference strain rate");
}

// dissipated energy
double ThermalPowerLawRateDependencyModel::dissipatedThMEnergy(const MaterialProperties& material,
                                                               const ParameterSet& extPar,
                                                               const MatLibArray& intPar0,
                                                               MatLibArray& intPar1,
                                                               double eps,double epsDot,double Th,
                                                               double& sig1,double& sig2,double& N,
                                                               double& h11,double& h22,double& C,
                                                               double& h12,double& h1T,double& h2T,
                                                               bool first,bool second) {
  // temperature
  double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
  double T = T0+Th;
  
  // get material parameters
  double sig0,dSig0;
  try {
    Function& fct = material.getFunctionProperty("REFERENCE_STRESS_EVOLUTION");
    sig0 = fct.value(T,dSig0);
  }
  catch (NoSuchPropertyException) {
    sig0 = material.getDoubleProperty("REFERENCE_STRESS");
    dSig0 = 0.0e0;
  }
  if (sig0 == 0.e0 || epsDot <= 0.0e0) {
    sig1 = sig2 = N = 0.e0;
    h11 = h22 = h12 = 0.e0;
    C   = h1T = h2T = 0.e0;
    return 0.0e0;
  }
  double epsDot0,dEpsDot0;
  try {
    Function& fct = material.getFunctionProperty("REFERENCE_STRAIN_RATE_EVOLUTION");
    epsDot0 = fct.value(T,dEpsDot0);
  }
  catch (NoSuchPropertyException) {
    epsDot0 = material.getDoubleProperty("REFERENCE_STRAIN_RATE");
    dEpsDot0 = 0.0e0;
  }
  double m = material.getDoubleProperty("RATE_DEPENDENCY_EXPONENT");
  
  // compute dissipation pseudo-potential and derivatives
  double phi;
  double expo = 1.0e0/m;
  double expo1 = expo+1;
  double coef = 1.e0/expo1;
  double val = epsDot/epsDot0;
  if (first) {
    sig1 = 0.e0;
    sig2 = sig0*std::pow(val,expo);
    N = coef*(dSig0/sig0*epsDot0-expo*dEpsDot0)*sig2*val;
    phi = coef*epsDot*sig2;
  }
  else {
    phi = coef*sig0*epsDot0*std::pow(val,expo1);
  }
  
  // compute second derivatives
  if (second) {
    h11 = h12 = h1T = 0.e0;
    double val1 = std::pow(val,expo-1);
    h22 = expo*sig0/epsDot0*val1;
    h2T = (dSig0-expo*sig0*dEpsDot0/epsDot0)*val1*val;
    C = -2*coef*dSig0*dEpsDot0*expo*val1*val*val;
  }
  
  return phi;
}


/*
 * Methods for class ThermalASinhRateDependencyModel.
 */

// check consistency of material properties
void ThermalASinhRateDependencyModel::checkProperties(MaterialProperties& material,
                                                  std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***ASinh rate dependency model (thermally-activated)***" << std::endl;
   
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

  // reference stress
  double sig0;
  try {
    sig0 = material.getDoubleProperty("REFERENCE_STRESS");
  }
  catch (NoSuchPropertyException) {
    sig0 = 0.0e0;
    material.setProperty("REFERENCE_STRESS",0.0e0);
  }
  if (sig0 < 0.0e0) {
    if (os) (*os) << "ERROR: reference stress must be positive or zero." << std::endl;
    throw InvalidPropertyException("reference stress");
  }
  else if (sig0 == 0.0e0) {
    if (os) (*os) << "\tnot defined (REFERENCE_STRESS = 0.0e0)" << std::endl;
    return;
  }
  
  // reference strain-rate
  double epsDot0;
  try {
    epsDot0 = material.getDoubleProperty("REFERENCE_STRAIN_RATE");
    if (epsDot0 <= 0.0e0) {
      if (os) (*os) << "ERROR: reference strain rate must be strictly positive." << std::endl;
      throw InvalidPropertyException("reference strain rate");
    }
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: reference strain rate is not defined." << std::endl;
    throw e;
  }
  
  // critical temperature
  double Tc;
  try {
    Tc = material.getDoubleProperty("CRITICAL_TEMPERATURE");
    if (Tc <= 0.0e0) {
      if (os) (*os) << "ERROR: critical temperature must be > 0." << std::endl;
      throw InvalidPropertyException("critical temperature");
    }
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: critical temperature is not defined." << std::endl;
    throw e;
  }

  // intrinsic properties
  double sig1,epsDot1;
  double coef = Tc/T0;
  sig1 = coef*sig0;
  material.setProperty("INTRINSIC_REFERENCE_STRESS",sig1);
  epsDot1 = epsDot0*std::exp(coef);
  material.setProperty("INTRINSIC_REFERENCE_STRAIN_RATE",epsDot1);

  // print-out properties
  if (os) {
    (*os) << "\n\tAt reference temperature (T = " << T0 << "):" << std::endl;
    (*os) << "\treference stress                = " << sig0    << std::endl;
    (*os) << "\treference strain rate           = " << epsDot0 << std::endl;
    (*os) << "\tcritical temperature            = " <<   Tc    << std::endl;
    (*os) << "\tintrinsic reference stress      = " << sig1    << std::endl;
    (*os) << "\tintrinsic reference strain rate = " << epsDot1 << std::endl;
  }
}

// dissipated energy
double ThermalASinhRateDependencyModel::dissipatedThMEnergy(const MaterialProperties& material,
                                                            const ParameterSet& extPar,
                                                            const MatLibArray& intPar0,
                                                            MatLibArray& intPar1,
                                                            double eps,double epsDot,double Th,
                                                            double& sig1,double& sig2,double& N,
                                                            double& h11,double& h22,double& C,
                                                            double& h12,double& h1T,double& h2T,
                                                            bool first,bool second) {
  // temperature
  double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
  double T = T0+Th;
  
  // get material parameters
  double sig0 = material.getDoubleProperty("INTRINSIC_REFERENCE_STRESS");
  double epsDot0 = material.getDoubleProperty("INTRINSIC_REFERENCE_STRAIN_RATE");
  double Tc = material.getDoubleProperty("CRITICAL_TEMPERATURE");
  
  // effective properties
  double val0 = 1.0/Tc;
  double val1 = 1.0/T;
  double coef0 = T*val0;
  double coef1 = std::exp(-Tc*val1);
  double sigE = coef0*sig0;
  double dSigE = sig0*val0;
  double epsDotE = coef1*epsDot0;
  double dEpsDotE = epsDotE*val1*val1*Tc;
  double d2EpsDotE = dEpsDotE*val1*(Tc*val1-2.0);
  
  // compute dissipation pseudo-potential and derivatives
  double phi;
  double val2 = epsDot/epsDotE;
  double val3 = std::sqrt(val2*val2+1.0);
  if (first) {
    sig1 = 0.e0;
    sig2 = sigE*asinh(val2);
    phi = epsDotE*(val2*sig2-sigE*val3+sigE);
    N = dSigE/sigE*phi-sigE*dEpsDotE*(val3-1.0);
  }
  else {
    phi = sigE*epsDotE*(val2*asinh(val2)-val3+1.0);
  }
  
  // compute second derivatives
  if (second) {
    double val4 = dEpsDotE*val2;
    h11 = h12 = h1T = 0.e0;
    h22 = sigE/(epsDotE*val3);
    h2T = dSigE*asinh(val2)-h22*val4;
    C = -(2*dSigE*dEpsDotE+sigE*d2EpsDotE)*(val3-1.0)+h22*val4*val4;
  }
  
  return phi;
}


/*
 * Methods for class ThermalNortonHoffRateDependencyModel.
 */

// check consistency of material properties
void ThermalNortonHoffRateDependencyModel::checkProperties(MaterialProperties& material,
                                                           std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Norton-Hoff rate dependency model (temperature-dependent)***" << std::endl;

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

  // reference yield stress
  double sig0;
  try {
    sig0 = material.getDoubleProperty("REFERENCE_YIELD_STRESS");
  }
  catch (NoSuchPropertyException) {
    sig0 = 0.0e0;
    material.setProperty("REFERENCE_YIELD_STRESS",0.0e0);
  }
  if (sig0 < 0.0e0) {
    if (os) (*os) << "ERROR: reference yield stress must be positive or zero." << std::endl;
    throw InvalidPropertyException("reference stress");
  }
  else if (sig0 == 0.0e0) {
    if (os) (*os) << "\tnot defined (REFERENCE_YIELD_STRESS = 0.0e0)" << std::endl;
    return;
  }
   
  // critical temperature
  double Tc;
  try {
    Tc = material.getDoubleProperty("CRITICAL_TEMPERATURE");
    if (Tc <= 0.0e0) {
      if (os) (*os) << "ERROR: critical temperature must be > 0." << std::endl;
      throw InvalidPropertyException("critical temperature");
    }
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: critical temperature is not defined." << std::endl;
    throw e;
  }

  // reference strain-rate
  double epsDot0,m;
  try {
    epsDot0 = material.getDoubleProperty("REFERENCE_STRAIN_RATE");
    if (epsDot0 <= 0.0e0) {
      if (os) (*os) << "ERROR: reference strain rate must be strictly positive." << std::endl;
      throw InvalidPropertyException("reference strain rate");
    }
  }
  catch (NoSuchPropertyException e) {
    epsDot0 = 1.0e0;
    material.setProperty("REFERENCE_STRAIN_RATE",epsDot0);
  }
  
  try {
    m = material.getDoubleProperty("RATE_DEPENDENCY_EXPONENT");
    if (m <= 1.0e0) {
      if (os) (*os) << "ERROR: rate dependency exponent must be > 1." << std::endl;
      throw InvalidPropertyException("rate dependency exponent");
    }
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: rate dependency exponent is not defined." << std::endl;
    throw e;
  }
  
  // reference strain
  double eps0,n;
  try {
    eps0 = material.getDoubleProperty("REFERENCE_STRAIN_NH");
    if (eps0 <= 0.0e0) {
      if (os) (*os) << "ERROR: reference strain must be strictly positive." << std::endl;
      throw InvalidPropertyException("reference strain");
    }
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: reference strain is not defined." << std::endl;
    throw e;
  }
  
  try {
    n = material.getDoubleProperty("HARDENING_EXPONENT_NH");
    if (n <= 1.0e0) {
      if (os) (*os) << "ERROR: hardening exponent must be > 1." << std::endl;
      throw InvalidPropertyException("hardening exponent");
    }
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: hardening exponent is not defined." << std::endl;
    throw e;
  }
   
   // intrinsic properties
   double sig1;
   double coef = Tc/T0;
   sig1 = sig0*std::exp(-coef);
   material.setProperty("INTRINSIC_REFERENCE_STRESS",sig1);

  // print-out properties
  if (os) {
    (*os) << "\n\tAt reference temperature (T = " << T0 << "):" << std::endl;
    (*os) << "\treference yield stress          = " << sig0    << std::endl;
    (*os) << "\tcritical temperature            = " <<   Tc    << std::endl;
    (*os) << "\tintrinsic reference stress      = " << sig1    << std::endl;
    (*os) << "\treference strain rate           = " << epsDot0 << std::endl;
    (*os) << "\trate dependency exponent        = " <<   m     << std::endl;
    (*os) << "\treference strain                = " << eps0    << std::endl;
    (*os) << "\thardening exponent              = " <<   n     << std::endl;
  }
}

// dissipated energy
double ThermalNortonHoffRateDependencyModel::dissipatedThMEnergy(const MaterialProperties& material,
                                                                 const ParameterSet& extPar,
                                                                 const MatLibArray& intPar0,
                                                                 MatLibArray& intPar1,
                                                                 double eps,double epsDot,double Th,
                                                                 double& sig1,double& sig2,double& N,
                                                                 double& h11,double& h22,double& C,
                                                                 double& h12,double& h1T,double& h2T,
                                                                 bool first,bool second) {
  // temperature
  double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
  double T = T0+Th;
  
  // get material parameters
  double sig0 = material.getDoubleProperty("INTRINSIC_REFERENCE_STRESS");
  if (sig0 == 0.0e0) {
    sig1 = 0.0e0;
    sig2 = 0.0e0;
    N = 0.0e0;
    h11 = h22 = h12 = 0.0e0;
    C = h1T = h2T = 0.0e0;
    return 0.0e0;
  }
  double Tc = material.getDoubleProperty("CRITICAL_TEMPERATURE");
  double coef = Tc/T;
  double sigE = sig0*std::exp(coef);
  double epsDot0 = material.getDoubleProperty("REFERENCE_STRAIN_RATE");
  double m = material.getDoubleProperty("RATE_DEPENDENCY_EXPONENT");
  double eps0 = material.getDoubleProperty("REFERENCE_STRAIN_NH");
  double n = material.getDoubleProperty("HARDENING_EXPONENT_NH");
  
  // compute dissipation pseudo-potential and derivatives
  double phi;
  double expo1 = 1.0e0/n;
  double expo2 = 1.0e0/m;
  double val1,val2,val11,val21;
  if (eps >= eps0) {
    val1 = eps/eps0;
    val11 = std::pow(val1,expo1);
  }
  else {
    val1 = 1.0e0;
    val11 = 1.0e0;
  }
  if (epsDot >= epsDot0) {
    val2 = epsDot/epsDot0;
    val21 = std::pow(val2,expo2);
  }
  else {
    val2 = 1.0e0;
    val21 = 1.0e0;
  }
  
  // pseudo-potential
  if (epsDot >= epsDot0)
    phi = sigE*val11*(epsDot*val21+epsDot0*expo2)/(expo2+1.);
  else
    phi = sigE*val11*epsDot;
  if (!first && !second) return phi;
  
  // first derivative
  double coef1 = -coef/T;
  if (first) {
    if (eps >= eps0)
      sig1 = phi*expo1/eps;
    else
      sig1 = 0.0e0;
    sig2 = sigE*val11*val21;
    N = phi*coef1;
  }
  
  // compute second derivatives
  if (second) {
    if (eps >= eps0) {
      h11 = phi*expo1*(expo1-1.)/(eps*eps);
      h12 = sigE*val11*val21*expo1/eps;
    }
    else {
      h11 = 0.0e0;
      h12 = 0.0e0;
    }
    h1T = sig1*coef1;
    if (epsDot >= epsDot0)
      h22 = sigE*val11*expo2/epsDot0*std::pow(val2,expo2-1.);
    else
      h22 = 0.0e0;
    h2T = sig2*coef1;
    C = N*coef1;
  }
  
  return phi;
}
