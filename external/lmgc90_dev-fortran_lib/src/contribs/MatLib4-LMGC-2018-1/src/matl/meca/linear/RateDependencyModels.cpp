/*
 *  $Id: RateDependencyModels.cpp 147 2014-08-01 14:46:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "RateDependencyModels.h"

// std C library
#include <cmath>
//#if defined(_WIN32) || defined(_WIN64)
//double asinh(double x) {
//  return std::log(x+std::sqrt(x*x+1.0));
//}
//#endif

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class PowerLawRateDependencyModel.
 */

// check consistency of material properties
void PowerLawRateDependencyModel::checkProperties(MaterialProperties& material,
                                                  std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Power-law rate dependency model***" << std::endl;
  
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
  
  // print-out properties
  if (os) {
    (*os) << "\treference stress               = " << sig0    << std::endl;
    (*os) << "\treference strain rate          = " << epsDot0 << std::endl;
    (*os) << "\trate dependency exponent       = " <<   m     << std::endl;
  }
}

// dissipated energy
double PowerLawRateDependencyModel::dissipatedEnergy(const MaterialProperties& material,
                                                     const ParameterSet& extPar,
                                                     const MatLibArray& intPar0,
                                                     MatLibArray& intPar1,
                                                     double eps,double epsDot,
                                                     double& sig1,double& sig2,
                                                     double& h11,double& h22,double& h12,
                                                     bool first,bool second) {
  // get material parameters
  double sig0 = material.getDoubleProperty("REFERENCE_STRESS");
  if (sig0 == 0.e0 || epsDot <= 0.0e0) {
    sig1 = sig2 = 0.e0;
    h11 = h22 = h12 = 0.e0;
    return 0.0e0;
  }
  double epsDot0 = material.getDoubleProperty("REFERENCE_STRAIN_RATE");
  double m = material.getDoubleProperty("RATE_DEPENDENCY_EXPONENT");
  
  // compute dissipation pseudo-potential and derivatives
  double phi;
  double expo = 1.0e0/m;
  double val = epsDot/epsDot0;
  if (first) {
    sig1 = 0.e0;
    sig2 = sig0*std::pow(val,expo);
    phi = epsDot/(expo+1)*sig2;
  }
  else {
    double expo1 = expo+1;
    phi = sig0*epsDot0/(expo1)*std::pow(val,expo1);
  }
  
  // compute second derivatives
  if (second) {
    h11 = h12 = 0.e0;
    h22 = expo*sig0/epsDot0*std::pow(val,expo-1);
  }
  
  return phi;
}


/*
 * Methods for class ASinhRateDependencyModel.
 */

// check consistency of material properties
void ASinhRateDependencyModel::checkProperties(MaterialProperties& material,
                                               std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***ASinh (thermal activation) rate dependency model***" << std::endl;
  
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

  // print-out properties
  if (os) {
    (*os) << "\treference stress               = " << sig0    << std::endl;
    (*os) << "\treference strain rate          = " << epsDot0 << std::endl;
  }
}

// dissipated energy
double ASinhRateDependencyModel::dissipatedEnergy(const MaterialProperties& material,
                                                  const ParameterSet& extPar,
                                                  const MatLibArray& intPar0,
                                                  MatLibArray& intPar1,
                                                  double eps,double epsDot,
                                                  double& sig1,double& sig2,
                                                  double& h11,double& h22,double& h12,
                                                  bool first,bool second) {
  // get material parameters
  double sig0 = material.getDoubleProperty("REFERENCE_STRESS");
  if (sig0 == 0.e0 || epsDot <= 0.0e0) {
    sig1 = sig2 = 0.e0;
    h11 = h22 = h12 = 0.e0;
    return 0.0e0;
  }
  double epsDot0 = material.getDoubleProperty("REFERENCE_STRAIN_RATE");
  
  // compute dissipation pseudo-potential and derivatives
  double phi;
  double val = epsDot/epsDot0;
  double val1 = std::sqrt(val*val+1.0);
  if (first) {
    sig1 = 0.e0;
    sig2 = sig0*asinh(val);
    phi = epsDot0*(val*sig2-sig0*val1+sig0);
  }
  else {
    phi = sig0*epsDot0*(val*asinh(val)-val1+1.0);
  }
  
  // compute second derivatives
  if (second) {
    h11 = h12 = 0.e0;
    h22 = sig0/(epsDot0*val1);
  }
  
  return phi;
}


/*
 * Methods for class NortonHoffRateDependencyModel.
 */

// check consistency of material properties
void NortonHoffRateDependencyModel::checkProperties(MaterialProperties& material,
                                                    std::ostream* os)
throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Norton-Hoff rate dependency model***" << std::endl;
  
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
  // print-out properties
  if (os) {
    (*os) << "\treference yield stress         = " << sig0    << std::endl;
    (*os) << "\treference strain rate          = " << epsDot0 << std::endl;
    (*os) << "\trate dependency exponent       = " <<   m     << std::endl;
    (*os) << "\treference strain               = " << eps0    << std::endl;
    (*os) << "\thardening exponent             = " <<   n     << std::endl;
  }
}

// dissipated energy
double NortonHoffRateDependencyModel::dissipatedEnergy(const MaterialProperties& material,
                                                       const ParameterSet& extPar,
                                                       const MatLibArray& intPar0,
                                                       MatLibArray& intPar1,
                                                       double eps,double epsDot,
                                                       double& sig1,double& sig2,
                                                       double& h11,double& h22,double& h12,
                                                       bool first,bool second) {
  // get material parameters
  double sig0 = material.getDoubleProperty("REFERENCE_YIELD_STRESS");
  if (sig0 == 0.e0) {
    sig1 = 0.e0;
    sig2 = 0.e0;
    h11 = h22 = h12 = 0.e0;
    return 0.0e0;
  }
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
    phi = sig0*val11*(epsDot*val21+epsDot0*expo2)/(expo2+1.);
  else
    phi = sig0*val11*epsDot;

  // first derivative
  if (first) {
    if (eps >= eps0)
      sig1 = phi*expo1/eps;
    else
      sig1 = 0.0e0;
    sig2 = sig0*val11*val21;
  }
  
  // compute second derivatives
  if (second) {
    if (eps >= eps0) {
      h11 = phi*expo1*(expo1-1.)/(eps*eps);
      h12 = sig0*val11*val21*expo1/eps;
    }
    else {
      h11 = 0.0e0;
      h12 = 0.0e0;
    }
    if (epsDot >= epsDot0)
      h22 = sig0*val11*expo2/epsDot0*std::pow(val2,expo2-1.);
    else
      h22 = 0.0e0;
  }
  
  return phi;
}
