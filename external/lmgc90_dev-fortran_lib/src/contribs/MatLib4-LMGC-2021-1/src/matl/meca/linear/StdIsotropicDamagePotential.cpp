/*
 *  $Id: StdIsotropicDamagePotential.cpp 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "StdIsotropicDamagePotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class StdIsotropicDamagePotential.
 */

// check consistency of material properties
void StdIsotropicDamagePotential::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Standard isotropic damage potential***" << std::endl;

  // Young's modulus (must have been defined in elasticity)
  double E = material.getDoubleProperty("YOUNG_MODULUS");
   
  // get Y0
  double Y0,epsC;
  try {
    Y0 = material.getDoubleProperty("CRITICAL_ENERGY_RELEASE_RATE");
    if (Y0 <= 0.0e0) {
      if (os) (*os) << "ERROR: critical energy release rate must be positive." << std::endl;
      throw InvalidPropertyException("critical energy release rate");
    }
  
    // compute critical strain
    epsC = std::sqrt(2*Y0/E);
    material.setProperty("CRITICAL_EQUIVALENT_STRAIN",epsC);
  }
  catch (NoSuchPropertyException) {
    try {
      epsC = material.getDoubleProperty("CRITICAL_EQUIVALENT_STRAIN");
      if (epsC <= 0.0e0) {
        if (os) (*os) << "ERROR: critical equivalent strain must be positive." << std::endl;
        throw InvalidPropertyException("critical equivalent strain");
      }
      Y0 = 0.5*E*epsC*epsC;
      material.setProperty("CRITICAL_ENERGY_RELEASE_RATE",Y0);
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: critical energy release rate is not defined." << std::endl;
      throw e;
    }
  }

  // get reference damage rate
  double dDot0;
  try {
    dDot0 = material.getDoubleProperty("REFERENCE_DAMAGE_RATE");
    if (dDot0 <= 0.0e0) {
      if (os) (*os) << "ERROR: reference damage rate must be positive." << std::endl;
      throw InvalidPropertyException("reference damage rate");
    }
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: reference damage rate is not defined." << std::endl;
    throw e;
  }

  // get exponent
  double N;
  try {
    N = material.getDoubleProperty("DAMAGE_EXPONENT");
    if (N < 1.0e0) {
      if (os) (*os) << "ERROR: damage exponent must be > 1." << std::endl;
      throw InvalidPropertyException("damage exponent");
    }
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: damage exponent is not defined." << std::endl;
    throw e;
  }
  
  if (os) {
    (*os) << "\tcritical energy release rate = " << Y0 << std::endl;
    (*os) << "\tcritical equivalent strain   = " << epsC << std::endl;
    (*os) << "\treference damage rate        = " << dDot0 << std::endl;
    (*os) << "\tdamage exponent              = " << N << std::endl;
  }
}

// dissipated energy
double StdIsotropicDamagePotential::dissipatedEnergy(const MaterialProperties& material,
                                                     const ParameterSet& extPar,
                                                     const MatLibArray& intPar0,
                                                     MatLibArray& intPar1,
                                                     double dDot,double d,
                                                     double& Y,double& Yd,
                                                     double& K,double& Kd,double& Kdd,
                                                     bool first,bool second) {
  // quick return
  if (dDot <= 0.0e0) {
    if (first) {
      Y  = 0.0e0;
      Yd = 0.0e0;
    }
    if (second) {
      K = 0.0e0;
      Kd = 0.0e0;
      Kdd = 0.0e0;
    }
    return 0.0e0;
  }

  // get material parameters
  double Y0 = material.getDoubleProperty("CRITICAL_ENERGY_RELEASE_RATE");
  double dDot0 = material.getDoubleProperty("REFERENCE_DAMAGE_RATE");
  double n = material.getDoubleProperty("DAMAGE_EXPONENT");

  // compute dissipation pseudo-potential and derivative
  double phi;
  double val = dDot/dDot0;
  double expo = 1.0e0/n;
  if (first) {
    Y = Y0*std::pow(val,expo);
    Yd = 0.0e0;
    phi = dDot*Y/(expo+1);
  }
  else {
    double expo1 = expo+1;
    phi = Y0*dDot0*std::pow(val,expo1)/(expo1);
  }
  
  // compute second derivative
  if (second) {
    K = (expo*Y0/dDot0)*std::pow(val,expo-1);
    Kd = 0.0e0;
    Kdd = 0.0e0;
  }
  
  return phi;
}

/*
 * Methods for class IsotropicElasticDamageBuilder.
 */

// the instance
IsotropicElasticDamageBuilder const* IsotropicElasticDamageBuilder::BUILDER 
  = new IsotropicElasticDamageBuilder();

// constructor
IsotropicElasticDamageBuilder::IsotropicElasticDamageBuilder() {
  ModelDictionary::add("ISOTROPIC_ELASTIC_DAMAGE",*this);
}

// build model
ConstitutiveModel* IsotropicElasticDamageBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicElasticDamage3D();
      break;
    case 2:
      return new IsotropicElasticDamage2D();
      break;
    case 1:
      return new IsotropicElasticDamage1D();
      break;
    default:
      return 0;
      break;
  }
}
