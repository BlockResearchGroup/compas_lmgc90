/*
 *  $Id: J2ChemoPlasticity.cpp 236 2017-06-06 09:11:19Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "J2ChemoPlasticity.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class BasicChemoPlasticity.
 */

// check consistency of material properties
void BasicChemoPlasticity::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Rate-independent plasticity model (weakly coupled)***" << std::endl;

  // initial yield stress
  double sig0;
  try {
    sig0 = material.getDoubleProperty("YIELD_IN_TENSION");
  }
  catch (NoSuchPropertyException) {
    try {
      sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS");
      material.setProperty("YIELD_IN_TENSION",sig0);
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: yield stress is not defined." << std::endl;
      throw e;
    }
  }
   
  if (os) {
    (*os) << "\tyield stress (in tension) = " << sig0 << std::endl;
  }
}

// compute irreversible energy and derivatives
double BasicChemoPlasticity::irreversibleEnergy(const MaterialProperties& material,
                                                const ParameterSet& extPar,
                                                const MatLibArray& intPar0,MatLibArray& intPar,
                                                double ePl0,double ePl,double c0,double c,
                                                double& sig,double& mu,double& H,double& dSig,double& C,
                                                double dTime,bool first,bool second) {
  // get yield stress
  double sig0 = material.getDoubleProperty("YIELD_IN_TENSION");
  
  // compute dissipation potential
  double phi=0.0e0;
  double dEPl = ePl-ePl0;
  if (dEPl >= 0.0e0) {
    phi = sig0*dEPl;
    if (first) {
      sig = sig0;
      mu = 0.0e0;
    }
  }
  else
    if (first) {
      sig = 0.0e0;
      mu = 0.0e0;
    }

  if (second) {
    H = 0.0e0;
    dSig = 0.0e0;
    C = 0.0e0;
  }
  
  return phi;
}


/*
 * Methods for class BasicChemoViscoPlasticity.
 */

// check consistency of material properties
void BasicChemoViscoPlasticity::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Rate-dependent plasticity model (weakly coupled)***" << std::endl;
  
  // initial yield stress
  double sig0;
  try {
    sig0 = material.getDoubleProperty("YIELD_IN_TENSION");
  }
  catch (NoSuchPropertyException) {
    try {
      sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS");
      material.setProperty("YIELD_IN_TENSION",sig0);
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: yield stress is not defined." << std::endl;
      throw e;
    }
  }
  
  // reference strain-rate
  double gamDot0;
  try {
    gamDot0 = material.getDoubleProperty("REFERENCE_STRAIN_RATE");
    if (gamDot0 <= 0.0e0) {
      if (os) (*os) << "ERROR: reference strain rate must be strictly positive." << std::endl;
      throw InvalidPropertyException("reference strain rate");
    }
  }
  catch (NoSuchPropertyException e) {
    gamDot0 = 1.0e0;
    material.setProperty("REFERENCE_STRAIN_RATE",gamDot0);
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
 
  if (os) {
    (*os) << "\tyield stress (in tension) = " << sig0;
    (*os) << "\n\treference strain rate     = " << gamDot0;
    (*os) << "\n\trate dependency exponent  = " << m << std::endl;
  }
}

// compute irreversible energy and derivatives
double BasicChemoViscoPlasticity::irreversibleEnergy(const MaterialProperties& material,
                                                     const ParameterSet& extPar,
                                                     const MatLibArray& intPar0,MatLibArray& intPar,
                                                     double ePl0,double ePl,double c0,double c,
                                                     double& sig,double& mu,double& H,double& dSig,double& C,
                                                     double dTime,bool first,bool second) {
  // get model parameters
  double sig0 = material.getDoubleProperty("YIELD_IN_TENSION");
  double gamDot0 = material.getDoubleProperty("REFERENCE_STRAIN_RATE");
  double m = material.getDoubleProperty("RATE_DEPENDENCY_EXPONENT");

  // quick exit
  double dEPl = ePl-ePl0;
  if (dEPl <= 0.0e0) {
    if (first) {
      sig = sig0;
      mu = 0.0e0;
    }
    if (second) {
      H = 0.0e0;
      dSig = 0.0e0;
      C = 0.0e0;
    }
    return 0.0e0;
  }
  
  // compute dissipation potential
  double phi=0.0e0;
  double expo = 1.0e0/m;
  double dEPl0 = dTime*gamDot0;
  double val = dEPl/dEPl0;
  if (first) {
    double sigV = sig0*std::pow(val,expo);
    sig = sig0+sigV;
    phi = (sig0+sigV/(expo+1))*dEPl;
    mu = 0.0e0;
  }
  else {
    double expo1 = expo+1;
    phi = sig0*(dEPl+dEPl0/expo1*std::pow(val,expo1));
  }
  
  // compute second derivatives
  if (second) {
    H = expo*sig0/dEPl0*std::pow(val,expo-1);
    dSig = 0.0e0;
    C = 0.0e0;
  }
  
  return phi;
}


/*
 * Methods for class LinearIsotropicJ2ChemoPlasticityBuilder.
 */

// the instance
LinearIsotropicJ2WkChemoPlasticityBuilder const* LinearIsotropicJ2WkChemoPlasticityBuilder::BUILDER
= new LinearIsotropicJ2WkChemoPlasticityBuilder();

// constructor
LinearIsotropicJ2WkChemoPlasticityBuilder::LinearIsotropicJ2WkChemoPlasticityBuilder() {
  ModelDictionary::add("LINEAR_ISOTROPIC_J2_CHEMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* LinearIsotropicJ2WkChemoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearIsotropicJ2WkChemoPlasticity3D();
      break;
    case 2:
      return new LinearIsotropicJ2WkChemoPlasticity2D();
      break;
    case 1:
      return new LinearIsotropicJ2WkChemoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class LinearIsotropicJ2ChemoViscoPlasticityBuilder.
 */

// the instance
LinearIsotropicJ2WkChemoViscoPlasticityBuilder const* LinearIsotropicJ2WkChemoViscoPlasticityBuilder::BUILDER
= new LinearIsotropicJ2WkChemoViscoPlasticityBuilder();

// constructor
LinearIsotropicJ2WkChemoViscoPlasticityBuilder::LinearIsotropicJ2WkChemoViscoPlasticityBuilder() {
  ModelDictionary::add("LINEAR_ISOTROPIC_J2_CHEMO_VISCO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* LinearIsotropicJ2WkChemoViscoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearIsotropicJ2WkChemoViscoPlasticity3D();
      break;
    case 2:
      return new LinearIsotropicJ2WkChemoViscoPlasticity2D();
      break;
    case 1:
      return new LinearIsotropicJ2WkChemoViscoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

