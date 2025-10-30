/*
 *  $Id: J2ChemoPlasticityCoupled.cpp 243 2017-06-22 12:16:37Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "J2ChemoPlasticityCoupled.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class BasicChemoPlasticityCoupled.
 */

// check consistency of material properties
void BasicChemoPlasticityCoupled::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Rate-independent plasticity model***" << std::endl;

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
double BasicChemoPlasticityCoupled::irreversibleEnergy(const MaterialProperties& material,
                                                       const ParameterSet& extPar,
                                                       const MatLibArray& intPar0,MatLibArray& intPar,
                                                       double alpha,double& sig,double& H,
                                                       double dTime,bool first,bool second) {
  // get yield stress
  double sig0 = material.getDoubleProperty("YIELD_IN_TENSION");
  
  // compute dissipation potential
  double phi=0.0e0;
  if (alpha >= 0.0e0) {
    phi = sig0*alpha;
    if (first) sig = sig0;
  }
  else
    if (first) sig = 0.0e0;

  if (second) H = 0.0e0;
  
  return phi;
}


/*
 * Methods for class BasicChemoViscoPlasticityCoupled.
 */

// check consistency of material properties
void BasicChemoViscoPlasticityCoupled::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Rate-dependent plasticity model***" << std::endl;
  
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
double BasicChemoViscoPlasticityCoupled::irreversibleEnergy(const MaterialProperties& material,
                                                            const ParameterSet& extPar,
                                                            const MatLibArray& intPar0,MatLibArray& intPar,
                                                            double alpha,double& sig,double& H,
                                                            double dTime,bool first,bool second) {
  // get model parameters
  double sig0 = material.getDoubleProperty("YIELD_IN_TENSION");
  double gamDot0 = material.getDoubleProperty("REFERENCE_STRAIN_RATE");
  double m = material.getDoubleProperty("RATE_DEPENDENCY_EXPONENT");

  // quick exit
  if (alpha <= 0.0e0) {
    if (first) sig = sig0;
    if (second) H = 0.0e0;
    return 0.0e0;
  }
  
  // compute dissipation potential
  double phi=0.0e0;
  double expo = 1.0e0/m;
  double alpha0 = dTime*gamDot0;
  double val = alpha/alpha0;
  if (first) {
    double sigV = sig0*std::pow(val,expo);
    sig = sig0+sigV;
    phi = (sig0+sigV/(expo+1))*alpha;
  }
  else {
    double expo1 = expo+1;
    phi = sig0*(alpha+alpha0/expo1*std::pow(val,expo1));
  }
  
  // compute second derivatives
  if (second) {
    H = expo*sig0/alpha0*std::pow(val,expo-1);
  }
  
  return phi;
}


/*
 * Methods for class LinearIsotropicJ2ChemoPlasticityBuilder.
 */

// the instance
LinearIsotropicJ2ChemoPlasticityBuilder const* LinearIsotropicJ2ChemoPlasticityBuilder::BUILDER
= new LinearIsotropicJ2ChemoPlasticityBuilder();

// constructor
LinearIsotropicJ2ChemoPlasticityBuilder::LinearIsotropicJ2ChemoPlasticityBuilder() {
  ModelDictionary::add("LINEAR_ISOTROPIC_J2_CHEMO_PLASTICITY_ELLIPTIC",*this);
}

// build model
ConstitutiveModel* LinearIsotropicJ2ChemoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearIsotropicJ2ChemoPlasticity3D();
      break;
    case 2:
      return new LinearIsotropicJ2ChemoPlasticity2D();
      break;
    case 1:
      return new LinearIsotropicJ2ChemoPlasticity1D();
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
LinearIsotropicJ2ChemoViscoPlasticityBuilder const* LinearIsotropicJ2ChemoViscoPlasticityBuilder::BUILDER
= new LinearIsotropicJ2ChemoViscoPlasticityBuilder();

// constructor
LinearIsotropicJ2ChemoViscoPlasticityBuilder::LinearIsotropicJ2ChemoViscoPlasticityBuilder() {
  ModelDictionary::add("LINEAR_ISOTROPIC_J2_CHEMO_VISCO_PLASTICITY_ELLIPTIC",*this);
}

// build model
ConstitutiveModel* LinearIsotropicJ2ChemoViscoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearIsotropicJ2ChemoViscoPlasticity3D();
      break;
    case 2:
      return new LinearIsotropicJ2ChemoViscoPlasticity2D();
      break;
    case 1:
      return new LinearIsotropicJ2ChemoViscoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

