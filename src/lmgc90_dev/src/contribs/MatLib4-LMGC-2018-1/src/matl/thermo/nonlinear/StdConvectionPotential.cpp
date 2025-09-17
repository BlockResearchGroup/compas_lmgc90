/*
 *  $Id: StdConvectionPotential.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "StdConvectionPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class StdConvectionPotential.
 */

// check consistency of material properties
void StdConvectionPotential::checkProperties(MaterialProperties& material,
                                                std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Variational (non-linear) convection model***" << std::endl;

  // exchange coefficient
  double h = material.getDoubleProperty("EXCHANGE_COEFFICIENT");
  if (h < 0.e0) {
    if (os) (*os) << "ERROR: convection exchange coefficient must be positive." << std::endl;
    throw InvalidPropertyException("convection exchange coefficient");
  }
  if (os) (*os) << "\n\tconvection exchange coefficient = " << h << std::endl;
  
  // reference temperature
  try {
    double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    if (TRef <= 0.e0) {
      if (os) (*os) << "ERROR: reference temperature must be strictly positive." << std::endl;
      throw InvalidPropertyException("reference temperature");
    }
    if (os) (*os) << "\n\treference temperature           = " << TRef << std::endl;
  }
  catch (NoSuchPropertyException) {
    // use initial temperature
    try {
      double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
      if (T0 <= 0.e0) {
        if (os) (*os) << "ERROR: initial temperature must be strictly positive." << std::endl;
        throw InvalidPropertyException("initial temperature");
      }
      material.setProperty("REFERENCE_TEMPERATURE",T0);
      if (os) (*os) << "\n\treference temperature           = " << T0 << std::endl;
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: reference temperature cannot be set." << std::endl;
      throw e;
    }
  }
}

// compute 
double StdConvectionPotential::diffusionEnergy(const MaterialProperties& material,
                                               const ParameterSet& extPar,
                                               double dT,double T,double& q,
                                               double& N,double& K,double& S,
                                               double& C,bool first,bool second) {
  // get exchange coefficient
  double h = material.getDoubleProperty("EXCHANGE_COEFFICIENT");
  
  // get reference temperature
  double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
  
  // get potential
  double X;
  double hEff = TRef*h;
  if (first) {
    q = hEff*dT;
    X = 0.5*q*dT;
    N = 0.0e0;
  }
  else {
    X = 0.5*dT*hEff*dT;
  }
  
  if (second) {
    K = hEff;
    S = C = 0.0e0;
  }
  
  return X;
}


/*
 * Methods for class StdVariationalConductionBuilder.
 */

// the instance
StdVariationalConvectionBuilder const* StdVariationalConvectionBuilder::BUILDER 
= new StdVariationalConvectionBuilder();

// constructor
StdVariationalConvectionBuilder::StdVariationalConvectionBuilder() {
  ModelDictionary::add("VARIATIONAL_CONVECTION",*this);
}

// build model
ConstitutiveModel* StdVariationalConvectionBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
    case 2:
    case 1:
      return new StdVariationalConvection();
      break;
    default:
      return 0;
      break;
  }
}
