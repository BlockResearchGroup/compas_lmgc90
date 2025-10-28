/*
 *  $Id: StdThMConvectionPotential.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "StdThMConvectionPotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class StdThMConvectionPotential.
 */

// check consistency of material properties
void StdThMConvectionPotential::checkProperties(MaterialProperties& material,
                                                std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Variational (non-linear) thermo-mechanical convection model***" << std::endl;
  
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
double StdThMConvectionPotential::diffusionEnergy(const MaterialProperties& material,
                                                  const ParameterSet& extPar,
                                                  double J,double T,double dT,
                                                  double& p,double& N,double& q,
                                                  double& K0,double& S0,double& S1,
                                                  double& C0,double& C1,double& K1,
                                                  bool first,bool second) {
  // get exchange coefficient
  double h = material.getDoubleProperty("EXCHANGE_COEFFICIENT");
  
  // get reference temperature
  double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
  
  // get potential
  double X;
  double hEff = TRef*h;
  double coef = J*hEff;
  if (first) {
    q = coef*dT;
    X = 0.5*q*dT;
    p = 0.5*hEff*dT*dT;
    N = 0.0e0;
  }
  else {
    X = 0.5*coef*dT*dT;
  }
  
  if (second) {
    K0 = S0 = 0.0e0;
    S1 = hEff*dT;
    C0 = C1 = 0.0e0;
    K1 = coef;
  }
  
  return X;
}


/*
 * Methods for class StdThMConductionBuilder.
 */

// the instance
StdThMConvectionBuilder const* StdThMConvectionBuilder::BUILDER 
= new StdThMConvectionBuilder();

// constructor
StdThMConvectionBuilder::StdThMConvectionBuilder() {
  ModelDictionary::add("VARIATIONAL_THERMO_MECHANICAL_CONVECTION",*this);
}

// build model
ConstitutiveModel* StdThMConvectionBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
    case 2:
    case 1:
      return new StdThMConvection();
      break;
    default:
      return 0;
      break;
  }
}

