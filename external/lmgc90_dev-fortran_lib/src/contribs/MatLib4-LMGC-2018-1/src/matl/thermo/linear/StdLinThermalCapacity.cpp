/*
 *  $Id: StdLinThermalCapacity.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "StdLinThermalCapacity.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// check consistency of material properties
void StdLinThermalCapacity::checkProperties(MaterialProperties& material,std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Standard linearized thermal capacity***" << std::endl;
   
  // capacity
  try {
    double Cv = material.getDoubleProperty("VOLUMIC_HEAT_CAPACITY");
    if (os) (*os) << "\n\tvolumic capacity = " << Cv << std::endl;
  }
  catch (NoSuchPropertyException) {
    try{
      double C = material.getDoubleProperty("SPECIFIC_HEAT_CAPACITY");
      if (os) (*os) << "\n\tspecific capacity = " << C << std::endl;
      double rho = material.getDoubleProperty("MASS_DENSITY");
      double Cv = rho*C;
      material.setProperty("VOLUMIC_HEAT_CAPACITY",Cv);
      if (os) (*os) << "\tvolumic capacity  = " << Cv << std::endl;
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: volumic capacity cannot be computed." << std::endl;
      throw e;
    }
  }
  
  // reference temperature
  try {
    double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    if (TRef <= 0.e0) {
      if (os) (*os) << "ERROR: reference temperature must be strictly positive." << std::endl;
      throw InvalidPropertyException("reference temperature");
    }
    if (os) (*os) << "\n\treference temperature = " << TRef << std::endl;
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
      if (os) (*os) << "\n\treference temperature = " << T0 << std::endl;
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: reference temperature cannot be set." << std::endl;
      throw e;
    }
  }
}

// compute 
double StdLinThermalCapacity::internalEnergy(const MaterialProperties& material,
                                             const ParameterSet& extPar,
                                             double Th,double& N,double& C,
                                             bool computeFirst,bool computeSecond) {

  // get volumic capacity and initial temperature
  double Cv = material.getDoubleProperty("VOLUMIC_HEAT_CAPACITY");
  double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
  
  // compute internal energy
  double W;
  double coef = -Cv/T0;
  if (computeFirst) {
    N = coef*Th;
    W = 0.5*Th*N;
  }
  else
    W = 0.5*coef*Th*Th;
  
  if (computeSecond) C = coef;
  
  return W;
}
