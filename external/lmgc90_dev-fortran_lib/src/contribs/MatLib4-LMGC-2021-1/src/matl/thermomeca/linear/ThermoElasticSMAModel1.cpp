/*
 *  $Id: ThermoElasticSMAModel1.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "ThermoElasticSMAModel1.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class StdSMALatentHeat.
 */

// check consistency of material properties
void StdSMALatentHeatThermalPotential::checkProperties(MaterialProperties& material,
						       std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Standard latent heat thermal potential (SMA)***" << std::endl;

  // capacity
  try {
    double Lv = material.getDoubleProperty("VOLUMIC_LATENT_HEAT");
    if (os) (*os) << "\n\tvolumic latent heat   = " << Lv << std::endl;
  }
  catch (NoSuchPropertyException) {
    try{
      double L = material.getDoubleProperty("SPECIFIC_LATENT_HEAT");
      if (os) (*os) << "\n\tspecific latent heat  = " << L << std::endl;
      double rho = material.getDoubleProperty("MASS_DENSITY");
      double Lv = rho*L;
      material.setProperty("VOLUMIC_LATENT_HEAT",Lv);
      if (os) (*os) << "\tvolumic latent heat   = " << Lv << std::endl;
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: volumic latent heat cannot not computed." << std::endl;
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
    if (os) (*os) << "\treference temperature   = " << TRef << std::endl;
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
      if (os) (*os) << "\treference temperature   = " << T0 << std::endl;
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: reference temperature cannot be set." << std::endl;
      throw e;
    }
  }
  
  // austenitic transformation temperature
  double Ta,Tm;
  try {
    Ta = material.getDoubleProperty("AUSTENITIC_TEMPERATURE");
    if (os) (*os) << "\taustenitic temperature  = " << Ta << std::endl;
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: austenitic temperature is not defined." << std::endl;
    throw e;
  }
  
  // martensitic transformation temperature
  try {
    Tm = material.getDoubleProperty("MARTENSITIC_TEMPERATURE");
    if (Tm > Ta) {
      if (os) (*os) << "ERROR: martensitic temperature must be below austenitic one." << std::endl;
      throw InvalidPropertyException("martensitic temperature");
    }
    if (os) (*os) << "\tmartensitic temperature = " << Tm << std::endl;
  }
  catch (NoSuchPropertyException) {
    material.setProperty("MARTENSITIC_TEMPERATURE",Ta);
  }
}

// compute latent heat energy
double StdSMALatentHeatThermalPotential::latentHeat(const MaterialProperties& material,
						    const ParameterSet& extPar,
						    double Th,double X,double& N,double& Y,
						    double& C11,double& C22,double& C12,
						    bool first,bool second) {
  
  // get properties
  double Lv = material.getDoubleProperty("VOLUMIC_LATENT_HEAT");
  double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
  double Ta = material.getDoubleProperty("AUSTENITIC_TEMPERATURE");
  double Tm = material.getDoubleProperty("MARTENSITIC_TEMPERATURE");
  
  // compute latent heat potential
  double coef = Lv/T0;
  double T = T0+Th;
  double W = coef*(T-Ta)*X;
  if (first) {
    N = coef*X;
    Y = coef*(T-Ta);
  }
  if (second) {
    C11 = 0.0e0;
    C22 = 0.0e0;
    C12 = coef;
  }
  
  // additional term?
  if (Tm < Ta) {
    double coef1 = coef*(Ta-Tm);
    W += 0.5*coef1*X*X;
    if (first) Y += coef1*X;
    if (second) C22 += coef1;
  }
  
  return W;
}


/*
 * Methods for class IsotropicThermoElasticSMAModel1Builder.
 */

// the instance
ThermoElasticSMAModel1Builder const* ThermoElasticSMAModel1Builder::BUILDER 
= new ThermoElasticSMAModel1Builder();

// constructor
ThermoElasticSMAModel1Builder::ThermoElasticSMAModel1Builder() {
  ModelDictionary::add("ISOTROPIC_THERMO_ELASTIC_SMA_1",*this);
}

// build model
ConstitutiveModel* ThermoElasticSMAModel1Builder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new ThermoElasticSMAModel1_3D();
      break;
    case 2:
      return new ThermoElasticSMAModel1_2D();
      break;
    case 1:
      return new ThermoElasticSMAModel1_1D();
      break;
    default:
      return 0;
      break;
  }
}
