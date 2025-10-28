/*
 *  $Id: StdLinChemicalCapacity.cpp 207 2016-08-19 16:52:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "StdLinChemicalCapacity.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// check consistency of material properties
void StdLinChemicalCapacity::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Standard linearized chemical capacity***" << std::endl;
   
  // chemical modulus
  try {
    double C = material.getDoubleProperty("CHEMICAL_MODULUS");
    if (C <= 0.0e0) {
      if (os) (*os) << "ERROR: chemical modulus must be strictly positive." << std::endl;
      throw InvalidPropertyException("chemical modulus");
    }
    double Cinv = 1.0e0/C;
    material.setProperty("CHEMICAL_COMPLIANCE",Cinv);
    if (os) (*os) << "\n\tchemical modulus = " << C << std::endl;
  }
  catch (NoSuchPropertyException) {
    try{
      // compute from gas constant, absolute temperature and reference concentration
      double R;
      try {
        R = material.getDoubleProperty("UNIVERSAL_GAS_CONSTANT");
        if (R <= 0.0e0) {
          if (os) (*os) << "ERROR: gas constant must be strictly positive." << std::endl;
          throw InvalidPropertyException("gas constant");
        }
      }
      catch (NoSuchPropertyException) {
        R = 8.31446; // default value in S.I. units (J.K^-1.mol^-1)
        material.setProperty("UNIVERSAL_GAS_CONSTANT",R);
      }
      double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
      if (T0 <= 0.0e0) {
        if (os) (*os) << "ERROR: reference temperature must be strictly positive." << std::endl;
        throw InvalidPropertyException("reference temperature");
      }
      double c0 = material.getDoubleProperty("REFERENCE_CONCENTRATION");
      if (c0 <= 0.0e0) {
        if (os) (*os) << "ERROR: reference concentration must be strictly positive." << std::endl;
        throw InvalidPropertyException("reference concentration");
      }
      double C = (R*T0)/c0;
      material.setProperty("CHEMICAL_MODULUS",C);
      double Cinv = 1.0e0/C;
      material.setProperty("CHEMICAL_COMPLIANCE",Cinv);

      // print-out
      if (os) {
        (*os) << "\n\tuniversal gas constant  = " << R;
        (*os) << "\n\treference temperature   = " << T0;
        (*os) << "\n\treference concentration = " << c0;
        (*os) << "\n\tchemical modulus        = " << C << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tchemical modulus cannot be defined" << std::endl;
    }
  }
}

// compute Gibbs energy
double StdLinChemicalCapacity::GibbsEnergy(const MaterialProperties& material,
                                           const ParameterSet& extPar,
                                           double c,double& mu,double& C,
                                           bool computeFirst,bool computeSecond) {
  
  // get chemical modulus
  double C0 = material.getDoubleProperty("CHEMICAL_MODULUS");
  
  // compute Gibbs energy
  double G;
  if (computeFirst) {
    mu = C0*c;
    G = 0.5*mu*c;
  }
  else
    G = 0.5*C0*c*c;
  
  if (computeSecond) C = C0;
  
  return G;
}

// compute dual Gibbs energy
double StdLinChemicalCapacity::dualGibbsEnergy(const MaterialProperties& material,
                                               const ParameterSet& extPar,
                                               double mu,double& c,double& C,
                                               bool computeFirst,bool computeSecond) {

  // get chemical modulus
  double C0inv = material.getDoubleProperty("CHEMICAL_COMPLIANCE");
  
  // compute dual Gibbs energy
  double G;
  if (computeFirst) {
    c = C0inv*mu;
    G = 0.5*mu*c;
  }
  else
    G = 0.5*C0inv*mu*mu;
  
  if (computeSecond) C = C0inv;
  
  return G;
}
