/*
 *  $Id: FickChemicalCapacity.cpp 207 2016-08-19 16:52:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "FickChemicalCapacity.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>


// check consistency of material properties
void FickChemicalCapacity::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Fickian chemical capacity***" << std::endl;
   
  // reference concentration
  double c0 = material.getDoubleProperty("REFERENCE_CONCENTRATION");
  if (c0 <= 0.0e0) {
    if (os) (*os) << "ERROR: reference concentration must be strictly positive." << std::endl;
    throw InvalidPropertyException("reference concentration");
  }
  if (os) (*os) << "\n\treference concentration = " << c0;

  // chemical modulus
  try {
    double C = material.getDoubleProperty("CHEMICAL_MODULUS");
    if (C <= 0.0e0) {
      if (os) (*os) << "ERROR: chemical must be strictly positive." << std::endl;
      throw InvalidPropertyException("chemical modulus");
    }
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
      double C = (R*T0)/c0;
      material.setProperty("CHEMICAL_MODULUS",C);

      // print-out
      if (os) {
        (*os) << "\n\tuniversal gas constant  = " << R;
        (*os) << "\n\treference temperature   = " << T0;
        (*os) << "\n\tchemical modulus        = " << C << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tchemical modulus cannot be defined" << std::endl;
    }
  }
}

// compute Gibbs energy
double FickChemicalCapacity::GibbsEnergy(const MaterialProperties& material,
                                         const ParameterSet& extPar,
                                         double dc,double& mu,double& C,
                                         bool computeFirst,bool computeSecond) {
  // get reference concentration
  double c0 = material.getDoubleProperty("REFERENCE_CONCENTRATION");

  // get chemical modulus
  double C0 = material.getDoubleProperty("CHEMICAL_MODULUS");

  // compute Gibbs energy
  double c = c0+dc;
  double x = std::log(c/c0);
  double RT = C0*c0;
  double G = RT*(c*x-dc);
  if (computeFirst) mu = RT*x;
  if (computeSecond) C = RT/c;
  
  return G;
}

// compute dual Gibbs energy
double FickChemicalCapacity::dualGibbsEnergy(const MaterialProperties& material,
                                             const ParameterSet& extPar,
                                             double mu,double& dc,double& C,
                                             bool computeFirst,bool computeSecond) {
  // get reference concentration
  double c0 = material.getDoubleProperty("REFERENCE_CONCENTRATION");

  // get chemical modulus
  double C0 = material.getDoubleProperty("CHEMICAL_MODULUS");
  
  // compute dual Gibbs energy
  double RT = C0*c0;
  double x = std::exp(mu/RT);
  double G = (RT*(x-1.0e0)-mu)*c0;
  if (computeFirst) dc = c0*(x-1.0e0);
  if (computeSecond) C = c0*x/RT;
  
  return G;
}
