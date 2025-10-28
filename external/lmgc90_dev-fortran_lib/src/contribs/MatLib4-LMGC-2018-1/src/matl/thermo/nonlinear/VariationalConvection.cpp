/*
 *  $Id: VariationalConvection.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "VariationalConvection.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class VariationalConvection
 */

// empty constructor
VariationalConvection::VariationalConvection(ConvectionPotential* h) {
  count = new unsigned int(1);
  convection = h;
};

// constructor
VariationalConvection::VariationalConvection(ConvectionPotential& h) {
  count = new unsigned int(1);
  convection = &h;
};

// copy constructor
VariationalConvection::VariationalConvection(const VariationalConvection& src) {
  count = src.count;
  (*count)++;
  convection = src.convection;
}

// destructor
VariationalConvection::~VariationalConvection() {
  if (--(*count) > 0) return;
  delete count;
  if (convection) delete convection;
}

// check consistency of material properties
void VariationalConvection::checkProperties(MaterialProperties& material,
                                               std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\nVariational (non-linear) convection:" << std::endl;
  
  // look for algorithmic parameter
  double alpha = 0.5;
  try {
    alpha = material.getDoubleProperty("TH_ALGORITHMIC_PARAMETER");
  }
  catch (NoSuchPropertyException) {
    material.setProperty("TH_ALGORITHMIC_PARAMETER",alpha);
  }
  if (os) (*os) << "\n\talgorithmic parameter = " << alpha << std::endl;
  
  // initial temperature
  try {
    double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
    if (T0 <= 0.e0) {
      if (os) (*os) << "ERROR: Initial temperature must be strictly positive." << std::endl;
      throw InvalidPropertyException("initial temperature");
    }
    if (os) (*os) << "\n\tinitial temperature = " << T0 << std::endl;
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: initial temperature is not defined." << std::endl;
    throw e;
  }
  
  // conduction part
  convection->checkProperties(material,os);
}

// update properties in function of external parameters
void VariationalConvection::updateProperties(MaterialProperties& mater,
                                                const ParameterSet& extPar) {
  convection->updateProperties(mater,extPar);
}

// self-documenting utilities
ConstitutiveModel::VariableType VariationalConvection::typeExtVar(unsigned int i) const {
  switch (i) {
    case 0:
      return ConstitutiveModel::TYPE_SCALAR;
      break;
    case 1:
      return ConstitutiveModel::TYPE_SCALAR;
      break;
    default:
      return ConstitutiveModel::TYPE_NONE;
      break;
  }
}
unsigned int VariationalConvection::indexExtVar(unsigned int i) const {
  switch (i) {
    case 0:
      return 0;
      break;
    case 1:
      return 1;
      break;
    default:
      return 2;
      break;
  }
}
std::string VariationalConvection::labelExtVar(unsigned int i) const {
  switch (i) {
    case 0:
      return "relative temperature difference";
      break;
    case 1:
      return "temperature";
      break;
    default:
      return "";
      break;
  }
}
std::string VariationalConvection::labelExtForce(unsigned int i) const {
  switch (i) {
    case 0:
      return "heat flux";
      break;
    case 1:
      return "entropy difference";
      break;
    default:
      return "";
      break;
  }
}

// initialize the state of the material
void VariationalConvection::initState(const MaterialProperties& material,
                                      MaterialState& state) {
  ConstitutiveModel::initState(material,state);
  state.grad = 0.e0;
  state.flux = 0.e0;
  state.internal = 0.e0;
  
  // set initial temperature
  double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
  state.grad[1] = T0;
}

// compute the incremental potential
double VariationalConvection::incrementalPotential(const MaterialProperties& material,
                                                   const ParameterSet& extPar,
                                                   const MaterialState& state0,
                                                   MaterialState& state,
                                                   double dTime,MatLibMatrix& M,
                                                   bool update,bool tangent) 
 throw (UpdateFailedException) {
  
  // extract temperature gradient and temperature
  double dT = state.grad[0];
  double T0 = state0.grad[1];
  double T1 = state.grad[1];
  
  // compute temperature for the step
  double alpha = material.getDoubleProperty("TH_ALGORITHMIC_PARAMETER");
  double coef = alpha*dTime;
  double T = (1.0-alpha)*T0+alpha*T1;
  
  // compute diffusion energy
  double q,N0,K,S0,C0;
  double X = convection->diffusionEnergy(material,extPar,dT,T,q,N0,K,S0,C0,
                                         update,tangent);
  if (update) {
    state.flux[0] = dTime*q;
    state.flux[1] = coef*N0;
  }
  if (tangent) {
    M[0][0] = dTime*K;
    M[1][0] = M[0][1] = coef*S0;
    M[1][1] = coef*C0;
  }
  
  return dTime*X;
}

