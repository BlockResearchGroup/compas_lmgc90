/*
 *  $Id: CoupledThMConvection.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "CoupledThMConvection.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class StdThMConvection
 */

// empty constructor
CoupledThMConvection::CoupledThMConvection(ConvectionPotential* h) {
  count = new unsigned int(1);
  convection = h;
};

// constructor
CoupledThMConvection::CoupledThMConvection(ConvectionPotential& h) {
  count = new unsigned int(1);
  convection = &h;
};

// copy constructor
CoupledThMConvection::CoupledThMConvection(const CoupledThMConvection& src) {
  count = src.count;
  (*count)++;
  convection = src.convection;
}

// destructor
CoupledThMConvection::~CoupledThMConvection() {
  if (--(*count) > 0) return;
  delete count;
  if (convection) delete convection;
}

// check consistency of material properties
void CoupledThMConvection::checkProperties(MaterialProperties& material,
                                           std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\nVariational (non-linear) thermo-mechanical convection:" << std::endl;

  // look for algorithmic parameter
  double alpha = 0.5;
  try {
    alpha = material.getDoubleProperty("THM_ALGORITHMIC_PARAMETER");
  }
  catch (NoSuchPropertyException) {
    material.setProperty("THM_ALGORITHMIC_PARAMETER",alpha);
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
  
  // convection part
  convection->checkProperties(material,os);
}

// update properties in function of external parameters
void CoupledThMConvection::updateProperties(MaterialProperties& mater,
                                            const ParameterSet& extPar) {
  convection->updateProperties(mater,extPar);
}

// self-documenting utilities
ConstitutiveModel::VariableType CoupledThMConvection::typeExtVar(unsigned int i) const {
  switch (i) {
    case 0:
      return ConstitutiveModel::TYPE_SCALAR;
      break;
    case 1:
      return ConstitutiveModel::TYPE_SCALAR;
      break;
    case 2:
      return ConstitutiveModel::TYPE_SCALAR;
      break;
    default:
      return ConstitutiveModel::TYPE_NONE;
      break;
  }
}
unsigned int CoupledThMConvection::indexExtVar(unsigned int i) const {
  switch (i) {
    case 0:
      return 0;
      break;
    case 1:
      return 1;
      break;
    case 2:
      return 2;
      break;
    default:
      return 3;
      break;
  }
}
std::string CoupledThMConvection::labelExtVar(unsigned int i) const {
  switch (i) {
    case 0:
      return "deformation";
      break;
    case 1:
      return "temperature";
      break;
    case 2:
      return "relative temperature difference";
      break;
    default:
      return "";
      break;
  }
}
std::string CoupledThMConvection::labelExtForce(unsigned int i) const {
  switch (i) {
    case 0:
      return "pressure";
      break;
    case 1:
      return "entropy difference";
      break;
    case 2:
      return "heat flux";
      break;
    default:
      return "";
      break;
  }
}

// initialize the state of the material
void CoupledThMConvection::initState(const MaterialProperties& material,
                                     MaterialState& state) {
  ConstitutiveModel::initState(material,state);
  state.grad = 0.e0;
  state.flux = 0.e0;
  state.internal = 0.e0;
  
  // set initial deformation
  state.grad[0] = 1.0e0;

  // set initial temperature
  double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
  state.grad[1] = T0;
}

// compute the incremental potential
double CoupledThMConvection::incrementalPotential(const MaterialProperties& material,
                                                  const ParameterSet& extPar,
                                                  const MaterialState& state0,
                                                  MaterialState& state,
                                                  double dTime,MatLibMatrix& M,
                                                  bool update,bool tangent) 
 throw (UpdateFailedException) {

  // extract jacobians
  double J0 = state0.grad[0];
  double J1 = state.grad[0];

  // extract temperature and temperature gradient
  double T0 = state0.grad[1];
  double T1 = state.grad[1];
  double dT = state.grad[2];

  // compute jacobian and temperature for the step
  double alpha = material.getDoubleProperty("THM_ALGORITHMIC_PARAMETER");
  double coef = alpha*dTime;
  double J = (1.0-alpha)*J0+alpha*J1;
  double T = (1.0-alpha)*T0+alpha*T1;
  
  // compute diffusion energy
  double p,q,N0,K0,K1,S0,S1,C0,C1;
  double X = convection->diffusionEnergy(material,extPar,J,T,dT,p,N0,q,
                                         K0,S0,S1,C0,C1,K1,
                                         update,tangent);
  if (update) {
    state.flux[0] = -coef*p;
    state.flux[1] = -coef*N0;
    state.flux[2] = -dTime*q;
  }
  if (tangent) {
    M[0][0] = -coef*K0;
    M[1][0] = M[0][1] = -coef*S0;
    M[2][0] = M[0][2] = -coef*S1;
    M[1][1] = -coef*C0;
    M[2][1] = M[1][2] = -coef*C1;
    M[2][2] = -dTime*K1;
  }
  
  return -dTime*X;
}

