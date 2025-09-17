/*
 *  $Id: CoupledLinThMConvection.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "CoupledLinThMConvection.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class CoupledLinThMConvection
 */

// empty constructor
CoupledLinThMConvection::CoupledLinThMConvection(ConvectionPotential* h) {
  count = new unsigned int(1);
  convection = h;
};

// constructor
CoupledLinThMConvection::CoupledLinThMConvection(ConvectionPotential& h) {
  count = new unsigned int(1);
  convection = &h;
};

// copy constructor
CoupledLinThMConvection::CoupledLinThMConvection(const CoupledLinThMConvection& src) {
  count = src.count;
  (*count)++;
  convection = src.convection;
}

// destructor
CoupledLinThMConvection::~CoupledLinThMConvection() {
  if (--(*count) > 0) return;
  delete count;
  if (convection) delete convection;
}

// check consistency of material properties
void CoupledLinThMConvection::checkProperties(MaterialProperties& material,
					      std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\nVariational (linear) thermo-mechanical convection:" << std::endl;

  // look for algorithmic parameter
  double alpha = 0.5;
  try {
    alpha = material.getDoubleProperty("THM_ALGORITHMIC_PARAMETER");
  }
  catch (NoSuchPropertyException) {
    material.setProperty("THM_ALGORITHMIC_PARAMETER",alpha);
  }
  if (os) (*os) << "\n\talgorithmic parameter = " << alpha << std::endl;
  
  // convection part
  convection->checkProperties(material,os);
}

// update properties in function of external parameters
void CoupledLinThMConvection::updateProperties(MaterialProperties& mater,
                                            const ParameterSet& extPar) {
  convection->updateProperties(mater,extPar);
}

// self-documenting utilities
ConstitutiveModel::VariableType CoupledLinThMConvection::typeExtVar(unsigned int i) const {
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
unsigned int CoupledLinThMConvection::indexExtVar(unsigned int i) const {
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
std::string CoupledLinThMConvection::labelExtVar(unsigned int i) const {
  switch (i) {
    case 0:
      return "relative temperature difference";
      break;
    case 1:
      return "temperature increment";
      break;
    default:
      return "";
      break;
  }
}
std::string CoupledLinThMConvection::labelExtForce(unsigned int i) const {
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
void CoupledLinThMConvection::initState(const MaterialProperties& material,
					MaterialState& state) {
  ConstitutiveModel::initState(material,state);
  state.grad = 0.e0;
  state.flux = 0.e0;
  state.internal = 0.e0;
}

// compute the incremental potential
double CoupledLinThMConvection::incrementalPotential(const MaterialProperties& material,
						     const ParameterSet& extPar,
						     const MaterialState& state0,
						     MaterialState& state,
						     double dTime,MatLibMatrix& M,
						     bool update,bool tangent) 
 throw (UpdateFailedException) {

  // extract temperature and temperature gradient
  double dT = state.grad[0];
  double Th0 = state0.grad[1];
  double Th1 = state.grad[1];

  // compute jacobian and temperature for the step
  double alpha = material.getDoubleProperty("THM_ALGORITHMIC_PARAMETER");
  double coef = alpha*dTime;
  double Th = (1.0-alpha)*Th0+alpha*Th1;
  
  // compute diffusion energy
  double q,N0,K,S0,C0;
  double X = convection->diffusionEnergy(material,extPar,dT,Th,q,N0,
                                         K,S0,C0,update,tangent);
  if (update) {
    state.flux[0] = -dTime*q;
    state.flux[1] = -coef*N0;
  }
  if (tangent) {
    M[0][0] = -coef*K;
    M[1][0] = M[0][1] = -coef*S0;
    M[1][1] = -coef*C0;
  }
  
  return -dTime*X;
}


/*
 * Methods for class LinThMConductionBuilder.
 */

// the instance
LinThMConvectionBuilder const* LinThMConvectionBuilder::BUILDER 
= new LinThMConvectionBuilder();

// constructor
LinThMConvectionBuilder::LinThMConvectionBuilder() {
  ModelDictionary::add("LINEAR_VARIATIONAL_THERMO_MECHANICAL_CONVECTION",*this);
}

// build model
ConstitutiveModel* LinThMConvectionBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
    case 2:
    case 1:
      return new LinThMConvection();
      break;
    default:
      return 0;
      break;
  }
}
