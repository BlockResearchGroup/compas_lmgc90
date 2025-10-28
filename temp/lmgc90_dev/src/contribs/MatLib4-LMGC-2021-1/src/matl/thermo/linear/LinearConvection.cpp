/*
 *  $Id: LinearConvection.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "LinearConvection.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class LinearConvection
 */

// check consistency of material properties
void LinearConvection::checkProperties(MaterialProperties& material,
                                       std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\nLinear classical convection:" << std::endl;
  
  // exchange coefficient
  double h = material.getDoubleProperty("EXCHANGE_COEFFICIENT");
  if (h < 0.e0) {
    if (os) (*os) << "ERROR: convection exchange coefficient must be positive.\n";
    throw InvalidPropertyException("convection exchange coefficient");
  }
  
  if (os) {
    (*os) << "\n\texchange coefficient = " <<  h << std::endl;
  }
}

// self-documenting utilities
ConstitutiveModel::VariableType LinearConvection::typeExtVar(unsigned int i) const {
  switch (i) {
    case 0:
      return ConstitutiveModel::TYPE_SCALAR;
      break;
    default:
      return ConstitutiveModel::TYPE_NONE;
      break;
  }
}
unsigned int LinearConvection::indexExtVar(unsigned int i) const {
  switch (i) {
    case 0:
      return 0;
      break;
    default:
      return 1;
      break;
  }
}
std::string LinearConvection::labelExtVar(unsigned int i) const {
  switch (i) {
    case 0:
      return "temperature difference";
      break;
    default:
      return "";
      break;
  }
}
std::string LinearConvection::labelExtForce(unsigned int i) const {
  switch (i) {
    case 0:
      return "heat flux";
      break;
    default:
      return "";
      break;
  }
}

// initialize the state of the material
void LinearConvection::initState(const MaterialProperties& material,
                                 MaterialState& state) {
  ConstitutiveModel::initState(material,state);
  state.grad = 0.e0;
  state.flux = 0.e0;
  state.internal = 0.e0;
}

// compute the incremental potential
double LinearConvection::incrementalPotential(const MaterialProperties& material,
                                              const ParameterSet& extPar,
                                              const MaterialState& state0,
                                              MaterialState& state,
                                              double dTime,MatLibMatrix& M,
                                              bool update,bool tangent) 
 throw (UpdateFailedException) {
  
  // extract temperature difference
  double dT = state.grad[0];
  
  // get exchange coefficient
  double h = material.getDoubleProperty("EXCHANGE_COEFFICIENT");
  
  // compute diffusion energy
  double X;
  if (update) {
    double q = h*dT;
    X = 0.5*q*dT;
    state.flux[0] = q;
  }
  else
    X = 0.5*h*dT*dT;
  
  if (tangent) M[0][0] = h;
  
  return X;
}


/*
 * Methods for class LinearConvectionBuilder.
 */

// the instance
LinearConvectionBuilder const* LinearConvectionBuilder::BUILDER 
= new LinearConvectionBuilder();

// constructor
LinearConvectionBuilder::LinearConvectionBuilder() {
  ModelDictionary::add("LINEAR_CONVECTION",*this);
}

// build model
ConstitutiveModel* LinearConvectionBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
    case 2:
    case 1:
      return new LinearConvection();
      break;
    default:
      return 0;
      break;
  }
}
