/*
 *  $Id: ContactPenalty.cpp 180 2015-09-17 18:40:22Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "ContactPenalty.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/**
 * Methods for class ContactPenalty
 */

// destructor
ContactPenalty::~ContactPenalty() {
  if (--(*count) > 0) return;
  delete count;
  if (penalty) delete penalty;
}

// check consistency of material properties
void ContactPenalty::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\nVariational contact penalty formulation (finite transformations):" << std::endl;
  
  // algorithmic paprameter
  double alpha;
  try {
    alpha = material.getDoubleProperty("CTC_ALGORITHMIC_PARAMETER");
  }
  catch (NoSuchPropertyException) {
    alpha = 0.5e0;
    material.setProperty("CTC_ALGORITHMIC_PARAMETER",alpha);
  }
  if (os) {
    (*os) << "\n\tcontact algorithmic parameter = " << alpha;
  }
  
  // check penalty potential
  penalty->checkProperties(material,os);
}

// update properties in function of external parameters
void ContactPenalty::updateProperties(MaterialProperties& material,const ParameterSet& params) {
  penalty->updateProperties(material,params);
}

// initialize the state of the material
void ContactPenalty::initState(const MaterialProperties& mater,MaterialState& state) {
  state.grad.resize(this->nExtVar());
  state.grad[0] = 1.0e0; // initial Jacobian
  state.grad[1] = 0.0e0; // initial gap
  state.flux.resize(this->nExtVar());
  state.flux = 0.0e0;    // energy and force
  state.internal.resize(this->nIntVar());
}

// compute the incremental potential
double ContactPenalty::incrementalPotential(const MaterialProperties& mater,const ParameterSet& params,
                                            const MaterialState& state0,MaterialState& state,double dTime,
                                            MatLibMatrix& M,bool update,bool tangent)
 throw (UpdateFailedException) {

  // get Jacobian
  double J0 = state0.grad[0];
  double J1 = state.grad[0];
  double alpha = mater.getDoubleProperty("CTC_ALGORITHMIC_PARAMETER");
  double J = (1.0-alpha)*J0+alpha*J1;

  // get normal gap
  double g = state.grad[1];

  // compute penalty energy and force
  double f,K;
  double W = penalty->penaltyEnergy(mater,params,g,f,K,update,tangent);

  if (update) {
    state.flux[0] = alpha*W;
    state.flux[1] = J*f;
  }
   
  // tangent
  if (tangent) {
    M[0][0] = 0.0e0;
    M[0][1] = M[1][0] = alpha*f;
    M[1][1] = K;
  }

  return J*W;
}


/**
 * Methods for standard penalty formulation.
 */

// check consistency of material properties
void StdContactPenaltyPotential::checkProperties(MaterialProperties& mater,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Normal contact (penalty formulation)***" << std::endl;

  // penalty coefficient
  double pen;
  try {
    pen = mater.getDoubleProperty("NORMAL_PENALTY_COEFFICIENT");
  }
  catch (NoSuchPropertyException) {
    pen = mater.getDoubleProperty("PENALTY_COEFFICIENT");
    mater.setProperty("NORMAL_PENALTY_COEFFICIENT",pen);
  }
  if (pen <= 0.0e0) {
    if (os) (*os) << "ERROR: penalty coefficient must be strictly positive." << std::endl;
    throw InvalidPropertyException("penalty coefficient");
  }
  
   // print-out
   if (os) (*os) << "\tpenalty coefficient = " << pen << std::endl;
}

// compute penalty potential
double StdContactPenaltyPotential::penaltyEnergy(const MaterialProperties& mater,const ParameterSet& params,
                                                 double g,double& f,double& K,bool first,bool second) {

  // case of no contact
  if (g < 0.0e0) {
    if (first) f = 0.0e0;
    if (second) K = 0.0e0;
    return 0.0e0;
  }

  // get penalty coefficient
  double pen = mater.getDoubleProperty("NORMAL_PENALTY_COEFFICIENT");
  
  // compute force and energy
  double W;
  if (first) {
    f = pen*g;
    W = 0.5*f*g;
  }
  else
    W = 0.5*pen*g*g;
  
  // compute tangent
  if (second) K = pen;
  
  return W;
}


/*
 * Methods for class StdContactPenaltyBuilder.
 */

// the instance
StdContactPenaltyBuilder const* StdContactPenaltyBuilder::BUILDER
= new StdContactPenaltyBuilder();

// constructor
StdContactPenaltyBuilder::StdContactPenaltyBuilder() {
  ModelDictionary::add("STANDARD_CONTACT_PENALTY",*this);
}

// build model
ConstitutiveModel* StdContactPenaltyBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
    case 2:
    case 1:
      return new StdContactPenalty();
      break;
    default:
      return 0;
      break;
  }
}
