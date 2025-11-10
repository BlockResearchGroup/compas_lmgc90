/*
 *  $Id: MaterialModel.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "MaterialModel.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


// constructor
MaterialModel::MaterialModel(ConstitutiveModel& m,
                             MaterialProperties& p) {
  modl = &m;
  prop = new MaterialProperties(p);
}

// copy constructor
MaterialModel::MaterialModel(const MaterialModel& src) {
  modl = src.modl;
  prop = new MaterialProperties(*(src.prop));
}

// destructor
MaterialModel::~MaterialModel() {
  delete prop;
}

// assignment operator
MaterialModel& MaterialModel::operator=(const MaterialModel& src) {
  if (&src == this) return *this;
  modl = src.modl;
  prop = new MaterialProperties(*(src.prop));
  return *this;  
}

// set material properties
void MaterialModel::setProperties(MaterialProperties& p) {
  delete prop;
  prop = new MaterialProperties(p);
}

// initialize material model
void MaterialModel::initialize(std::ostream* out)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  // check validity of properties
  modl->checkProperties(*prop,out);
}
void MaterialModel::initialize(const char* file)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  // check validity of properties
  modl->checkProperties(*prop,file);
}
  
// rotate material properties
void MaterialModel::rotateProperties(const Rotation& R) {
  modl->rotateProperties(*prop,R);
}

// update material properties
void MaterialModel::updateProperties(const ParameterSet& extPar) {
  modl->updateProperties(*prop,extPar);
}

// initialize the state of the material
void MaterialModel::initState(MaterialState& state) {
  modl->initState(*prop,state);
}

// update the state of the material (with the ability to compute tangents)
void MaterialModel::updateState(const ParameterSet& extPar,
                                const MaterialState& state0,MaterialState& state1,
                                double dTime,MatLibMatrix& M,bool tangent)
 throw (UpdateFailedException) {
  modl->updateState(*prop,extPar,state0,state1,dTime,M,tangent);
}

// compute material tangents (without updating)
void MaterialModel::computeTangent(const ParameterSet& extPar,
                                   const MaterialState& state0,MaterialState& state1,
                                   double dTime,MatLibMatrix& M) {
  modl->computeTangent(*prop,extPar,state0,state1,dTime,M);
}
