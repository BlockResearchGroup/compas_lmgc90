/*
 *  $Id: OptiProblem.cpp 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "OptiProblem.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/**
 * Methods for class BasicOptiProblem.
 */

// constructor
BasicOptiProblem::BasicOptiProblem(const OptiFunction& objF,
                                   unsigned int nLow,const unsigned int iLow[],
                                   const double vLow[],
                                   unsigned int nUpp,const unsigned int iUpp[],
                                   const double vUpp[],
                                   unsigned int nEqua,const OptiFunction *cEqua[],
                                   unsigned int nIneq,const OptiFunction *cIneq[]) {

  // objective function
  objective = objF.clone();

  // lower bounds
  if (nLow > 0) {
    idxLower.insert(idxLower.end(),iLow,iLow+nLow);
    valLower.insert(valLower.end(),vLow,vLow+nLow);
  }

  // upper bounds
  if (nUpp > 0) {
    idxUpper.insert(idxUpper.end(),iUpp,iUpp+nUpp);
    valUpper.insert(valUpper.end(),vUpp,vUpp+nUpp);
  }

  // equality constraints
  for (unsigned int n=0; n < nEqua; n++) (this->cEqua).push_back(cEqua[n]->clone());

  // inequality constraints
  for (unsigned int n=0; n < nIneq; n++) (this->cIneq).push_back(cIneq[n]->clone());
}

// copy constructor
BasicOptiProblem::BasicOptiProblem(const BasicOptiProblem& src) {

  // objective function
  objective = (src.objective)->clone();

  // lower bounds
  idxLower = src.idxLower;
  valLower = src.valLower;

  // upper bounds
  idxUpper = src.idxUpper;
  valUpper = src.valUpper;

  // equality constraints
  for (unsigned int n=0; n < src.cEqua.size(); n++)
    cEqua.push_back(src.cEqua[n]->clone());

  // inequality constraints
  for (unsigned int n=0; n < src.cIneq.size(); n++)
    cIneq.push_back(src.cIneq[n]->clone());
}

// destructor
BasicOptiProblem::~BasicOptiProblem() {
  
  // objective function
  if (objective) delete objective;
  
  // equality constraints
  for (unsigned int n=0; n < cEqua.size(); n++) if (cEqua[n]) delete cEqua[n];
  
  // inequality constraints
  for (unsigned int n=0; n < cIneq.size(); n++) if (cIneq[n]) delete cIneq[n];
}

// set lower bounds on design space
void BasicOptiProblem::setLowerBounds(unsigned int n,const std::vector<unsigned int> idx,
                                      const std::vector<double> val) {
  // copy values
  idxLower = idx;
  valLower = val;

  // impose size
  idxLower.resize(n);
  valLower.resize(n);
}

// set upper bounds on design space
void BasicOptiProblem::setUpperBounds(unsigned int n,const std::vector<unsigned int> idx,
                                      const std::vector<double> val) {
  // copy values
  idxUpper = idx;
  valUpper = val;

  // impose size
  idxUpper.resize(n);
  valUpper.resize(n);
}

// set equality constraints
void BasicOptiProblem::setEqualityConstraints(unsigned int nEqua,
                                              const std::vector<const OptiFunction*> c) {
  // clear values
  for (unsigned int n=0; n < nEqua; n++) if (cEqua[n]) delete cEqua[n];

  // impose size
  cEqua.resize(nEqua);
  
  // copy values
  for (unsigned int n=0; n < nEqua; n++) cEqua[n] = c[n]->clone();
}

// set inequality constraints
void BasicOptiProblem::setInequalityConstraints(unsigned int nIneq,
                                                const std::vector<const OptiFunction*> c) {
  // clear values
  for (unsigned int n=0; n < nIneq; n++) if (cIneq[n]) delete cIneq[n];
  
  // impose size
  cIneq.resize(nIneq);
  
  // copy values
  for (unsigned int n=0; n < nIneq; n++) cIneq[n] = c[n]->clone();
}
