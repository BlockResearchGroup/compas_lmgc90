/*
 *  $Id: TaylorModelFCC.cpp 196 2016-02-18 14:44:05Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "TaylorModelFCC.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>


/*
 * Methods for FCCHardeningTaylor
 */

// private constructor
FCCHardeningTaylor::FCCHardeningTaylor(IsotropicHardeningModel* h) {
  count = new unsigned int(1);
  hardening = h;
}

// constructor
FCCHardeningTaylor::FCCHardeningTaylor(IsotropicHardeningModel& h) {
  count = new unsigned int(1);
  hardening = &h;
}

// copy constructor
FCCHardeningTaylor::FCCHardeningTaylor(const FCCHardeningTaylor& src) {
  (*count)++;
  hardening = src.hardening;
}

// destructor
FCCHardeningTaylor::~FCCHardeningTaylor() {
  if (--(*count) > 0) return;
  delete count;
  if (hardening) delete hardening;
}

// check consistency of material properties
void FCCHardeningTaylor::checkProperties(MaterialProperties& material,
                                         std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***FCC Taylor hardening model***" << std::endl;

  // check hardening material properties
  hardening->checkProperties(material,os);
}

// plastically stored energy
double FCCHardeningTaylor::storedEnergy(const MaterialProperties& material,
                                        const ParameterSet& extPar,
                                        const MatLibArray& intPar0,
                                        MatLibArray& intPar1,double Wp0,
                                        const MatLibArray& gamma0,
                                        const MatLibArray& gamma1,
                                        MatLibArray& tau,MatLibMatrix& H,
                                        bool first,bool second) {
  // compute total cumulated slip
  double Gamma0 = 0.0e0;
  double Gamma1 = 0.0e0;
  for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) {
    Gamma0 += gamma0[k];
    Gamma1 += gamma1[k];
  }

  // compute plastic energy
  double sig,h;
  double Wp = hardening->storedEnergy(material,extPar,intPar0,intPar1,Wp0,
                                      Gamma0,Gamma1,sig,h,first,second);
  if (first) {
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) {
      tau[k] = sig;
    }
  }
  if (second) {
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++)
      for (unsigned int l=0; l < SingleCrystalFCC::NSYS; l++) {
        H[k][l] = h;
    }
  }
  
  return Wp;
}

// yield stress
void FCCHardeningTaylor::yieldStress(const MaterialProperties& material,
                                     const ParameterSet& extPar,
                                     const MatLibArray& intPar0,
                                     MatLibArray& intPar1,
                                     const MatLibArray& gamma0,
                                     const MatLibArray& gamma1,
                                     const MatLibArray& gamma,
                                     MatLibArray& tau,MatLibMatrix& H,
                                     std::vector<MatLibMatrix>& dH,
                                     bool first,bool second) {
  // compute total cumulated slip
  double Gamma = 0.0e0;
  for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) {
    Gamma += gamma[k];
  }
  
  // get yield stress
  double h;
  double sig = hardening->yieldStress(material,extPar,intPar0,intPar1,
                                      Gamma,h,first);

  // compute CRSS on all systems
  for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) {
      tau[k] = sig;
  }
  
  // hardening moduli
  if (first) {
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++)
      for (unsigned int l=0; l < SingleCrystalFCC::NSYS; l++) {
        H[k][l] = h;
      }
  }
  if (second) { // neglect second derivatives
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++)
      dH[k] = 0.0e0;
  }
}


/*
 * Methods for class LinearCrystalHEPlasticityFCCBuilder.
 */

// the instance
LinearCrystalHEPlasticityFCCBuilder const* LinearCrystalHEPlasticityFCCBuilder::BUILDER
= new LinearCrystalHEPlasticityFCCBuilder();

// constructor
LinearCrystalHEPlasticityFCCBuilder::LinearCrystalHEPlasticityFCCBuilder() {
  ModelDictionary::add("FCC_CRYSTAL_FINITE_PLASTICITY_LINEAR",*this);
}

// build model
ConstitutiveModel* LinearCrystalHEPlasticityFCCBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearCrystalHEPlasticityFCC3D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class LinearCrystalPlasticityFCCBuilder.
 */

// the instance
LinearCrystalPlasticityFCCBuilder const* LinearCrystalPlasticityFCCBuilder::BUILDER
= new LinearCrystalPlasticityFCCBuilder();

// constructor
LinearCrystalPlasticityFCCBuilder::LinearCrystalPlasticityFCCBuilder() {
  ModelDictionary::add("FCC_CRYSTAL_PLASTICITY_LINEAR",*this);
}

// build model
ConstitutiveModel* LinearCrystalPlasticityFCCBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearCrystalPlasticityFCC3D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class NonLinearCrystalHEPlasticityFCCBuilder.
 */

// the instance
NonLinearCrystalHEPlasticityFCCBuilder const* NonLinearCrystalHEPlasticityFCCBuilder::BUILDER
= new NonLinearCrystalHEPlasticityFCCBuilder();

// constructor
NonLinearCrystalHEPlasticityFCCBuilder::NonLinearCrystalHEPlasticityFCCBuilder() {
  ModelDictionary::add("FCC_CRYSTAL_FINITE_PLASTICITY_NONLINEAR",*this);
}

// build model
ConstitutiveModel* NonLinearCrystalHEPlasticityFCCBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NonLinearCrystalHEPlasticityFCC3D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class NonLinearCrystalPlasticityFCCBuilder.
 */

// the instance
NonLinearCrystalPlasticityFCCBuilder const* NonLinearCrystalPlasticityFCCBuilder::BUILDER
= new NonLinearCrystalPlasticityFCCBuilder();

// constructor
NonLinearCrystalPlasticityFCCBuilder::NonLinearCrystalPlasticityFCCBuilder() {
  ModelDictionary::add("FCC_CRYSTAL_PLASTICITY_NONLINEAR",*this);
}

// build model
ConstitutiveModel* NonLinearCrystalPlasticityFCCBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NonLinearCrystalPlasticityFCC3D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class LinearPolyCrystalHEPlasticityFCCBuilder.
 */

// the instance
LinearPolyCrystalHEPlasticityFCCBuilder const* LinearPolyCrystalHEPlasticityFCCBuilder::BUILDER
= new LinearPolyCrystalHEPlasticityFCCBuilder();

// constructor
LinearPolyCrystalHEPlasticityFCCBuilder::LinearPolyCrystalHEPlasticityFCCBuilder() {
  ModelDictionary::add("FCC_POLY_CRYSTAL_FINITE_PLASTICITY_LINEAR",*this);
}

// build model
ConstitutiveModel* LinearPolyCrystalHEPlasticityFCCBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new LinearPolyCrystalHEPlasticityFCC3D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class NonLinearPolyCrystalHEPlasticityFCCBuilder.
 */

// the instance
NonLinearPolyCrystalHEPlasticityFCCBuilder const* NonLinearPolyCrystalHEPlasticityFCCBuilder::BUILDER
= new NonLinearPolyCrystalHEPlasticityFCCBuilder();

// constructor
NonLinearPolyCrystalHEPlasticityFCCBuilder::NonLinearPolyCrystalHEPlasticityFCCBuilder() {
  ModelDictionary::add("FCC_POLY_CRYSTAL_FINITE_PLASTICITY_NONLINEAR",*this);
}

// build model
ConstitutiveModel* NonLinearPolyCrystalHEPlasticityFCCBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new NonLinearPolyCrystalHEPlasticityFCC3D();
      break;
    default:
      return 0;
      break;
  }
}
