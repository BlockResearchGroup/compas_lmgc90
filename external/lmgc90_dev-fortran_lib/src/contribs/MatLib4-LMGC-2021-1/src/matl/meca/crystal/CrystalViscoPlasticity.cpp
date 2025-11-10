/*
 *  $Id: CrystalViscoPlasticity.cpp 202 2016-03-31 11:51:40Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "CrystalViscoPlasticity.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


// constructor
StdCrystalViscoPlasticity::StdCrystalViscoPlasticity(CrystalHardeningModel* h,
                                                     CrystalRateDependencyModel* v) {
  count = new unsigned int(1);
  hardening = h;
  viscous = v;
}

// copy constructor
StdCrystalViscoPlasticity::StdCrystalViscoPlasticity(const StdCrystalViscoPlasticity& src) {
  count = src.count;
  (*count)++;
  hardening = src.hardening;
  viscous = src.viscous;
}

// destructor
StdCrystalViscoPlasticity::~StdCrystalViscoPlasticity() {
  if (--(*count) > 0) return;
  delete count;
  if (hardening) delete hardening;
  if (viscous) delete viscous;
}

// check consistency of material properties
void StdCrystalViscoPlasticity::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Standard crystal viscoplasticity model***" << std::endl;
   
  // look for algorithmic parameter
  double alpha = 0.5;
  try {
    alpha = material.getDoubleProperty("VP_ALGORITHMIC_PARAMETER");
  }
  catch (NoSuchPropertyException) {
    material.setProperty("VP_ALGORITHMIC_PARAMETER",alpha);
  }
  if (os) (*os) << "\talgorithmic parameter = " << alpha << std::endl;

  // rate-independent part (stored and dissipated)
  if (hardening) hardening->checkProperties(material,os);

  // dissipated viscous (rate-dependent) part
  if (viscous) viscous->checkProperties(material,os);
}

// update properties in function of external parameters
void StdCrystalViscoPlasticity::updateProperties(MaterialProperties& material,
                                                 const ParameterSet& extPar) {
  
  // rate-independent part (stored and dissipated)
  if (hardening) hardening->updateProperties(material,extPar);
  
  // dissipated viscous (rate-dependent) part
  if (viscous) viscous->updateProperties(material,extPar);
}

// number of additional internal parameters
unsigned int StdCrystalViscoPlasticity::nIntPar() const {
  unsigned int np = 1; // plastic stored energy
  
  // rate-independent part (stored and dissipated)
  if (hardening) np += hardening->nIntPar();
  
  // dissipated viscous (rate-dependent) part
  if (viscous) np += viscous->nIntPar();
  
  return np;
}

// compute plastic potential and derivatives
double StdCrystalViscoPlasticity::irreversibleEnergy(const MaterialProperties& material,
                                                     const ParameterSet& extPar,
                                                     const MatLibArray& intPar0,
                                                     MatLibArray& intPar1,
                                                     const MatLibArray& gamma0,
                                                     const MatLibArray& gamma1,
                                                     MatLibArray& tau,MatLibMatrix& H,
                                                     double dTime,bool first,bool second) {
  // get initial plastic stored energy
  double Wp0,Wp;
  Wp0 = intPar0[0];
  
  // get algorithmic parameter
  unsigned int nSys = gamma1.size();
  double alpha = material.getDoubleProperty("VP_ALGORITHMIC_PARAMETER");
  MatLibArray gamma(nSys),dGamma(nSys);
  dGamma = gamma1-gamma0;
  gamma = gamma0+alpha*dGamma;
  
  // compute hardening part (stored and dissipated energy)
  unsigned int nIntParHarden = 0;
  MatLibArray tau0(nSys),tau1(nSys);
  MatLibMatrix H0(nSys),H1(nSys);
  std::vector<MatLibMatrix> dH1(nSys,H1);
  double Dp=0.e0;
  if (hardening) {
    hardening->initialize = initialize;
    hardening->finalize   = finalize;
    nIntParHarden = hardening->nIntPar();
    const MatLibArray intP0Harden(intPar0,nIntParHarden,1);
    MatLibArray intP1Harden(intPar1,nIntParHarden,1);
    // compute plastic stored energy
    Wp = hardening->storedEnergy(material,extPar,intP0Harden,intP1Harden,
                                 Wp0,gamma0,gamma1,tau0,H0,first,second);
    // compute plastic dissipated energy
    hardening->yieldStress(material,extPar,intP0Harden,intP1Harden,
                           gamma0,gamma1,gamma,tau1,H1,dH1,
                           first || second,second);
    Dp = innerProd(tau1,dGamma);
  }
  else {
    Wp = 0.0e0;
    tau0 = 0.0e0;
    tau1 = 0.0e0;
    H0 = 0.0e0;
    H1 = 0.0e0;
    for (unsigned int k=0; k < nSys; k++) dH1[k] = 0.0e0;
  }
  intPar1[0] = Wp;
  
  // compute viscous (rate-dependent) dissipated energy
  unsigned int nIntParDiss = 0;
  double Dv=0.e0;
  MatLibArray tau2(nSys);
  MatLibMatrix H2(nSys);
  if (viscous && dTime > 0.e0) {
    viscous->initialize = initialize;
    viscous->finalize   = finalize;
    nIntParDiss = viscous->nIntPar();
    const MatLibArray intP0Diss(intPar0,nIntParDiss,1+nIntParHarden);
    MatLibArray intP1Diss(intPar1,nIntParDiss,1+nIntParHarden);
    MatLibArray gamDot(nSys);
    gamDot = dGamma/dTime;
    MatLibArray siga(nSys),sigb(nSys);
    MatLibMatrix haa(nSys),hbb(nSys),hab(nSys);
    Dv = dTime*viscous->dissipatedEnergy(material,extPar,intP0Diss,intP1Diss,
                                         gamma,gamDot,siga,sigb,haa,hbb,hab,
                                         first,second);
    double coef = alpha*dTime;
    if (first) tau2 = coef*siga + sigb;
    if (second) H2 =  (alpha*coef)*haa + (2*alpha)*hab + hbb/dTime;
  }
  else {
    tau2 = 0.0e0;
    H2 = 0.0e0;
  }
  
  // assemble components
  if (first) {
    for (unsigned int k=0; k < nSys; k++) {
      double val=0.0e0;
      for (unsigned int l=0; l < nSys; l++) val += H1[l][k]*dGamma[l];
      tau[k] = tau0[k] + tau1[k]+alpha*val + tau2[k];
    }
  }
  if (second) {
    for (unsigned int k=0; k < nSys; k++) {
      for (unsigned int l=0; l < nSys; l++) {
        double val=0.0e0;
        for (unsigned int m=0; m < nSys; m++) val += dH1[m][k][l]*dGamma[m];
        H[k][l] = H0[k][l] + alpha*(H1[k][l]+H1[l][k]+alpha*val) + H2[k][l];
      }
    }
  }
  
  // components have been initialized
  if (initialize) initialize = false;

  return Wp-Wp0+Dp+Dv;
}
