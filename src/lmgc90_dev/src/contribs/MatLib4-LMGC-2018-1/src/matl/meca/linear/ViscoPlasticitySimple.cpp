/*
 *  $Id: ViscoPlasticitySimple.cpp 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "ViscoPlasticitySimple.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


// constructor
StdViscoPlasticitySimple::StdViscoPlasticitySimple(IsotropicHardeningModel* h,
                                                   ScalarRateDependencyModel* v) {
  count = new unsigned int(1);
  hardening = h;
  viscous = v;
}

// copy constructor
StdViscoPlasticitySimple::StdViscoPlasticitySimple(const StdViscoPlasticitySimple& src) {
  count = src.count;
  (*count)++;
  hardening = src.hardening;
  viscous = src.viscous;
}

// destructor
StdViscoPlasticitySimple::~StdViscoPlasticitySimple() {
  if (--(*count) > 0) return;
  delete count;
  if (hardening) delete hardening;
  if (viscous) delete viscous;
}

// check consistency of material properties
void StdViscoPlasticitySimple::checkProperties(MaterialProperties& material,std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Standard viscoplasticity model (isotropic hardening)***" << std::endl;
  
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
void StdViscoPlasticitySimple::updateProperties(MaterialProperties& material,const ParameterSet& extPar) {

  // rate-independent part (stored and dissipated)
  if (hardening) hardening->updateProperties(material,extPar);
  
  // dissipated viscous (rate-dependent) part
  if (viscous) viscous->updateProperties(material,extPar);
}

// number of internal parameters
unsigned int StdViscoPlasticitySimple::nIntPar() const {
  unsigned int np = 1; // plastic stored energy
  
  // rate-independent part (stored and dissipated)
  if (hardening) np += hardening->nIntPar();
  
  // dissipated viscous (rate-dependent) part
  if (viscous) np += viscous->nIntPar();
  
  return np;
}

// compute irreversible energy and derivatives
double StdViscoPlasticitySimple::irreversibleEnergy(const MaterialProperties& material,
                                                    const ParameterSet& extPar,
                                                    const MatLibArray& intPar0,MatLibArray& intPar,
                                                    double epsPl0,double epsPl1,double& sig,double& h,
                                                    double dTime,bool first,bool second) {
  // get initial plastic stored energy
  double Wp0,Wp;
  Wp0 = intPar0[0];
  
  // get algorithmic parameter
  double alpha = material.getDoubleProperty("VP_ALGORITHMIC_PARAMETER");
  double epsPl = (1.0-alpha)*epsPl0+alpha*epsPl1;
  
  // compute hardening part (stored and dissipated energy)
  unsigned int nIntParHarden = 0;
  double sig0=0.e0,h0=0.e0;
  double Dp=0.e0,sig1=0.e0,h1=0.e0;
  if (hardening) {
    nIntParHarden = hardening->nIntPar();
    const MatLibArray intP0Harden(intPar0,nIntParHarden,1);
    MatLibArray intP1Harden(intPar,nIntParHarden,1);
    // compute plastic stored energy
    Wp = hardening->storedEnergy(material,extPar,intP0Harden,intP1Harden,
                                 Wp0,epsPl0,epsPl1,sig0,h0,first,second);
    // compute plastic dissipated energy
    sig1 = hardening->yieldStress(material,extPar,intP0Harden,intP1Harden,
                                  epsPl,h1,first || second);
    Dp = sig1*(epsPl1-epsPl0);
  }
  else
    Wp = 0.e0;
  intPar[0] = Wp;

  // compute viscous (rate-dependent) dissipated energy
  unsigned int nIntParDiss = 0;
  double Dv=0.e0,sig2=0.e0,h2=0.e0;
  if (viscous && dTime > 0.e0) {
    nIntParDiss = viscous->nIntPar();
    const MatLibArray intP0Diss(intPar0,nIntParDiss,1+nIntParHarden);
    MatLibArray intP1Diss(intPar,nIntParDiss,1+nIntParHarden);
    double epsPlDot = (epsPl1-epsPl0)/dTime;
    double siga,sigb,haa,hbb,hab;
    Dv = dTime*viscous->dissipatedEnergy(material,extPar,intP0Diss,intP1Diss,
                                         epsPl,epsPlDot,siga,sigb,haa,hbb,hab,
                                         first,second);
    double coef = alpha*dTime;
    if (first) sig2 = coef*siga + sigb;
    if (second) h2 =  alpha*coef*haa + 2*alpha*hab + hbb/dTime;
  }

  // assemble components
  if (first) sig = sig0 + sig1+alpha*h1*(epsPl1-epsPl0) + sig2;
  if (second) h = h0 + 2*alpha*h1 + h2;

  return Wp-Wp0+Dp+Dv;
}
