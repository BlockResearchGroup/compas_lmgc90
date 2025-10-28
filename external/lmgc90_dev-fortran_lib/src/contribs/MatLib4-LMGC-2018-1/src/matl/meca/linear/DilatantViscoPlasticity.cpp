/*
 *  $Id: DilatantViscoPlasticity.cpp 244 2017-06-22 12:54:45Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "DilatantViscoPlasticity.h"

// std C library
#include <cmath>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


// constructor
EllipticViscoPlasticity::EllipticViscoPlasticity(IsotropicHardeningModel* h,
                                                 ScalarRateDependencyModel* v) {
  count = new unsigned int(1);
  hardening = h;
  viscous = v;
}

// copy constructor
EllipticViscoPlasticity::EllipticViscoPlasticity(const EllipticViscoPlasticity& src) {
  count = src.count;
  (*count)++;
  hardening = src.hardening;
  viscous = src.viscous;
}

// destructor
EllipticViscoPlasticity::~EllipticViscoPlasticity() {
  if (--(*count) > 0) return;
  delete count;
  if (hardening) delete hardening;
  if (viscous) delete viscous;
}

// check consistency of material properties
void EllipticViscoPlasticity::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Elliptic viscoplasticity model (isotropic hardening)***";

  // yield surface shape parameter
  double q = material.getDoubleProperty("PLASTIC_DILATANCY_PARAMETER");
  if (q <= 0.0e0) {
    if (os) (*os) << "ERROR: plastic dilatancy parameter must be positive" << std::endl;
    throw InvalidPropertyException("plastic dilatancy parameter");
  }
  if (os) (*os) << "\n\tplastic dilatancy parameter = " << q;

  // look for algorithmic parameter
  double alpha = 0.5;
  try {
    alpha = material.getDoubleProperty("VP_ALGORITHMIC_PARAMETER");
  }
  catch (NoSuchPropertyException) {
    material.setProperty("VP_ALGORITHMIC_PARAMETER",alpha);
  }
  if (os) (*os) << "\n\talgorithmic parameter = " << alpha << std::endl;
    
  // rate-independent part (stored and dissipated)
  if (hardening) hardening->checkProperties(material,os);
  
  // dissipated viscous (rate-dependent) part
  if (viscous) viscous->checkProperties(material,os);
}

// update properties in function of external parameters
void EllipticViscoPlasticity::updateProperties(MaterialProperties& material,const ParameterSet& extPar) {

  // rate-independent part (stored and dissipated)
  if (hardening) hardening->updateProperties(material,extPar);
  
  // dissipated viscous (rate-dependent) part
  if (viscous) viscous->updateProperties(material,extPar);
}

// number of internal parameters
unsigned int EllipticViscoPlasticity::nIntPar() const {
  unsigned int np = 2; // equiv. plastic strain + plastic stored energy
  
  // rate-independent part (stored and dissipated)
  if (hardening) np += hardening->nIntPar();
  
  // dissipated viscous (rate-dependent) part
  if (viscous) np += viscous->nIntPar();
  
  return np;
}

// compute irreversible energy and derivatives
double EllipticViscoPlasticity::irreversibleEnergy(const MaterialProperties& material,
                                                   const ParameterSet& extPar,
                                                   const MatLibArray& intPar0,MatLibArray& intPar,
                                                   double epsPlD0,double epsPlD1,
                                                   double epsPlV0,double epsPlV1,
                                                   double& sigD,double& sigV,
                                                   double& hDD,double& hDV,double& hVV,
                                                   double dTime,bool first,bool second) {
  // get initial plastic stored energy
  double Wp0,Wp;
  Wp0 = intPar0[1];
  
  // get algorithmic parameter
  double alpha = material.getDoubleProperty("VP_ALGORITHMIC_PARAMETER");
  
  // get plastic dilatancy parameter
  double q = material.getDoubleProperty("PLASTIC_DILATANCY_PARAMETER");
  double coef = 1.0e0/(q*q);

  // compute equivalent plastic strain
  double dEpsPlD = epsPlD1-epsPlD0;
  double dEpsPlV = epsPlV1-epsPlV0;
  double dEpsPl = std::sqrt(dEpsPlD*dEpsPlD+coef*dEpsPlV*dEpsPlV);
  double epsPl0 = intPar0[0];
  double epsPl1 = epsPl0+dEpsPl;
  double epsPl = epsPl0+alpha*dEpsPl;
  intPar[0] = epsPl1;
  
  // compute hardening part (stored and dissipated energy)
  unsigned int nIntParHarden = 0;
  double sig0=0.e0,h0=0.e0;
  double Dp=0.e0,sig1=0.e0,h1=0.e0;
  if (hardening) {
    nIntParHarden = hardening->nIntPar();
    const MatLibArray intP0Harden(intPar0,nIntParHarden,2);
    MatLibArray intP1Harden(intPar,nIntParHarden,2);
    // compute plastic stored energy
    Wp = hardening->storedEnergy(material,extPar,intP0Harden,intP1Harden,Wp0,
                                 epsPl0,epsPl1,sig0,h0,first || second,second);
    // compute plastic dissipated energy
    sig1 = hardening->yieldStress(material,extPar,intP0Harden,intP1Harden,
                                  epsPl,h1,first || second);
    Dp = sig1*(epsPl1-epsPl0);
  }
  else
    Wp = 0.e0;
  intPar[1] = Wp;

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
                                         first || second,second);
    double coef = alpha*dTime;
    if (first || second) sig2 = coef*siga + sigb;
    if (second) h2 =  alpha*coef*haa + 2*alpha*hab + hbb/dTime;
  }

  // assemble components
  double coefD,coefV,sig;
  if (first || second) {
    if (dEpsPl > 1.e-16) {
      coefD = dEpsPlD/dEpsPl;
      coefV = coef*dEpsPlV/dEpsPl;
    }
    else {
      coefD = 1.0e0;
      coefV = 1.0/q;
    }
    sig = sig0 + sig1+alpha*h1*(epsPl1-epsPl0) + sig2;
  }
  if (first) {
    sigD = coefD*sig;
    sigV = coefV*sig;
  }
  if (second) {
    double h = h0 + 2*alpha*h1 + h2;
    hDD = coefD*coefD*h;
    hDV = coefD*coefV*h;
    hVV = coefV*coefV*h;
    if (dEpsPl > 1.e-16) {
      double val = sig/dEpsPl;
      hDD += (1.-coefD*coefD)*val;
      hDV -= coefD*coefV*val;
      hVV += (coef-coefV*coefV)*val;
    }
  }

  return Wp-Wp0+Dp+Dv;
}

// compute steepest gradient direction (at origin)
double EllipticViscoPlasticity::steepestGradient(const MaterialProperties& material,
                                                 const ParameterSet& extPar,
                                                 const MatLibArray& intPar0,MatLibArray& intPar,
                                                 double epsPlD0,double epsPlD1,
                                                 double epsPlV0,double epsPlV1,
                                                 double sigEq,double sigVol,
                                                 double& dEpsD,double& dEpsV,double dTime) {
  // get plastic dilatancy parameter
  double q = material.getDoubleProperty("PLASTIC_DILATANCY_PARAMETER");
  
  // steepest gradient direction
  double norm = std::sqrt(sigEq*sigEq+q*q*sigVol*sigVol);
  if (norm < 1.e-16) {
    dEpsD = 1.0e0;
    dEpsV = 0.0e0;
    return 0.0e0;
  }
  double coef = 1.0e0/norm;
  dEpsD = coef*sigEq;
  dEpsV = coef*q*q*sigVol;
  
  // get algorithmic parameter
  double alpha = material.getDoubleProperty("VP_ALGORITHMIC_PARAMETER");
  
  // compute equivalent plastic strain
  double dEpsPlD = epsPlD1-epsPlD0;
  double dEpsPlV = epsPlV1-epsPlV0;
  double coef1 = 1.0/(q*q);
  double dEpsPl = std::sqrt(dEpsPlD*dEpsPlD+coef1*dEpsPlV*dEpsPlV);
  double epsPl0 = intPar0[0];
  double epsPl1 = epsPl0+dEpsPl;
  double epsPl = epsPl0+alpha*dEpsPl;

  // steepest slope
  double sig0=0.0e0,sig1=0.0e0,h1=0.0e0;
  unsigned int nIntParHarden=0;
  if (hardening) {
    nIntParHarden = hardening->nIntPar();
    const MatLibArray intP0Harden(intPar0,nIntParHarden,2);
    MatLibArray intP1Harden(intPar,nIntParHarden,2);
    // compute plastic stored energy
    double Wp0 = intPar0[1],dummy;
    hardening->storedEnergy(material,extPar,intP0Harden,intP1Harden,
                            Wp0,epsPl0,epsPl1,sig0,dummy,true,false);
    // compute plastic dissipated energy
    sig1 = hardening->yieldStress(material,extPar,intP0Harden,intP1Harden,
                                  epsPl,h1,true);
  }
  double sig2=0.0e0;
  if (viscous && dTime > 0.e0) {
    unsigned int nIntParDiss = viscous->nIntPar();
    const MatLibArray intP0Diss(intPar0,nIntParDiss,1+nIntParHarden);
    MatLibArray intP1Diss(intPar,nIntParDiss,1+nIntParHarden);
    double epsPlDot = (epsPl1-epsPl0)/dTime;
    double siga,sigb,dummy;
    viscous->dissipatedEnergy(material,extPar,intP0Diss,intP1Diss,
                              epsPl,epsPlDot,siga,sigb,dummy,dummy,
                              dummy,true,false);
    double coef = alpha*dTime;
    sig2 = coef*siga + sigb;
  }
  
  return sig0 + sig1+alpha*h1*dEpsPl + sig2;
}
