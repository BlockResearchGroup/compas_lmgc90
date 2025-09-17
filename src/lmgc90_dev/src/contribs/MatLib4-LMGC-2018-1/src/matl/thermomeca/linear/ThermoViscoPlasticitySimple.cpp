/*
 *  $Id: ThermoViscoPlasticitySimple.cpp 142 2014-02-07 12:51:54Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "ThermoViscoPlasticitySimple.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


// constructor
StdThermoViscoPlasticitySimple::StdThermoViscoPlasticitySimple(ThermalIsotropicHardeningModel* h,
                                                               ThermalScalarRateDependencyModel* v) {
  count = new unsigned int(1);
  hardening = h;
  viscous = v;
}

// copy constructor
StdThermoViscoPlasticitySimple::StdThermoViscoPlasticitySimple(const StdThermoViscoPlasticitySimple& src) {
  count = src.count;
  (*count)++;
  hardening = src.hardening;
  viscous = src.viscous;
}

// destructor
StdThermoViscoPlasticitySimple::~StdThermoViscoPlasticitySimple() {
  if (--(*count) > 0) return;
  delete count;
  if (hardening) delete hardening;
  if (viscous) delete viscous;
}

// check consistency of material properties
void StdThermoViscoPlasticitySimple::checkProperties(MaterialProperties& material,std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Standard thermoviscoplasticity model (isotropic hardening)***" << std::endl;
  
  // look for algorithmic parameter
  double alpha = 0.5;
  try {
    alpha = material.getDoubleProperty("TVP_ALGORITHMIC_PARAMETER");
  }
  catch (NoSuchPropertyException) {
    material.setProperty("TVP_ALGORITHMIC_PARAMETER",alpha);
  }
  if (os) (*os) << "\talgorithmic parameter = " << alpha << std::endl;
  
  // rate-independent part (stored and dissipated)
  if (hardening) hardening->checkProperties(material,os);
  
  // dissipated viscous (rate-dependent) part
  if (viscous) viscous->checkProperties(material,os);
}

// update properties in function of external parameters
void StdThermoViscoPlasticitySimple::updateProperties(MaterialProperties& material,
                                                      const ParameterSet& extPar) {
  // rate-independent part (stored and dissipated)
  if (hardening) hardening->updateProperties(material,extPar);
  
  // dissipated viscous (rate-dependent) part
  if (viscous) viscous->updateProperties(material,extPar);
}

// number of internal parameters
unsigned int StdThermoViscoPlasticitySimple::nIntPar() const {
  unsigned int np = 2; // plastic stored energy + heat fraction
  
  // rate-independent part (stored and dissipated)
  if (hardening) np += hardening->nIntPar();
  
  // dissipated viscous (rate-dependent) part
  if (viscous) np += viscous->nIntPar();
  
  return np;
}

// compute irreversible energy and derivatives
double StdThermoViscoPlasticitySimple::irreversibleEnergy(const MaterialProperties& material,
                                                          const ParameterSet& extPar,
                                                          const MatLibArray& intPar0,MatLibArray& intPar,
                                                          double epsPl0,double epsPl1,double Th0,double Th1,
                                                          double T0,double T1,double& sig,double& Np,
                                                          double& dNp,double& h,double& dSig,double& C,
                                                          double dTime,bool first,bool second) {
  // get initial plastic stored energy
  double Wp0,Wp;
  Wp0 = intPar0[0];
  
  // get algorithmic parameter
  double alpha = material.getDoubleProperty("TVP_ALGORITHMIC_PARAMETER");
  double epsPl = (1.0-alpha)*epsPl0+alpha*epsPl1;
  double Th = (1.0-alpha)*Th0+alpha*Th1;
  double dT = Th1-Th0;
  
  // get temperature ratio(s)
  double ratio1 = dT/T0;
  double ratio0 = T1/T0;
  double ratio2 = dT/T1;
  double ratio3 = T0/T1;
  
  // compute hardening part (stored and dissipated energy)
  unsigned int nIntParHarden = 0;
  double sig0=0.e0,h0=0.e0,dSig0=0.e0,C0=0.e0,dummy;
  double Dp=0.e0,sig1a=0.e0,sig1b=0.e0,h1a=0.e0,h1b=0.e0,dSig1=0.e0;
  if (hardening) {
    nIntParHarden = hardening->nIntPar();
    const MatLibArray intP0Harden(intPar0,nIntParHarden,2);
    MatLibArray intP1Harden(intPar,nIntParHarden,2);
    // compute plastic stored energy
    Wp = hardening->storedThMEnergy(material,extPar,intP0Harden,intP1Harden,
                                    Wp0,epsPl0,epsPl1,Th1,sig0,Np,h0,dSig0,C0,
                                    first,second);
    // compute plastic dissipated energy
    sig1a = hardening->yieldThMStress(material,extPar,intP0Harden,intP1Harden,
                                      epsPl,Th0,h1a,dummy,first || second);
    sig1b = hardening->yieldThMStress(material,extPar,intP0Harden,intP1Harden,
                                      epsPl,Th,h1b,dSig1,first || second);
    Dp = (sig1a+ratio1*sig1b)*(epsPl1-epsPl0);
  }
  else {
    Wp = 0.e0;
    if (first) Np = 0.e0;
  }
  intPar[0] = Wp;

  // compute viscous (rate-dependent) dissipated energy
  unsigned int nIntParDiss = 0;
  double Dv=0.e0,sig2=0.e0,N2=0.e0,h2=0.e0,dSig2=0.e0,C2=0.e0;
  if (viscous && dTime > 0.e0) {
    nIntParDiss = viscous->nIntPar();
    const MatLibArray intP0Diss(intPar0,nIntParDiss,2+nIntParHarden);
    MatLibArray intP1Diss(intPar,nIntParDiss,2+nIntParHarden);
    double epsPlDot = ratio0*(epsPl1-epsPl0)/dTime;
    double Dv1,Dv2,sigv1a,sigv2a,sigv1b,sigv2b,sigc;
    double hv1aa,hv2aa,hv1bb,hv2bb,hcc,hv1ab,hv2ab,hac,hbc;
    Dv1 = dTime*viscous->dissipatedThMEnergy(material,extPar,intP0Diss,intP1Diss,
                                             epsPl,epsPlDot,Th0,sigv1a,sigv1b,sigc,
                                             hv1aa,hv1bb,hcc,hv1ab,hac,hbc,
                                             first || second,second);
    Dv2 = dTime*viscous->dissipatedThMEnergy(material,extPar,intP0Diss,intP1Diss,
                                             epsPl,epsPlDot,Th,sigv2a,sigv2b,sigc,
                                             hv2aa,hv2bb,hcc,hv2ab,hac,hbc,
                                             first || second,second);
    Dv = ratio3*Dv1+ratio2*Dv2;
    double coef = alpha*dTime;
    if (first) {
      double val = sigv1b+ratio1*sigv2b;
      sig2 = coef*(ratio3*sigv1a+ratio2*sigv2a) + val;
      N2 = (val*(epsPl1-epsPl0) + coef*dT*sigc + dTime*ratio3*(Dv2-Dv1))/T1;
    }
    if (second) {
      h2 =  alpha*coef*(ratio3*hv1aa+ratio2*hv2aa) + 2*alpha*(hv1ab+ratio1*hv2ab) 
          + ratio0*(hv1bb+ratio1*hv2bb)/dTime;
      dSig2 =  alpha*ratio2*(coef*hac + ratio0*hbc) 
             + (coef*(hv1ab+ratio1*hv2ab) + ratio0*(hv1bb+ratio1*hv2bb))*epsPlDot*ratio3/T1
             + sigv2b/T0 + coef*ratio3*(sigv2a-sigv1a)/T1;
      C2 =  coef*ratio2*(2*hbc*epsPlDot/T1 + alpha*hcc)
          + 2*dTime*(alpha*sigc - (Dv2-Dv1)/T1)*ratio3/T1
          + ((hv1bb+ratio1*hv2bb)*epsPlDot + 2*(sigv2b-sigv1b))*(epsPl1-epsPl0)/(T1*T1);
    }
  }

  // assemble components
  if (first) {
    sig = sig0 + sig1a+ratio1*sig1b+alpha*(h1a+ratio1*h1b)*(epsPl1-epsPl0) + sig2;
    dNp = Np + (sig1b/T0+alpha*ratio1*dSig1)*(epsPl1-epsPl0) + N2;
  }
  if (second) {
    h = h0 + 2*alpha*(h1a+ratio1*h1b) + h2;
    dSig = dSig0 + alpha*ratio1*dSig1+(sig1b+alpha*h1b*(epsPl1-epsPl0))/T0 + dSig2;
    C = C0 + 2*alpha*dSig1*(epsPl1-epsPl0)/T0 + C2;
  }
  
  // compute heat fraction
  if (first) {
    double beta = 1.0e0;
    if (sig > 1.e-16) beta = (sig-sig0)/sig;
    /*double Dt = (sig-sig0)*(epsPl1-epsPl0); // this version includes entropic effects
    double Wt = Wp-Wp0+Dt;
    if (Wt > 1.e-16) beta = Dt/Wt;*/
    intPar[1] = beta;
  }

  return Wp-Wp0+Dp+Dv;
}
