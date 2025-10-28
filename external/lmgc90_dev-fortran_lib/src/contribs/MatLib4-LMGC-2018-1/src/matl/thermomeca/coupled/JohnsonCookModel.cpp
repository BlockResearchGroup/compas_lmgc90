/*
 *  $Id: JohnsonCookModel.cpp 153 2014-10-03 09:12:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "JohnsonCookModel.h"

// std C library
#include <cmath>
// std C++ library
#include <limits>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


// check consistency of material properties
void JohnsonCookModel::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Johnson-Cook thermoviscoplasticity model***" << std::endl;
  
  // look for algorithmic parameter
  double alpha = 0.5;
  try {
    alpha = material.getDoubleProperty("TVP_ALGORITHMIC_PARAMETER");
  }
  catch (NoSuchPropertyException) {
    material.setProperty("TVP_ALGORITHMIC_PARAMETER",alpha);
  }
  if (os) (*os) << "\talgorithmic parameter = " << alpha << std::endl;
   
  // reference temperature
  double T0;
  try {
    T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    if (T0 <= 0.e0) {
      if (os) (*os) << "ERROR: reference temperature must be strictly positive." << std::endl;
      throw InvalidPropertyException("reference temperature");
    }
  }
  catch (NoSuchPropertyException) {
    // use initial temperature
    try {
      T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
      if (T0 <= 0.e0) {
        if (os) (*os) << "ERROR: initial temperature must be strictly positive." << std::endl;
        throw InvalidPropertyException("initial temperature");
      }
      material.setProperty("REFERENCE_TEMPERATURE",T0);
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: reference temperature cannot be set." << std::endl;
      throw e;
    }
  }
  
  /*
   * stored part
   */
  double sig0,b0,n0=1.0e0;
  // initial yield stress
  try {
    sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
  }
  catch (NoSuchPropertyException) {
    sig0 = 0.0e0;
    material.setProperty("INITIAL_YIELD_STRESS_STORED",sig0);
  }
  // hardening coefficient
  try {
    b0 = material.getDoubleProperty("HARDENING_COEFFICIENT_STORED");
  }
  catch (NoSuchPropertyException) {
    b0 = 0.0e0;
    material.setProperty("HARDENING_COEFFICIENT_STORED",b0);
  }
  // hardening exponent
  if (b0 != 0.0e0) {
    try {
      n0 = material.getDoubleProperty("HARDENING_EXPONENT_STORED");
    }
    catch (NoSuchPropertyException) {
      try {
        n0 = material.getDoubleProperty("HARDENING_EXPONENT");
        material.setProperty("HARDENING_EXPONENT_STORED",n0);
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: hardening exponent (stored part) is not defined." << std::endl;
        throw e;
      }
    }
    if (n0 < 1.0e0) {
      if (os) (*os) << "ERROR: hardening exponent (stored part) must be >= 1." << std::endl;
      throw InvalidPropertyException("hardening exponent (stored)");
    }
  }

  /*
   * dissipated part
   */
  double sig1,b1,n1=1.0e0;
  // initial yield stress
  try {
    sig1 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  }
  catch (NoSuchPropertyException) {
    try {
      sig1 = material.getDoubleProperty("INITIAL_YIELD_STRESS")-sig0;
    }
    catch (NoSuchPropertyException) {
      try {
        sig1 = material.getDoubleProperty("YIELD_IN_TENSION")-sig0;
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: initial yield stress is not defined." << std::endl;
        throw e;
      }
    }
    material.setProperty("INITIAL_YIELD_STRESS_DISSIPATED",sig1);
  }
  // hardening coefficient
  try {
    b1 = material.getDoubleProperty("HARDENING_COEFFICIENT_DISSIPATED");
  }
  catch (NoSuchPropertyException) {
    try {
      b1 = material.getDoubleProperty("HARDENING_COEFFICIENT")-b0;
    }
    catch (NoSuchPropertyException) {
      b1 = 0.0e0;
    }
    material.setProperty("HARDENING_COEFFICIENT_DISSIPATED",b1);
  }
  // hardening exponent
  if ((b0+b1) != 0.0e0) {
    try {
      n1 = material.getDoubleProperty("HARDENING_EXPONENT_DISSIPATED");
    }
    catch (NoSuchPropertyException) {
      try {
        n1 = material.getDoubleProperty("HARDENING_EXPONENT");
      }
      catch (NoSuchPropertyException) {
        try {
          n0 = material.getDoubleProperty("HARDENING_EXPONENT_STORED");
          n1 = n0;
        }
        catch (NoSuchPropertyException e) {
          if (os) (*os) << "ERROR: hardening exponent (dissipated part) is not defined." << std::endl;
          throw e;
        }
      }
      material.setProperty("HARDENING_EXPONENT_DISSIPATED",n1);
    }
    if (n1 < 1.0e0) {
      if (os) (*os) << "ERROR: hardening exponent (dissipated part) must be >= 1." << std::endl;
      throw InvalidPropertyException("hardening exponent (dissipated)");
    }
  }
   
  double c,epsDot0;
  // strain rate dependency coefficient
  try {
    c = material.getDoubleProperty("RATE_DEPENDENCY_COEFFICIENT");
    if (c < 0.0e0) {
      if (os) (*os) << "ERROR: strain rate dependency coefficient must be positive." << std::endl;
      throw InvalidPropertyException("strain rate dependency coefficient");
    }
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: strain rate dependency coefficient is not defined." << std::endl;
    throw e;
  }
  // reference strain-rate
  try {
    epsDot0 = material.getDoubleProperty("REFERENCE_STRAIN_RATE");
    if (epsDot0 <= 0.0e0) {
      if (os) (*os) << "ERROR: reference strain rate must be strictly positive." << std::endl;
      throw InvalidPropertyException("reference strain rate");
    }
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: reference strain rate is not defined." << std::endl;
    throw e;
  }

  /*
   *temperature dependence
   */
  double T0_JC,Tm,m0,m1;
  // reference temperature for JC flow stress
  try {
    T0_JC = material.getDoubleProperty("REFERENCE_TEMPERATURE_JC");
    if (T0_JC <= 0.0e0) {
      if (os) (*os) << "ERROR: JC reference temperature must be strictly positive." << std::endl;
      throw InvalidPropertyException("JC reference temperature");
    }
  }
  catch (NoSuchPropertyException) {
    T0_JC = T0;
    material.setProperty("REFERENCE_TEMPERATURE_JC",T0);
  }
  // melting temperature
  try {
    Tm = material.getDoubleProperty("MELTING_TEMPERATURE");
    if (Tm <= 0.0e0) {
      if (os) (*os) << "ERROR: melting temperature must be strictly positive." << std::endl;
      throw InvalidPropertyException("melting temperature");
    }
  }
  catch (NoSuchPropertyException e) {
    if (os) (*os) << "ERROR: melting temperature is not defined." << std::endl;
    throw e;
  }
  // temperature-dependence exponent
  try {
    m0 = material.getDoubleProperty("TEMPERATURE_DEPENDENCE_EXPONENT_STORED");
  }
  catch (NoSuchPropertyException) {
    try {
      m0 = material.getDoubleProperty("TEMPERATURE_DEPENDENCE_EXPONENT");
      material.setProperty("TEMPERATURE_DEPENDENCE_EXPONENT_STORED",m0);
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: temperature dependence exponent is not defined." << std::endl;
      throw e;
    }
  }
  if (m0 <= 0.0e0) {
    if (os) (*os) << "ERROR: temperature dependence exponent (stored part) must be strictly positive." << std::endl;
    throw InvalidPropertyException("temperature dependence exponent");
  }
  try {
    m1 = material.getDoubleProperty("TEMPERATURE_DEPENDENCE_EXPONENT_DISSIPATED");
    if (m1 <= 0.0e0) {
      if (os) (*os) << "ERROR: temperature dependence exponent (dissipated part) must be strictly positive." << std::endl;
      throw InvalidPropertyException("temperature dependence exponent");
    }
  }
  catch (NoSuchPropertyException) {
    m1 = m0;
    material.setProperty("TEMPERATURE_DEPENDENCE_EXPONENT_DISSIPATED",m1);
  }
   
  /*
   * check values
  */
  double sig = sig0+sig1;
  if (sig < 0.e0) {
    if (os) (*os) << "ERROR: initial yield stress must be positive." << std::endl;
    throw InvalidPropertyException("initial yield stress");
  }
   
  /*
   * print-out
   */
  if (os) {
    (*os) << "\n\tAt reference temperature (T = " << T0 << "):" << std::endl;
    (*os) << "\tinitial yield stress (stored part)                = " <<    sig0 << std::endl;
    (*os) << "\thardening coefficient (stored part)               = " <<      b0 << std::endl;
    if (b0 != 0.0e0)
      (*os) << "\thardening exponent (stored part)                  = " <<      n0 << std::endl;
    (*os) << "\tinitial yield stress (dissipated part)            = " <<    sig1 << std::endl;
    (*os) << "\thardening coefficient (dissipated part)           = " <<      b1 << std::endl;
    if (b1 != 0.0e0)
      (*os) << "\thardening exponent (dissipated part)              = " <<      n1 << std::endl;
    (*os) << "\tstrain rate dependency coefficient                = " <<      c  << std::endl;
    (*os) << "\treference strain rate                             = " << epsDot0 << std::endl;
    (*os) << "\treference temperature (JC)                        = " <<   T0_JC << std::endl;
    (*os) << "\tmelting temperature                               = " <<      Tm << std::endl;
    (*os) << "\ttemperature dependency exponent (stored part)     = " <<      m0 << std::endl;
    (*os) << "\ttemperature dependency exponent (dissipated part) = " <<      m1 << std::endl;
   }
}

// compute irreversible energy and derivatives
double JohnsonCookModel::irreversibleEnergy(const MaterialProperties& material,
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
  
  // stored part
  double sig0=0.e0,h0=0.e0,dSig0=0.e0,C0=0.e0;
  Wp = storedEnergy(material,extPar,epsPl1,Th1,sig0,Np,h0,dSig0,C0,first,second);
  intPar[0] = Wp;

  // dissipated part
  double D=0.e0,sig1=0.e0,N1=0.e0,h1=0.e0,dSig1=0.e0,C1=0.e0,epsPlDot=0.0e0;
  if (dTime > 0.0e0) epsPlDot = ratio0*(epsPl1-epsPl0)/dTime;
  double D1,D2,sig1a,sig2a,sig1b,sig2b,sigc;
  double h1aa,h2aa,h1bb,h2bb,hcc,h1ab,h2ab,hac,hbc;
  D1 = dTime*dissipatedEnergy(material,extPar,epsPl,epsPlDot,Th0,sig1a,sig1b,sigc,
                              h1aa,h1bb,hcc,h1ab,hac,hbc,first || second,second);
  D2 = dTime*dissipatedEnergy(material,extPar,epsPl,epsPlDot,Th,sig2a,sig2b,sigc,
                              h2aa,h2bb,hcc,h2ab,hac,hbc,first || second,second);
  D = ratio3*D1+ratio2*D2;

  double coef = alpha*dTime;
  if (first) {
    double val = sig1b+ratio1*sig2b;
    sig1 = coef*(ratio3*sig1a+ratio2*sig2a) + val;
    N1 = (val*(epsPl1-epsPl0) + coef*dT*sigc + dTime*ratio3*(D2-D1))/T1;
  }
  if (second) {
    h1 =  alpha*coef*(ratio3*h1aa+ratio2*h2aa) + 2*alpha*(h1ab+ratio1*h2ab);
    if (dTime > 0.0e0) h1 += ratio0*(h1bb+ratio1*h2bb)/dTime;
    dSig1 =  alpha*ratio2*(coef*hac + ratio0*hbc)
           + (coef*(h1ab+ratio1*h2ab) + ratio0*(h1bb+ratio1*h2bb))*epsPlDot*ratio3/T1
           + sig2b/T0 + coef*ratio3*(sig2a-sig1a)/T1;
    if (std::fabs(hcc) < std::numeric_limits<double>::max())
      C1 =  coef*ratio2*(2*hbc*epsPlDot/T1 + alpha*hcc)
          + 2*dTime*(alpha*sigc - (D2-D1)/T1)*ratio3/T1
          + ((h1bb+ratio1*h2bb)*epsPlDot + 2*(sig2b-sig1b))*(epsPl1-epsPl0)/(T1*T1);
    else if (hcc <= -std::numeric_limits<double>::max())
      C1 = -std::numeric_limits<double>::max();
    else if (hcc >=std::numeric_limits<double>::max())
      C1 = +std::numeric_limits<double>::max();
  }
  
  // assemble components
  if (first) {
    sig = sig0 + sig1;
    dNp = Np + N1;
  }
  if (second) {
    h = h0 + h1;
    dSig = dSig0 + dSig1;
    C = C0 + C1;
  }
  
  // compute heat fraction
  if (first) {
    double beta = 1.0e0;
    if (sig > 1.e-16) beta = sig1/sig;
    /*double Dt = (sig-sig0)*(epsPl1-epsPl0); // this version includes entropic effects
     double Wt = Wp-Wp0+Dt;
     if (Wt > 1.e-16) beta = Dt/Wt;*/
    intPar[1] = beta;
  }
  
  return Wp-Wp0+D;
}

// (plastic) stored energy
double JohnsonCookModel::storedEnergy(const MaterialProperties& material,const ParameterSet& extPar,
                                      double epsPl,double Th,double& sig,double& N,
                                      double& h,double& dSig,double& C,bool first,bool second) {

  static const double PRECISION = 1.0e-16;
  
  // initialize
  double Wp = 0.e0;
  sig = N = 0.0e0;
  h = dSig = C = 0.0e0;
  
  // temperature dependence
  double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
  double T0_JC = material.getDoubleProperty("REFERENCE_TEMPERATURE_JC");
  double Th_JC = Th+T0-T0_JC;
  double Tm = material.getDoubleProperty("MELTING_TEMPERATURE");
  double m = material.getDoubleProperty("TEMPERATURE_DEPENDENCE_EXPONENT_STORED");
  double expoT = 1.0e0/m;
  double val0 = 1.0e0/(Tm-T0_JC);
  double val = Th_JC*val0;
  double coef,val1,dVal1;
  if (val < 1.0e0) {
    if (first || second) {
      if (val > PRECISION) {
        dVal1 = std::pow(val,expoT-1);
        val1 = dVal1*val;
      }
      else {
        val1 = 0.0e0;
        dVal1 = 0.0e0;
      }
    }
    else {
      if (val > PRECISION)
        val1 = std::pow(val,expoT);
      else
        val1 = 0.0e0;
    }
  }
  else {
    val1 = 1.0e0;
    dVal1 = 0.0e0;
  }
  coef = 1.0-val1;

  // power-law (Ludwik type)
  double sig0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
  double b = material.getDoubleProperty("HARDENING_COEFFICIENT_STORED");
  if (b != 0.0e0 && epsPl > PRECISION) {
    double n = material.getDoubleProperty("HARDENING_EXPONENT_STORED");
    double expo = 1.0e0/n;
    if (first) {
      double val = std::pow(epsPl,expo);
      sig = (sig0+b*val)*coef;
      double W0 = (sig0+b*val/(expo+1))*epsPl;
      Wp = W0*coef;
      N = -W0*expoT*dVal1*val0;
    }
    else {
      double val = std::pow(epsPl,expo+1);
      Wp = (sig0*epsPl+b*val/(expo+1))*coef;
    }
    if (second) {
      double val =std::pow(epsPl,expo-1);
      h = expo*b*val*coef;
      if (Th_JC > PRECISION) {
        dSig = -(sig0+b*val*epsPl)*expoT*val1/Th_JC;
        C = -(sig0+b*val*epsPl/(expo+1))*epsPl*expoT*(expoT-1)*val1/(Th_JC*Th_JC);
      }
      else {
        dSig = 0.0e0;
        C = 0.0e0;
      }
    }
  }
  else {
    double sigT = sig0*coef;
    Wp = sigT*epsPl;
    if (first) {
      sig = sigT;
      N = -sig0*epsPl*expoT*dVal1*val0;
    }
    if (second) {
      if (b != 0.0)
        h = std::numeric_limits<double>::max();
      if (Th_JC > PRECISION) {
        dSig = -sig0*expoT*val1/Th_JC;
        C = -sig0*epsPl*expoT*(expoT-1)*val1/(Th_JC*Th_JC);
      }
      else {
        dSig = 0.0e0;
        C = 0.0e0;
      }
    }
  }
  
  return Wp;
}

// dissipation potential
double JohnsonCookModel::dissipatedEnergy(const MaterialProperties& material,
                                          const ParameterSet& extPar,
                                          double epsPl,double epsPlDot,double Th,
                                          double& sig1,double& sig2,double& N,
                                          double& h11,double& h22,double& C,
                                          double& h12,double& h1T,double& h2T,
                                          bool first,bool second) {
  
  static const double PRECISION = 1.0e-16;
  
  // initialize
  double D = 0.0e0;
  
  // hardening part (rate-independent)
  double siga,dSiga=0.0e0,d2Siga=0.0e0;
  double sig0d = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  double bd = material.getDoubleProperty("HARDENING_COEFFICIENT_DISSIPATED");
  if (bd != 0.0e0 && epsPl > PRECISION) {
    double n = material.getDoubleProperty("HARDENING_EXPONENT_DISSIPATED");
    double expo = 1.0e0/n;
    double val = std::pow(epsPl,expo);
    siga = sig0d+bd*val;
    if (first || second) dSiga = bd*expo*val/epsPl;
    if (second) d2Siga = dSiga*(expo-1.0)/epsPl;
  }
  else {
    siga = sig0d;
    if (bd != 0.0e0) {
      double n = material.getDoubleProperty("HARDENING_EXPONENT_DISSIPATED");
      double expo = 1.0e0/n;
      if (expo < 1.0) dSiga = std::numeric_limits<double>::max();
      if (expo < 2.0) d2Siga = std::numeric_limits<double>::max();
    }
  }
  
  // hardening part (rate-dependent)
  double sigb,dSigb=0.0e0,d2Sigb=0.0e0;
  double sig0 = sig0d+material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
  double b = bd+material.getDoubleProperty("HARDENING_COEFFICIENT_STORED");
  if (b != 0.0e0 && epsPl > PRECISION) {
    double n = material.getDoubleProperty("HARDENING_EXPONENT_DISSIPATED");
    double expo = 1.0e0/n;
    double val = std::pow(epsPl,expo);
    sigb = sig0+b*val;
    if (first || second) dSigb = b*expo*val/epsPl;
    if (second) d2Sigb = dSigb*(expo-1.0)/epsPl;
  }
  else {
    sigb = sig0;
    if (b != 0.0e0) {
      double n = material.getDoubleProperty("HARDENING_EXPONENT_DISSIPATED");
      double expo = 1.0e0/n;
      if (expo < 1.0) dSigb = std::numeric_limits<double>::max();
      if (expo < 2.0) d2Sigb = std::numeric_limits<double>::max();
    }
  }

  // rate-dependency part
  double fact,dFact=0.0,d2Fact=0.0;
  double cV = material.getDoubleProperty("RATE_DEPENDENCY_COEFFICIENT");
  double epsDot0 = material.getDoubleProperty("REFERENCE_STRAIN_RATE");
  double eDot;
  if (epsPlDot >= epsDot0)
    eDot = epsPlDot/epsDot0;
  else
    eDot = 1.0e0;
  if (cV != 0.0e0 && eDot > 1.0e0) {
    double val = std::log(eDot);
    fact = cV*epsDot0*(eDot*val-eDot+1.0);
    if (first || second) dFact = cV*val;
    if (second) d2Fact = cV/eDot;
  }
  else
    fact = 0.0e0;
  
  // temperature-dependence part
  double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
  double T0_JC = material.getDoubleProperty("REFERENCE_TEMPERATURE_JC");
  double Tm = material.getDoubleProperty("MELTING_TEMPERATURE");
  double m = material.getDoubleProperty("TEMPERATURE_DEPENDENCE_EXPONENT_DISSIPATED");
  double expoT = 1.0e0/m;
  double val0 = 1.0e0/(Tm-T0_JC);
  double Th0 = (Th+T0-T0_JC)*val0;
  double coef,dCoef=0.0,d2Coef=0.0;
  if (Th0 < 1.0e0) {
    if (first || second) {
      if (Th0 > PRECISION) {
        double val = std::pow(Th0,expoT-1);
        coef = 1.0-val*Th0;
        dCoef = -expoT*val*val0;
      }
      else {
        coef = 1.0e0;
        dCoef = 0.0e0;
      }
    }
    else {
      if (Th0 > PRECISION)
        coef = 1.0-std::pow(Th0,expoT);
      else
        coef = 1.0e0;
    }
    if (second) {
      if (Th0 > PRECISION)
        d2Coef = -dCoef*(expoT-1)/(Th+T0-T0_JC);
      else
        d2Coef = 0.0e0;
    }
  }
  else {
    coef = 0.0e0;
    dCoef = 0.0e0;
    d2Coef = 0.0e0;
  }
  
  // assemble expression
  D = (siga*epsPlDot+sigb*fact)*coef;
  if (first) {
    sig1 = (dSiga*epsPlDot+dSigb*fact)*coef;
    sig2 = (siga+sigb*dFact)*coef;
    N = (siga*epsPlDot+sigb*fact)*dCoef;
  }
  if (second) {
    h11 = (d2Siga*epsPlDot+d2Sigb*fact)*coef;
    h22 = sigb*d2Fact*coef;
    C = (siga*epsPlDot+sigb*fact)*d2Coef;
    h12 = (dSiga+dSigb*dFact)*coef;
    h1T = (dSiga*epsPlDot+dSigb*fact)*dCoef;
    h2T = (siga+sigb*dFact)*dCoef;
  }
  
  return D;
}


/*
 * Methods for class JohnsonCookJ2ThermoPlasticityBuilder.
 */

// the instance
JohnsonCookJ2ThermoPlasticityBuilder const* JohnsonCookJ2ThermoPlasticityBuilder::BUILDER
  = new JohnsonCookJ2ThermoPlasticityBuilder();

// constructor
JohnsonCookJ2ThermoPlasticityBuilder::JohnsonCookJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("JOHNSON_COOK_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* JohnsonCookJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new JohnsonCookJ2ThermoPlasticity3D();
      break;
    case 2:
      return new JohnsonCookJ2ThermoPlasticity2D();
      break;
    case 1:
      return new JohnsonCookJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class AdiabaticJohnsonCookJ2ThermoPlasticityBuilder.
 */

// the instance
AdiabaticJohnsonCookJ2ThermoPlasticityBuilder const* AdiabaticJohnsonCookJ2ThermoPlasticityBuilder::BUILDER
  = new AdiabaticJohnsonCookJ2ThermoPlasticityBuilder();

// constructor
AdiabaticJohnsonCookJ2ThermoPlasticityBuilder::AdiabaticJohnsonCookJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("ADIABATIC_JOHNSON_COOK_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* AdiabaticJohnsonCookJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new AdiabaticJohnsonCookJ2ThermoPlasticity3D();
      break;
    case 2:
      return new AdiabaticJohnsonCookJ2ThermoPlasticity2D();
      break;
    case 1:
      return new AdiabaticJohnsonCookJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledJohnsonCookJ2ThermoPlasticityBuilder.
 */

// the instance
CoupledJohnsonCookJ2ThermoPlasticityBuilder const* CoupledJohnsonCookJ2ThermoPlasticityBuilder::BUILDER
  = new CoupledJohnsonCookJ2ThermoPlasticityBuilder();

// constructor
CoupledJohnsonCookJ2ThermoPlasticityBuilder::CoupledJohnsonCookJ2ThermoPlasticityBuilder() {
  ModelDictionary::add("COUPLED_JOHNSON_COOK_J2_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledJohnsonCookJ2ThermoPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledJohnsonCookJ2ThermoPlasticity3D();
      break;
    case 2:
      return new CoupledJohnsonCookJ2ThermoPlasticity2D();
      break;
    case 1:
      return new CoupledJohnsonCookJ2ThermoPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class JohnsonCookJ2ThermoHEPlasticityBuilder.
 */

// the instance
JohnsonCookJ2ThermoHEPlasticityBuilder const* JohnsonCookJ2ThermoHEPlasticityBuilder::BUILDER
  = new JohnsonCookJ2ThermoHEPlasticityBuilder();

// constructor
JohnsonCookJ2ThermoHEPlasticityBuilder::JohnsonCookJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("JOHNSON_COOK_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* JohnsonCookJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new JohnsonCookJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new JohnsonCookJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new JohnsonCookJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class AdiabaticJohnsonCookJ2ThermoHEPlasticityBuilder.
 */

// the instance
AdiabaticJohnsonCookJ2ThermoHEPlasticityBuilder const* AdiabaticJohnsonCookJ2ThermoHEPlasticityBuilder::BUILDER
  = new AdiabaticJohnsonCookJ2ThermoHEPlasticityBuilder();

// constructor
AdiabaticJohnsonCookJ2ThermoHEPlasticityBuilder::AdiabaticJohnsonCookJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("ADIABATIC_JOHNSON_COOK_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* AdiabaticJohnsonCookJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new AdiabaticJohnsonCookJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new AdiabaticJohnsonCookJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new AdiabaticJohnsonCookJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}

/*
 * Methods for class CoupledJohnsonCookJ2ThermoHEPlasticityBuilder.
 */

// the instance
CoupledJohnsonCookJ2ThermoHEPlasticityBuilder const* CoupledJohnsonCookJ2ThermoHEPlasticityBuilder::BUILDER
  = new CoupledJohnsonCookJ2ThermoHEPlasticityBuilder();

// constructor
CoupledJohnsonCookJ2ThermoHEPlasticityBuilder::CoupledJohnsonCookJ2ThermoHEPlasticityBuilder() {
  ModelDictionary::add("COUPLED_JOHNSON_COOK_J2_FINITE_THERMO_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CoupledJohnsonCookJ2ThermoHEPlasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CoupledJohnsonCookJ2ThermoHEPlasticity3D();
      break;
    case 2:
      return new CoupledJohnsonCookJ2ThermoHEPlasticity2D();
      break;
    case 1:
      return new CoupledJohnsonCookJ2ThermoHEPlasticity1D();
      break;
    default:
      return 0;
      break;
  }
}
