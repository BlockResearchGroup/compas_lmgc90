/*
 *  $Id: CuitinoBCC.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "CuitinoBCC.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>
//#if defined(_WIN32) || defined(_WIN64)
//double asinh(double x) {
//  return std::log(x+std::sqrt(x*x+1.0));
//}
//double erf(double x) { /* from http://www.johndcook.com/cpp_erf.html */
//    // constants
//    double a1 =  0.254829592;
//    double a2 = -0.284496736;
//    double a3 =  1.421413741;
//    double a4 = -1.453152027;
//    double a5 =  1.061405429;
//    double p  =  0.3275911;
//
//    // Save the sign of x
//    int sign = 1;
//    if (x < 0) sign = -1;
//    x = std::fabs(x);
//
//    // A&S formula 7.1.26
//    double t = 1.0/(1.0 + p*x);
//    double y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x);
//
//    return sign*y;
//}
//inline double erfc(double x) {return 1.0-erf(x);}
//#endif
// std C++ library
#include <limits>


/*
 * Methods for BCCHardeningCuitino
 */

// flag for twinning systems
const bool BCCHardeningCuitino::ANTITWINNING[48] = {
  false,false,false,false,false,false,
  false,false,false,false,false,false,
  false,false,false,false,false,false,
  false,false,false,false,false,false,
  false,false,true ,true ,false,false,
  true ,true ,false,false,true ,true,
  false,false,true ,true ,false,false,
  true ,true ,false,false,true ,true};

// check consistency of material properties
void BCCHardeningCuitino::checkProperties(MaterialProperties& material,
                                          std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***BCC hardening model (Stainier-Cuitino-Ortiz style)***" << std::endl;

  // check hardening material properties
  double g0,g1;
  try {
    g0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
    if (g0 < 0.0e0) {
      if (os) (*os) << "ERROR: initial yield stress must be positive or zero." << std::endl;
      throw InvalidPropertyException("initial yield stress (stored part)");
    }
  }
  catch (NoSuchPropertyException) {
    g0 = material.getDoubleProperty("INITIAL_YIELD_STRESS");
    if (g0 < 0.0e0) {
      if (os) (*os) << "ERROR: initial yield stress must be positive or zero." << std::endl;
      throw InvalidPropertyException("initial yield stress");
    }
    material.setProperty("INITIAL_YIELD_STRESS_STORED",g0);
  }
  try {
    g1 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
    if (g1 < 0.0e0) {
      if (os) (*os) << "ERROR: initial yield stress must be positive or zero." << std::endl;
      throw InvalidPropertyException("initial yield stress (dissipated part)");
    }
  }
  catch (NoSuchPropertyException) {
    g1 = 0.0e0;
    material.setProperty("INITIAL_YIELD_STRESS_DISSIPATED",g1);
  }
  
  double factor = 1.0e0;
  try {
    factor = material.getDoubleProperty("ANTI_TWINNING_FACTOR");
  }
  catch (NoSuchPropertyException) {
    material.setProperty("ANTI_TWINNING_FACTOR",factor);
  }
  if (factor <= 0.0e0) {
    if (os) (*os) << "ERROR: anti-twinning factor must be positive." << std::endl;
    throw InvalidPropertyException("anti-twinning factor");
  }
  
  double b = material.getDoubleProperty("BURGERS_VECTOR");
  if (b <= 0.0e0) {
    if (os) (*os) << "ERROR: Burgers vector must be positive." << std::endl;
    throw InvalidPropertyException("Burgers vector");
  }

  double Ue = material.getDoubleProperty("EDGE_DISLOCATION_SELF_ENERGY");
  if (Ue <= 0.0e0) {
    if (os) (*os) << "ERROR: dislocation line self-energy must be positive." << std::endl;
    throw InvalidPropertyException("dislocation line self-energy");
  }
  
  double ratio = material.getDoubleProperty("EDGE_TO_SCREW_RATIO");
  if (ratio <= 0.0e0) {
    if (os) (*os) << "ERROR: edge/screw self-energy ratio must be positive." << std::endl;
    throw InvalidPropertyException("edge/screw self-energy ratio");
  }
  double Us = Ue/ratio;
  material.setProperty("SCREW_DISLOCATION_SELF_ENERGY",Us);
  
  double tauSat = material.getDoubleProperty("JUNCTION_STRENGTH");
  if (tauSat <= 0.0e0) {
    if (os) (*os) << "ERROR: junction strength must be positive." << std::endl;
    throw InvalidPropertyException("junction strength");
  }
  
  double rho0 = material.getDoubleProperty("INITIAL_DISLOCATION_DENSITY");
  if (rho0 <= 0.0e0) {
    if (os) (*os) << "ERROR: initial dislocation density must be positive." << std::endl;
    throw InvalidPropertyException("initial dislocation density");
  }

  double rhoSat = material.getDoubleProperty("SATURATION_DISLOCATION_DENSITY");
  if (rhoSat <= 0.0e0) {
    if (os) (*os) << "ERROR: saturation dislocation density must be positive." << std::endl;
    throw InvalidPropertyException("saturation dislocation density");
  }

  double gamSat = material.getDoubleProperty("SATURATION_STRAIN");
  if (gamSat <= 0.0e0) {
    if (os) (*os) << "ERROR: saturation strain must be positive." << std::endl;
    throw InvalidPropertyException("saturation strain");
  }

  double a0 = material.getDoubleProperty("INTERACTION_COEFFICIENT");
  if (a0 <= 0.0e0) {
    if (os) (*os) << "ERROR: interaction coefficient must be positive." << std::endl;
    throw InvalidPropertyException("interaction coefficient a0");
  }

  // print-out properties
  if (os) {
    (*os) << "\tinitial yield stress (stored part)     = " <<   g0   << std::endl;
    (*os) << "\tinitial yield stress (dissipated part) = " <<   g1   << std::endl;
    (*os) << "\tanti-twinning factor                   = " << factor << std::endl;
    (*os) << "\tBurgers vector                         = " <<   b    << std::endl;
    (*os) << "\tedge dislocation line self-energy      = " <<   Ue   << std::endl;
    (*os) << "\tscrew dislocation line self-energy     = " <<   Us   << std::endl;
    (*os) << "\tedge/screw self-energy ratio           = " <<  ratio << std::endl;
    (*os) << "\treference junction strength            = " << tauSat << std::endl;
    (*os) << "\tinitial dislocation density            = " << rho0   << std::endl;
    (*os) << "\tsaturation dislocation density         = " << rhoSat << std::endl;
    (*os) << "\tsaturation strain                      = " << gamSat << std::endl;
    (*os) << "\tinteraction coefficient a0             = " <<   a0   << std::endl;
  }
  
  static const double PI = 4.*std::atan(1.0e0);
  static const double PRECISION = 1.0e-16;

  // get slip systems
  SlipSystemsProperty<Vector3D>& systems
    = dynamic_cast<SlipSystemsProperty<Vector3D>&>(material.getProperty("SLIP_SYSTEMS"));
  
  // compute interaction matrix
  StdProperty<MatLibMatrix> inter;
  inter.value().resize(SingleCrystalBCC::NSYS/2);
  MatLibMatrix& aMat = inter.value();
  double coef = 2/PI*a0;
  for (unsigned int k=0; k < SingleCrystalBCC::NSYS/2; k++)
    for (unsigned int l=0; l < SingleCrystalBCC::NSYS/2; l++) {
      double val = innerProd(systems.system(k).second,systems.system(l).second);
      val = 1.0e0-val*val;
      if (val < PRECISION) val = 0.0e0;
      aMat[k][l] = coef*std::sqrt(val);
    }
  material.setProperty("INTERACTION_MATRIX",inter);
  
  // compute jog energies
  StdProperty<MatLibMatrix> jogs;
  jogs.value().resize(SingleCrystalBCC::NSYS/2);
  MatLibMatrix& jMat = jogs.value();
  for (unsigned int k=0; k < SingleCrystalBCC::NSYS/2; k++) {
    
    // edge direction (screw direction = slip direction)
    Vector3D se = crossProd(systems.system(k).first,systems.system(k).second);
    
    for (unsigned int l=0; l < SingleCrystalBCC::NSYS/2; l++) {
      double css = innerProd(systems.system(k).first,systems.system(l).first);
      css = std::fabs(css);
      double c2 = css*css;
      double s2 = 1.0e0-c2;
      double ct = innerProd(systems.system(l).first,se);
      ct = std::fabs(ct);
      //jMat[l][k] = 2.*c2+2.*ratio*s2 - css - ratio*ct;
      if (s2 < PRECISION)
        jMat[l][k] = 1.0e0-ratio*ct;
      else
        jMat[l][k] = 2*ratio-std::fabs(css)-ratio*ct;
    }
  }
  std::cout << jMat << std::endl;
  material.setProperty("JOG_ENERGIES_MATRIX",jogs);
}

// plastically stored energy
double BCCHardeningCuitino::storedEnergy(const MaterialProperties& material,
                                         const ParameterSet& extPar,
                                         const MatLibArray& intPar0,
                                         MatLibArray& intPar1,double Wp0,
                                         const MatLibArray& gamma0,
                                         const MatLibArray& gamma1,
                                         MatLibArray& tau,MatLibMatrix& H,
                                         bool first,bool second) {
  
  // compute hardening matrix
  static MatLibArray hDiag(SingleCrystalBCC::NSYS);
  if (initialize) {
    hardeningMatrix(material,gamma0,intPar0,hDiag);
    initialize = false;
  }
  
  // compute plastic energy
  double Wp = Wp0;
  double g00 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
  double factor = material.getDoubleProperty("ANTI_TWINNING_FACTOR");
  double g0AT = g00*factor;
  double dGam,dg;
  if (first) {
    for (unsigned int k=0; k < SingleCrystalBCC::NSYS; k++) {
      dGam = gamma1[k]-gamma0[k];
      dg = hDiag[k]*dGam;
      if (!ANTITWINNING[k]) {
        tau[k] = g00+intPar0[k]+dg;
        Wp += (g00+intPar0[k]+0.5*dg)*dGam;
      }
      else {
        tau[k] = g0AT+intPar0[k]+dg;
        Wp += (g0AT+intPar0[k]+0.5*dg)*dGam;
      }
    }
  }
  else {
    for (unsigned int k=0; k < SingleCrystalBCC::NSYS; k++) {
      dGam = gamma1[k]-gamma0[k];
      if (!ANTITWINNING[k])
        Wp += (g00+intPar0[k]+0.5*hDiag[k]*dGam)*dGam;
      else
        Wp += (g0AT+intPar0[k]+0.5*hDiag[k]*dGam)*dGam;
    }
  }
  
  if (second) {
    H = 0.0e0;
    for (unsigned int k=0; k < SingleCrystalBCC::NSYS; k++) H[k][k] = hDiag[k];
  }
  
  // update internal parameters
  if (finalize) {
    for (unsigned int k=0; k < SingleCrystalBCC::NSYS; k++) {
      dGam = gamma1[k]-gamma0[k];
      dg = hDiag[k]*dGam;
      intPar1[k] = intPar0[k]+dg;
    }
  }
  
  return Wp;
}

// yield stress
void BCCHardeningCuitino::yieldStress(const MaterialProperties& material,
                                      const ParameterSet& extPar,
                                      const MatLibArray& intPar0,
                                      MatLibArray& intPar1,
                                      const MatLibArray& gamma0,
                                      const MatLibArray& gamma1,
                                      const MatLibArray& gamma,
                                      MatLibArray& tau,MatLibMatrix& H,
                                      std::vector<MatLibMatrix>& dH,
                                      bool first,bool second) {
  // get yield stress
  double g1 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  double factor = material.getDoubleProperty("ANTI_TWINNING_FACTOR");
  double g1AT = g1*factor;

  // set CRSS on all systems
  for (unsigned int k=0; k < SingleCrystalBCC::NSYS; k++) {
    if (!ANTITWINNING[k])
      tau[k] = g1;
    else
      tau[k] = g1AT;
  }
  
  // tangents
  if (first) H = 0.0e0;
}

// The heart of the model: the hardening matrix.
void BCCHardeningCuitino::hardeningMatrix(const MaterialProperties& material,
                                          const MatLibArray& gamma,
                                          const MatLibArray& g,MatLibArray& hDiag) {
  
  static const double PI = 4*std::atan(1.0e0);
  static const double SQRPI = std::sqrt(PI);
  static const double THRSHLD 
    = std::sqrt(1.0e0/std::log(std::numeric_limits<double>::max()));
  static const double PRECISION = 1.0e-16;

  // compute dislocation densities
  MatLibArray rho(SingleCrystalBCC::NSYS/2);
  double rhoInit = material.getDoubleProperty("INITIAL_DISLOCATION_DENSITY");
  rhoInit = std::sqrt(rhoInit);
  double rhoSat = material.getDoubleProperty("SATURATION_DISLOCATION_DENSITY");
  rhoSat = std::sqrt(rhoSat);
  double dRho = rhoSat-rhoInit;
  double gamSat = material.getDoubleProperty("SATURATION_STRAIN");
  double coef = 0.5e0/gamSat;

  unsigned int k,l,nSys2 = SingleCrystalBCC::NSYS/2;
  for (k=0, l=nSys2; k < nSys2; k++, l++) {
    double gamP = gamma[k];
    double gamN = gamma[l];
    double gamT = gamP+gamN;
    double val = rhoSat-dRho*std::exp(-gamT*coef);
    rho[k] = val*val;
  }
  
  // compute obstacle densities
  MatLibArray nObst(SingleCrystalBCC::NSYS/2);
  MatLibMatrix nAlphaBeta(SingleCrystalBCC::NSYS/2);
  StdProperty<MatLibMatrix>& inter
    = dynamic_cast<StdProperty<MatLibMatrix>&>(material.getProperty("INTERACTION_MATRIX"));
  MatLibMatrix& aMat = inter.value();
  for (k=0; k < nSys2; k++) {
    double val = 0.0e0;
    for (l=0; l < nSys2; l++) {
      nAlphaBeta[k][l] = aMat[k][l]*rho[l];
      if (k == l) continue;
      val += nAlphaBeta[k][l];
    }
    nObst[k] = val;
  }
  // compute pair probabilities
  MatLibMatrix pAlphaBeta(SingleCrystalBCC::NSYS/2);
  StdProperty<MatLibMatrix>& jogs
    = dynamic_cast<StdProperty<MatLibMatrix>&>(material.getProperty("JOG_ENERGIES_MATRIX"));
  MatLibMatrix& jMat = jogs.value();
  for (k=0; k < nSys2; k++) {
    double val = 1.0e0/nObst[k];
    val = val*val;
    for (l=0; l < nSys2; l++) {
      if (k == l)
        pAlphaBeta[k][l] = 0.0e0;
      else {
        double nab,val1=0.0e0,val2=0.0e0;
        for (unsigned int m=0; m < nSys2; m++) {
          if (k == m || l == m) continue;
          nab = nAlphaBeta[k][m];
          if (jMat[k][m] == jMat[k][l]) val1 += nab;
          if (jMat[k][m] >  jMat[k][l]) val2 += nab;
        }
        nab = nAlphaBeta[k][l];
        pAlphaBeta[k][l] = nab*(nab+val1+2*val2)*val;
      }
    }
  }
  
  // compute hardening matrix
  double burgers = material.getDoubleProperty("BURGERS_VECTOR");
  double Uedge = material.getDoubleProperty("EDGE_DISLOCATION_SELF_ENERGY");
  coef = 2*Uedge/burgers*SQRPI;
  double g00 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
  double factor = material.getDoubleProperty("ANTI_TWINNING_FACTOR");
  double g0AT = g00*factor;
  double tauSat0 = material.getDoubleProperty("JUNCTION_STRENGTH");
  
  double gc,tc,hc,rn;
  for (k=0; k < SingleCrystalBCC::NSYS; k++) {
    unsigned int kk = (k < nSys2) ? k:(k-nSys2);
    rn = std::sqrt(nObst[kk]);
    tc = coef*rn;
    gc = 0.5*burgers*rho[kk]/rn;
    hc = tc/gc;
    double g0;
    if (!ANTITWINNING[k])
      g0 = g00+g[k];
    else
      g0 = g0AT+g[k];
    double r = g0/tc;
    if (r < THRSHLD) r = THRSHLD;
    double r2 = r*r;
    double ri = 1.0e0/r;
    double r2i = 1.0e0/r2;
    
    double val1=0.0e0,val2=0.0e0;
    for (l=0; l < nSys2; l++) {
      if (l == kk) continue;
      double tauSat = tauSat0*jMat[kk][l];
      if (g0 > tauSat) continue;
      double val = erfc(tc/tauSat);
      if (val > PRECISION)
        val = 1.0e0/val;
      else // because factors to val below will tend to zero faster
        val = 0.0e0;
      val1 += pAlphaBeta[kk][l]*val*std::exp(-r2i);
      val2 += pAlphaBeta[kk][l]*(1.0e0-val*erfc(ri));
    }
    
    val2 = val2*val2;
    if (val1 > PRECISION && val2 > PRECISION)
      hDiag[k] = 0.5*SQRPI*hc*r2*val2/val1;
    else if (val2 > PRECISION)
      hDiag[k] = 1.0e16;
    else 
      hDiag[k] = 0.0e0;
  }
}


/*
 * Methods for class BCCRateDependency.
 */

// check consistency of material properties
void BCCRateDependency::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***BCC rate dependency model (power law)***" << std::endl;
  
  double tau0;
  try {
    tau0 = material.getDoubleProperty("REFERENCE_STRESS");
  }
  catch (NoSuchPropertyException) {
    tau0 = material.getDoubleProperty("REFERENCE_PEIERLS_STRESS");
    material.setProperty("REFERENCE_STRESS",tau0);
  }
  if (tau0 < 0.0e0) {
    if (os) (*os) << "ERROR: reference stress must be positive or zero." << std::endl;
    throw InvalidPropertyException("reference Peierls stress");
  }
  
  double gamDot0 = material.getDoubleProperty("REFERENCE_SLIP_RATE");
  if (gamDot0 <= 0.0e0) {
    if (os) (*os) << "ERROR: reference slip rate must be strictly positive." << std::endl;
    throw InvalidPropertyException("reference slip rate");
  }
  
  double m = material.getDoubleProperty("RATE_DEPENDENCY_EXPONENT");
  if (m <= 0.0e0) {
    if (os) (*os) << "ERROR: rate dependency exponent must be strictly positive." << std::endl;
    throw InvalidPropertyException("rate dependency exponent");
  }
  
  // print-out properties
  if (os) {
    (*os) << "\treference stress         = " << tau0    << std::endl;
    (*os) << "\treference slip rate      = " << gamDot0 << std::endl;
    (*os) << "\trate dependency exponent = " <<   m     << std::endl;
  }
}

// dissipated energy
double BCCRateDependency::dissipatedEnergy(const MaterialProperties& material,
                                           const ParameterSet& extPar,
                                           const MatLibArray& intPar0,
                                           MatLibArray& intPar1,
                                           const MatLibArray& gamma,
                                           const MatLibArray& gamDot,
                                           MatLibArray& tau1,MatLibArray& tau2,
                                           MatLibMatrix& H11,MatLibMatrix& H22,
                                           MatLibMatrix& H12,bool first,bool second) {
  // initialize to zero
  tau1 = tau2 = 0.0e0;
  H11 = H22 = H12 = 0.0e0;
  
  // get material parameters
  static const double PRECISION = 1.0e-16;
  double tau0 = material.getDoubleProperty("REFERENCE_STRESS");
  if (tau0 < PRECISION) return 0.0e0;
  double gamDot0 = material.getDoubleProperty("REFERENCE_SLIP_RATE");
  double m = material.getDoubleProperty("RATE_DEPENDENCY_EXPONENT");
  
  // compute dissipation pseudo-potential and derivatives
  double phi = 0.0e0;
  double expo = 1.0e0/m;
  double coef1 = 1.0e0/gamDot0;
  double coef2 = 1.0e0/(expo+1);
  if (first) {
    for (unsigned int k=0; k < SingleCrystalBCC::NSYS; k++) {
      if (gamDot[k] <= 0.0e0) continue;
      double val = gamDot[k]*coef1;
      tau2[k] = tau0*std::pow(val,expo);
      phi += coef2*tau2[k]*gamDot[k];
    }
  }
  else {
    double expo1 = expo+1;
    double coef0 = tau0*gamDot0*coef2;
    for (unsigned int k=0; k < SingleCrystalBCC::NSYS; k++) {
      if (gamDot[k] <= 0.0e0) continue;
      double val = gamDot[k]*coef1;
      phi += coef0*std::pow(val,expo1);
    }
  }
  
  // compute second derivatives (diagonal matrix)
  if (second) {
    double expo2 = expo-1;
    double coef3 = expo*tau0/gamDot0;
    for (unsigned int k=0; k < SingleCrystalBCC::NSYS; k++) {
      if (gamDot[k] <= 0.0e0) continue;
      double val = gamDot[k]*coef1;
      H22[k][k] = coef3*std::pow(val,expo2);
    }
  }
  
  return phi;
}


/*
 * Methods for class BCCRateDependencyASinh.
 */

// check consistency of material properties
void BCCRateDependencyASinh::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***BCC rate dependency model (thermal activation)***" << std::endl;
  
  double tau0 = material.getDoubleProperty("REFERENCE_PEIERLS_STRESS");
  if (tau0 < 0.0e0) {
    if (os) (*os) << "ERROR: reference stress must be positive or zero." << std::endl;
    throw InvalidPropertyException("reference Peierls stress");
  }
  
  double gamDot0 = material.getDoubleProperty("REFERENCE_SLIP_RATE");
  if (gamDot0 <= 0.0e0) {
    if (os) (*os) << "ERROR: reference slip rate must be strictly positive." << std::endl;
    throw InvalidPropertyException("reference slip rate");
  }
  
  // print-out properties
  if (os) {
    (*os) << "\treference Peierls stress = " << tau0    << std::endl;
    (*os) << "\treference slip rate      = " << gamDot0 << std::endl;
  }
}

// dissipated energy
double BCCRateDependencyASinh::dissipatedEnergy(const MaterialProperties& material,
                                                const ParameterSet& extPar,
                                                const MatLibArray& intPar0,
                                                MatLibArray& intPar1,
                                                const MatLibArray& gamma,
                                                const MatLibArray& gamDot,
                                                MatLibArray& tau1,MatLibArray& tau2,
                                                MatLibMatrix& H11,MatLibMatrix& H22,
                                                MatLibMatrix& H12,bool first,bool second) {
  // initialize to zero
  tau1 = tau2 = 0.0e0;
  H11 = H22 = H12 = 0.0e0;
  
  // get material parameters
  static const double PRECISION = 1.0e-16;
  double tau0 = material.getDoubleProperty("REFERENCE_PEIERLS_STRESS");
  if (tau0 < PRECISION) return 0.0e0;
  double gamDot0 = material.getDoubleProperty("REFERENCE_SLIP_RATE");
  
  // compute dissipation pseudo-potential and derivatives
  double phi = 0.0e0;
  double coef = 1.0e0/gamDot0;
  if (first) {
    for (unsigned int k=0; k < SingleCrystalBCC::NSYS; k++) {
      if (gamDot[k] <= 0.0e0) continue;
      double val = gamDot[k]*coef;
      tau2[k] = tau0*asinh(val);
      phi += gamDot0*(tau2[k]*val-tau0*std::sqrt(val*val+1.0e0)+tau0);
    }
  }
  else {
    for (unsigned int k=0; k < SingleCrystalBCC::NSYS; k++) {
      if (gamDot[k] <= 0.0e0) continue;
      double val = gamDot[k]*coef;
      phi += gamDot0*tau0*(val*asinh(val)-std::sqrt(val*val+1.0e0)+1.0e0);
    }
  }
  
  // compute second derivatives (diagonal matrix)
  if (second) {
    double coef1 = tau0*coef;
    for (unsigned int k=0; k < SingleCrystalBCC::NSYS; k++) {
      if (gamDot[k] <= 0.0e0) continue;
      double val = gamDot[k]*coef;
      H22[k][k] = coef1/std::sqrt(val*val+1.0e0);
    }
  }
  
  return phi;
}


/*
 * Methods for class CrystalHEPlasticityBCCBuilder.
 */

// the instance
CrystalHEPlasticityBCCBuilder const* CrystalHEPlasticityBCCBuilder::BUILDER 
= new CrystalHEPlasticityBCCBuilder();

// constructor
CrystalHEPlasticityBCCBuilder::CrystalHEPlasticityBCCBuilder() {
  ModelDictionary::add("BCC_CRYSTAL_FINITE_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CrystalHEPlasticityBCCBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CrystalHEPlasticityBCC3D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class PolyCrystalHEPlasticityBCCBuilder.
 */

// the instance
PolyCrystalHEPlasticityBCCBuilder const* PolyCrystalHEPlasticityBCCBuilder::BUILDER 
= new PolyCrystalHEPlasticityBCCBuilder();

// constructor
PolyCrystalHEPlasticityBCCBuilder::PolyCrystalHEPlasticityBCCBuilder() {
  ModelDictionary::add("BCC_POLY_CRYSTAL_FINITE_PLASTICITY",*this);
}

// build model
ConstitutiveModel* PolyCrystalHEPlasticityBCCBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new PolyCrystalHEPlasticityBCC3D();
      break;
    default:
      return 0;
      break;
  }
}
