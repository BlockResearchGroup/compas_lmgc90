/*
 *  $Id: CuitinoFCC.cpp 202 2016-03-31 11:51:40Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "CuitinoFCC.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>
// std C++ library
#include <limits>


/*
 * Methods for FCCHardeningCuitino
 */

// check consistency of material properties
void FCCHardeningCuitino::checkProperties(MaterialProperties& material,
                                          std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***FCC hardening model (Cuitino style)***" << std::endl;

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
    
  double b = material.getDoubleProperty("BURGERS_VECTOR");
  if (b <= 0.0e0) {
    if (os) (*os) << "ERROR: Burgers vector must be positive." << std::endl;
    throw InvalidPropertyException("Burgers vector");
  }

  double U = material.getDoubleProperty("DISLOCATION_SELF_ENERGY");
  if (U <= 0.0e0) {
    if (os) (*os) << "ERROR: dislocation line self-energy must be positive." << std::endl;
    throw InvalidPropertyException("dislocation line self-energy");
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

  double a0 = material.getDoubleProperty("INTERACTION_COEFFICIENT_0");
  if (a0 <= 0.0e0) {
    if (os) (*os) << "ERROR: interaction coefficient must be positive." << std::endl;
    throw InvalidPropertyException("interaction coefficient a0");
  }

  double a1 = material.getDoubleProperty("INTERACTION_COEFFICIENT_1");
  if (a1 <= 0.0e0) {
    if (os) (*os) << "ERROR: interaction coefficient must be positive." << std::endl;
    throw InvalidPropertyException("interaction coefficient a1");
  }

  double a2 = material.getDoubleProperty("INTERACTION_COEFFICIENT_2");
  if (a2 <= 0.0e0) {
    if (os) (*os) << "ERROR: interaction coefficient must be positive." << std::endl;
    throw InvalidPropertyException("interaction coefficient a2");
  }

  double a3 = material.getDoubleProperty("INTERACTION_COEFFICIENT_3");
  if (a3 <= 0.0e0) {
    if (os) (*os) << "ERROR: interaction coefficient must be positive." << std::endl;
    throw InvalidPropertyException("interaction coefficient a3");
  }

  // print-out properties
  if (os) {
    (*os) << "\tinitial yield stress (stored part)     = " <<   g0   << std::endl;
    (*os) << "\tinitial yield stress (dissipated part) = " <<   g1   << std::endl;
    (*os) << "\tBurgers vector                         = " <<   b    << std::endl;
    (*os) << "\tdislocation line self-energy           = " <<   U    << std::endl;
    (*os) << "\tinitial dislocation density            = " << rho0   << std::endl;
    (*os) << "\tsaturation dislocation density         = " << rhoSat << std::endl;
    (*os) << "\tsaturation strain                      = " << gamSat << std::endl;
    (*os) << "\tinteraction coefficient a0             = " <<   a0   << std::endl;
    (*os) << "\tinteraction coefficient a1/a0          = " <<   a1   << std::endl;
    (*os) << "\tinteraction coefficient a2/a0          = " <<   a2   << std::endl;
    (*os) << "\tinteraction coefficient a3/a0          = " <<   a3   << std::endl;
  }
  
  // compute interaction matrix
  StdProperty<MatLibMatrix> inter;
  inter.value().resize(SingleCrystalFCC::NSYS/2);
  MatLibMatrix& aMat = inter.value();
  // CAUTION: this is dependent on the order of slip systems (defined elsewhere)
  a1 *= a0;
  a2 *= a0;
  a3 *= a0;
  // A2
  aMat[ 0][ 0] = a0;
  aMat[ 0][ 1] = a1;
  aMat[ 0][ 2] = a1;
  aMat[ 0][ 3] = a1;
  aMat[ 0][ 4] = a2;
  aMat[ 0][ 5] = a2;
  aMat[ 0][ 6] = a1;
  aMat[ 0][ 7] = a2;
  aMat[ 0][ 8] = a3;
  aMat[ 0][ 9] = a1;
  aMat[ 0][10] = a3;
  aMat[ 0][11] = a2;
  // A3
  aMat[ 1][ 1] = a0;
  aMat[ 1][ 2] = a1;
  aMat[ 1][ 3] = a2;
  aMat[ 1][ 4] = a1;
  aMat[ 1][ 5] = a3;
  aMat[ 1][ 6] = a2;
  aMat[ 1][ 7] = a1;
  aMat[ 1][ 8] = a2;
  aMat[ 1][ 9] = a3;
  aMat[ 1][10] = a1;
  aMat[ 1][11] = a2;
  // A6
  aMat[ 2][ 2] = a0;
  aMat[ 2][ 3] = a2;
  aMat[ 2][ 4] = a3;
  aMat[ 2][ 5] = a1;
  aMat[ 2][ 6] = a3;
  aMat[ 2][ 7] = a2;
  aMat[ 2][ 8] = a1;
  aMat[ 2][ 9] = a2;
  aMat[ 2][10] = a2;
  aMat[ 2][11] = a1;
  // B2
  aMat[ 3][ 3] = a0;
  aMat[ 3][ 4] = a1;
  aMat[ 3][ 5] = a1;
  aMat[ 3][ 6] = a1;
  aMat[ 3][ 7] = a3;
  aMat[ 3][ 8] = a2;
  aMat[ 3][ 9] = a1;
  aMat[ 3][10] = a2;
  aMat[ 3][11] = a3;
  // B4
  aMat[ 4][ 4] = a0;
  aMat[ 4][ 5] = a1;
  aMat[ 4][ 6] = a3;
  aMat[ 4][ 7] = a1;
  aMat[ 4][ 8] = a2;
  aMat[ 4][ 9] = a2;
  aMat[ 4][10] = a1;
  aMat[ 4][11] = a2;
  // B5
  aMat[ 5][ 5] = a0;
  aMat[ 5][ 6] = a2;
  aMat[ 5][ 7] = a2;
  aMat[ 5][ 8] = a1;
  aMat[ 5][ 9] = a3;
  aMat[ 5][10] = a2;
  aMat[ 5][11] = a1;
  // C1
  aMat[ 6][ 6] = a0;
  aMat[ 6][ 7] = a1;
  aMat[ 6][ 8] = a1;
  aMat[ 6][ 9] = a1;
  aMat[ 6][10] = a2;
  aMat[ 6][11] = a2;
  // C3
  aMat[ 7][ 7] = a0;
  aMat[ 7][ 8] = a1;
  aMat[ 7][ 9] = a2;
  aMat[ 7][10] = a1;
  aMat[ 7][11] = a3;
  // C5
  aMat[ 8][ 8] = a0;
  aMat[ 8][ 9] = a2;
  aMat[ 8][10] = a3;
  aMat[ 8][11] = a1;
  // D1
  aMat[ 9][ 9] = a0;
  aMat[ 9][10] = a1;
  aMat[ 9][11] = a1;
  // D4
  aMat[10][10] = a0;
  aMat[10][11] = a1;
  // D6
  aMat[11][11] = a0;
  // lower part
  for (unsigned int k=1; k < SingleCrystalFCC::NSYS/2; k++)
    for (unsigned int l=0; l < k; l++) aMat[k][l] = aMat[l][k];

  material.setProperty("INTERACTION_MATRIX",inter);
}

// plastically stored energy
double FCCHardeningCuitino::storedEnergy(const MaterialProperties& material,
                                         const ParameterSet& extPar,
                                         const MatLibArray& intPar0,
                                         MatLibArray& intPar1,double Wp0,
                                         const MatLibArray& gamma0,
                                         const MatLibArray& gamma1,
                                         MatLibArray& tau,MatLibMatrix& H,
                                         bool first,bool second) {
  
  // compute hardening matrix
  static MatLibArray hDiag(SingleCrystalFCC::NSYS);
  if (initialize) {
    hardeningMatrix(material,gamma0,intPar0,hDiag);
    initialize = false;
  }
  
  // compute plastic energy
  double Wp = Wp0;
  double g00 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
  double dGam,dg;
  if (first) {
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) {
      dGam = gamma1[k]-gamma0[k];
      dg = hDiag[k]*dGam;
      tau[k] = g00+intPar0[k]+dg;
      Wp += (g00+intPar0[k]+0.5*dg)*dGam;
    }
  }
  else {
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) {
      dGam = gamma1[k]-gamma0[k];
      Wp += (g00+intPar0[k]+0.5*hDiag[k]*dGam)*dGam;
    }
  }
  
  if (second) {
    H = 0.0e0;
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) H[k][k] = hDiag[k];
  }
  
  // update internal parameters
  if (finalize) {
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) {
      dGam = gamma1[k]-gamma0[k];
      dg = hDiag[k]*dGam;
      intPar1[k] = intPar0[k]+dg;
    }
  }
  
  return Wp;
}

// yield stress
void FCCHardeningCuitino::yieldStress(const MaterialProperties& material,
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
  
  // set CRSS on all systems
  tau = g1;
  
  // tangents
  if (first) H = 0.0e0;
  if (second) {
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) dH[k] = 0.0e0;
  }
}

// The heart of the model: the hardening matrix.
void FCCHardeningCuitino::hardeningMatrix(const MaterialProperties& material,
                                          const MatLibArray& gamma,
                                          const MatLibArray& g,MatLibArray& hDiag) {
  
  static const double PI = 4*std::atan(1.0e0);
  static const double THRSHLD 
    = 1.05*std::sqrt(1.0e0/std::log(std::numeric_limits<double>::max()));
  
  // compute dislocation densities
  MatLibArray rho(SingleCrystalFCC::NSYS/2);
  double rhoInit = material.getDoubleProperty("INITIAL_DISLOCATION_DENSITY");
  double rhoSat = material.getDoubleProperty("SATURATION_DISLOCATION_DENSITY");
  double dRho = rhoSat-rhoInit;
  double gamSat = material.getDoubleProperty("SATURATION_STRAIN");
  double coef = 1.0e0/gamSat;
  
  unsigned int k,l,nSys2 = SingleCrystalFCC::NSYS/2;
  for (k=0, l=nSys2; k < nSys2; k++, l++) {
    double gamP = gamma[k];
    double gamN = gamma[l];
    double gamT = gamP+gamN;
    rho[k] = rhoSat-dRho*std::exp(-gamT*coef);
  }
  
  // compute obstacle densities
  MatLibArray nObst(SingleCrystalFCC::NSYS/2);
  StdProperty<MatLibMatrix>& inter
    = dynamic_cast<StdProperty<MatLibMatrix>&>(material.getProperty("INTERACTION_MATRIX"));
  nObst = inter.value()*rho;
  
  // compute hardening matrix
  double burgers = material.getDoubleProperty("BURGERS_VECTOR");
  double Uedge = material.getDoubleProperty("DISLOCATION_SELF_ENERGY");
  coef = Uedge/burgers*std::sqrt(PI);
  double g0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
  
  double gc,tc,hc,rn;
  for (k=0; k < SingleCrystalFCC::NSYS; k++) {
    unsigned int kk = (k < nSys2) ? k:(k-nSys2);
    rn = std::sqrt(nObst[kk]);
    tc = coef*rn;
    gc = 0.5*burgers*rho[kk]/rn;
    hc = tc/gc;
    double r = (g0+g[k])/tc;
    if (r < THRSHLD) r = THRSHLD;
    double r2 = r*r;
    hDiag[k] = hc*r2*r*(std::cosh(1.0e0/r2)-1.0e0);
  }
}


/*
 * Methods for class FCCRateDependency.
 */

// check consistency of material properties
void FCCRateDependency::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***FCC rate dependency model (power law)***" << std::endl;

  double tau0;
  try {
    tau0 = material.getDoubleProperty("REFERENCE_STRESS");
    if (tau0 < 0.0e0) {
      if (os) (*os) << "ERROR: reference stress must be positive or zero." << std::endl;
      throw InvalidPropertyException("reference stress");
    }
  }
  catch (NoSuchPropertyException) {
    tau0 = 0.0e0;
    material.setProperty("REFERENCE_STRESS",tau0);
    return;
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
double FCCRateDependency::dissipatedEnergy(const MaterialProperties& material,
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
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) {
      if (gamDot[k] <= 0.0e0) continue;
      double val = gamDot[k]*coef1;
      tau2[k] = tau0*std::pow(val,expo);
      phi += coef2*tau2[k]*gamDot[k];
    }
  }
  else {
    double expo1 = expo+1;
    double coef0 = tau0*gamDot0*coef2;
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) {
      if (gamDot[k] <= 0.0e0) continue;
      double val = gamDot[k]*coef1;
      phi += coef0*std::pow(val,expo1);
    }
  }
  
  // compute second derivatives (diagonal matrix)
  if (second) {
    double expo2 = expo-1;
    double coef3 = expo*tau0/gamDot0;
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) {
      if (gamDot[k] <= 0.0e0) continue;
      double val = gamDot[k]*coef1;
      H22[k][k] = coef3*std::pow(val,expo2);
    }
  }
  
  return phi;
}


/*
 * Methods for class CrystalHEPlasticityFCCBuilder.
 */

// the instance
CrystalHEPlasticityFCCBuilder const* CrystalHEPlasticityFCCBuilder::BUILDER 
= new CrystalHEPlasticityFCCBuilder();

// constructor
CrystalHEPlasticityFCCBuilder::CrystalHEPlasticityFCCBuilder() {
  ModelDictionary::add("FCC_CRYSTAL_FINITE_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CrystalHEPlasticityFCCBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CrystalHEPlasticityFCC3D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class CrystalPlasticityFCCBuilder.
 */

// the instance
CrystalPlasticityFCCBuilder const* CrystalPlasticityFCCBuilder::BUILDER 
= new CrystalPlasticityFCCBuilder();

// constructor
CrystalPlasticityFCCBuilder::CrystalPlasticityFCCBuilder() {
  ModelDictionary::add("FCC_CRYSTAL_PLASTICITY",*this);
}

// build model
ConstitutiveModel* CrystalPlasticityFCCBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CrystalPlasticityFCC3D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class PolyCrystalHEPlasticityFCCBuilder.
 */

// the instance
PolyCrystalHEPlasticityFCCBuilder const* PolyCrystalHEPlasticityFCCBuilder::BUILDER 
= new PolyCrystalHEPlasticityFCCBuilder();

// constructor
PolyCrystalHEPlasticityFCCBuilder::PolyCrystalHEPlasticityFCCBuilder() {
  ModelDictionary::add("FCC_POLY_CRYSTAL_FINITE_PLASTICITY",*this);
}

// build model
ConstitutiveModel* PolyCrystalHEPlasticityFCCBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new PolyCrystalHEPlasticityFCC3D();
      break;
    default:
      return 0;
      break;
  }
}
