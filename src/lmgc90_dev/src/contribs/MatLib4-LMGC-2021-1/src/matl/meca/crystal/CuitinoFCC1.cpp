/*
 *  $Id: CuitinoFCC1.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "CuitinoFCC1.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>
// std C++ library
#include <limits>


/*
 * Methods for FCCHardeningCuitino1
 */

// check consistency of material properties
void FCCHardeningCuitino1::checkProperties(MaterialProperties& material,
                                           std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***FCC hardening model (Cuitino style - new version)***" << std::endl;

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
    g0 = 0.0e0;
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
    g1 = material.getDoubleProperty("INITIAL_YIELD_STRESS");
    if (g1 < 0.0e0) {
      if (os) (*os) << "ERROR: initial yield stress must be positive or zero." << std::endl;
      throw InvalidPropertyException("initial yield stress");
    }
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
double FCCHardeningCuitino1::storedEnergy(const MaterialProperties& material,
                                          const ParameterSet& extPar,
                                          const MatLibArray& intPar0,
                                          MatLibArray& intPar1,double Wp0,
                                          const MatLibArray& gamma0,
                                          const MatLibArray& gamma1,
                                          MatLibArray& tau,MatLibMatrix& H,
                                          bool first,bool second) {
  
  // compute plastic energy
  double Wp = Wp0;
  double g00 = material.getDoubleProperty("INITIAL_YIELD_STRESS_STORED");
  if (first) {
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) {
      tau[k] = g00;
      Wp += g00*(gamma1[k]-gamma0[k]);
    }
  }
  else {
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) {
      Wp += g00*(gamma1[k]-gamma0[k]);
    }
  }
  
  if (second) {
    H = 0.0e0;
  }
  
  return Wp;
}

// yield stress
void FCCHardeningCuitino1::yieldStress(const MaterialProperties& material,
                                       const ParameterSet& extPar,
                                       const MatLibArray& intPar0,
                                       MatLibArray& intPar1,
                                       const MatLibArray& gamma0,
                                       const MatLibArray& gamma1,
                                       const MatLibArray& gamma,
                                       MatLibArray& tau,MatLibMatrix& H,
                                       std::vector<MatLibMatrix>& dH,
                                       bool first,bool second) {
  static const unsigned int MAXITER = 50;
  static const double PI = 4*std::atan(1.0e0);
  static const double THRSHLD_MIN 
    = std::sqrt(2.0e0/std::log(std::numeric_limits<double>::max()));
  static const double THRSHLD_MAX = std::sqrt(std::numeric_limits<double>::max());

  // compute dislocation densities
  MatLibArray rho(SingleCrystalFCC::NSYS/2);
  MatLibArray dRhoGam(SingleCrystalFCC::NSYS/2);
  MatLibArray d2RhoGam(SingleCrystalFCC::NSYS/2);
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
    double val = dRho*std::exp(-gamT*coef);
    rho[k] = rhoSat-val;
    if (first || second) dRhoGam[k] = val*coef;
    if (second) d2RhoGam[k] = -dRhoGam[k]*coef;
  }
  
  // compute obstacle densities
  MatLibArray nObst(SingleCrystalFCC::NSYS/2);
  StdProperty<MatLibMatrix>& inter
    = dynamic_cast<StdProperty<MatLibMatrix>&>(material.getProperty("INTERACTION_MATRIX"));
  MatLibMatrix& aMat = inter.value();
  nObst = aMat*rho;
  
  // get initial yield stress
  double g0 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  
  // compute CRSS on all systems
  double burgers = material.getDoubleProperty("BURGERS_VECTOR");
  double Uedge = material.getDoubleProperty("DISLOCATION_SELF_ENERGY");
  coef = Uedge/burgers*std::sqrt(PI);
  MatLibArray HLocal(SingleCrystalFCC::NSYS/2);
  MatLibArray dHdGam(SingleCrystalFCC::NSYS/2);
  MatLibArray d2HDiagdGam(SingleCrystalFCC::NSYS/2);
  for (k=0, l=nSys2; k < SingleCrystalFCC::NSYS/2; k++, l++) {
    
    double dGamP = gamma[k]-gamma0[k];
    double dGamN = gamma[l]-gamma0[l];
    double dGam = dGamP+dGamN;
    double rn = std::sqrt(nObst[k]);
    double tc = coef*rn;
    double gc = 0.5*burgers*rho[k]/rn;
    double hc = tc/gc;
    
    // initial guess
    double g1 = intPar0[k];
    
    // iterative search
    unsigned int iter;
    double test = 1.0e-6*(g0+g1);
    double dg,r,r2,valc,vals,hDiag,dHDiag;
    for (iter=0; iter < MAXITER; iter++) {
      r = (g0+g1)/tc;
      if (r < THRSHLD_MIN) r = THRSHLD_MIN;
      r2 = r*r;
      vals = std::sinh(1.0e0/r2);
      valc = std::sqrt(1.0e0+vals*vals); /* =cosh() */
      hDiag = hc*r2*r*(valc-1.0e0);
      if (hDiag > THRSHLD_MAX) {
        hDiag = THRSHLD_MAX;
        dHDiag = 0.0e0;
      }
      else
        dHDiag = (3*r2*(valc-1.0e0)-2*vals)/gc;
      dg = (intPar0[k]+hDiag*dGam-g1)/(1.0e0-dHDiag*dGam);
      if (std::fabs(dg) < test) break;
      
      g1 += dg;
    }
    if (iter == MAXITER) {
      std::cout << "no convergence in forest hardening model" << std::endl;
      throw UpdateFailedException("no convergence in forest hardening model");
    }
    
    tau[k] = g0+g1;
    tau[l] = g0+g1;
    if (finalize) 
      intPar1[k] = intPar0[k]+hDiag*(gamma1[k]+gamma1[l]-gamma0[k]-gamma0[l]);
    
    // tangents
    double coef1=0.0e0;
    if (first || second) {
      double val = (hc*r*vals-0.5*hDiag)/nObst[k];
      for (unsigned int m=0; m < nSys2; m++) {
        dHdGam[m] = val*aMat[k][m]*dRhoGam[m];
      }
      dHdGam[k] -= hDiag/rho[k]*dRhoGam[k];
      
      for (unsigned int m=0; m < nSys2; m++) {
        HLocal[m] = dHdGam[m]*dGam;
      }
      HLocal[k] += hDiag;

      coef1 = 1.0e0/(1.0e0-dHDiag*dGam);
      HLocal *= coef1;
    }
    if (first) {
      for (unsigned int m=0; m < SingleCrystalFCC::NSYS; m++) {
        unsigned int mm = (m < nSys2) ? m : m-nSys2;
        H[k][m] = HLocal[mm];
        H[l][m] = HLocal[mm];
      }
    }
    
    if (second) {
      unsigned int m,n;
      double d2HDiag = (6.0*r2*(valc-1.0)+4*valc/r2-6.0*vals)/(g0+g1)/gc;
      double coef2 = d2HDiag*dGam;

      double val = (-1.5*r2*(valc-1.0)+2*vals-2*valc/r2)*dGam/gc/nObst[k];
      for (m=0; m < nSys2; m++)
        d2HDiagdGam[m] = val*aMat[k][m]*dRhoGam[m];
      d2HDiagdGam[k] -= (3*r2*(valc-1.0)-2*vals)*dGam/gc/rho[k]*dRhoGam[k];
      
      dH[k] = 0.0e0;
      dH[l] = 0.0e0;
      val = (0.75*hDiag-hc*r*vals+hc*valc/r)*dGam/(nObst[k]*nObst[k]);
      for (m=0; m < nSys2; m++) {
        double coef3 = aMat[k][m]*dRhoGam[m];
        for (n=0; n < nSys2; n++) {
          double val1 = val*coef3*aMat[k][n]*dRhoGam[n]
                       +coef2*HLocal[m]*HLocal[n]
                       +d2HDiagdGam[m]*HLocal[n]+HLocal[m]*d2HDiagdGam[n];
          dH[k][m      ][n]       += val1;
          dH[k][m      ][n+nSys2] += val1;
          dH[k][m+nSys2][n]       += val1;
          dH[k][m+nSys2][n+nSys2] += val1;

          dH[l][m      ][n]       += val1;
          dH[l][m      ][n+nSys2] += val1;
          dH[l][m+nSys2][n]       += val1;
          dH[l][m+nSys2][n+nSys2] += val1;
        }
        double val2 = (0.5*hDiag-hc*r*vals)*dGam/nObst[k];
        double val3 = val2*coef3*dRhoGam[k]/rho[k];
        dH[k][m      ][k]       += val3;
        dH[k][m      ][l]       += val3;
        dH[k][m+nSys2][k]       += val3;
        dH[k][m+nSys2][l]       += val3;
        dH[k][k      ][m]       += val3;
        dH[k][k      ][m+nSys2] += val3;
        dH[k][l      ][m]       += val3;
        dH[k][l      ][m+nSys2] += val3;
        dH[l][m      ][k]       += val3;
        dH[l][m      ][l]       += val3;
        dH[l][m+nSys2][k]       += val3;
        dH[l][m+nSys2][l]       += val3;
        dH[l][k      ][m]       += val3;
        dH[l][k      ][m+nSys2] += val3;
        dH[l][l      ][m]       += val3;
        dH[l][l      ][m+nSys2] += val3;
        double val4 = val2*aMat[k][m]*d2RhoGam[m];
        dH[k][m      ][m]       -= val4;
        dH[k][m      ][m+nSys2] -= val4;
        dH[k][m+nSys2][m]       -= val4;
        dH[k][m+nSys2][m+nSys2] -= val4;
        dH[l][m      ][m]       -= val4;
        dH[l][m      ][m+nSys2] -= val4;
        dH[l][m+nSys2][m]       -= val4;
        dH[l][m+nSys2][m+nSys2] -= val4;
        double val5 = dHdGam[m]+dHDiag*HLocal[m];
        dH[k][m][k] += val5;
        dH[k][m][l] += val5;
        dH[k][k][m] += val5;
        dH[k][l][m] += val5;
        dH[k][m+nSys2][k] += val5;
        dH[k][m+nSys2][l] += val5;
        dH[k][k][m+nSys2] += val5;
        dH[k][l][m+nSys2] += val5;
        dH[l][m][k] += val5;
        dH[l][m][l] += val5;
        dH[l][k][m] += val5;
        dH[l][l][m] += val5;
        dH[l][m+nSys2][k] += val5;
        dH[l][m+nSys2][l] += val5;
        dH[l][k][m+nSys2] += val5;
        dH[l][l][m+nSys2] += val5;
      }
      double val6 = hDiag*dGam*(2*dRhoGam[k]*dRhoGam[k]/rho[k]
                                -d2RhoGam[k])/rho[k];
      dH[k][k][k] += val6;
      dH[k][k][l] += val6;
      dH[k][l][k] += val6;
      dH[k][l][l] += val6;
      dH[l][k][k] += val6;
      dH[l][k][l] += val6;
      dH[l][l][k] += val6;
      dH[l][l][l] += val6;
      
      dH[k] *= coef1;
      dH[l] *= coef1;
    }
  }
}


/*
 * Methods for class CrystalHEPlasticityFCC1Builder.
 */

// the instance
CrystalHEPlasticityFCC1Builder const* CrystalHEPlasticityFCC1Builder::BUILDER 
= new CrystalHEPlasticityFCC1Builder();

// constructor
CrystalHEPlasticityFCC1Builder::CrystalHEPlasticityFCC1Builder() {
  ModelDictionary::add("FCC_CRYSTAL_FINITE_PLASTICITY_NEW",*this);
}

// build model
ConstitutiveModel* CrystalHEPlasticityFCC1Builder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CrystalHEPlasticityFCCNew3D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class CrystalPlasticityFCC1Builder.
 */

// the instance
CrystalPlasticityFCC1Builder const* CrystalPlasticityFCC1Builder::BUILDER 
= new CrystalPlasticityFCC1Builder();

// constructor
CrystalPlasticityFCC1Builder::CrystalPlasticityFCC1Builder() {
  ModelDictionary::add("FCC_CRYSTAL_PLASTICITY_NEW",*this);
}

// build model
ConstitutiveModel* CrystalPlasticityFCC1Builder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CrystalPlasticityFCCNew3D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class PolyCrystalHEPlasticityFCC1Builder.
 */

// the instance
PolyCrystalHEPlasticityFCC1Builder const* PolyCrystalHEPlasticityFCC1Builder::BUILDER 
= new PolyCrystalHEPlasticityFCC1Builder();

// constructor
PolyCrystalHEPlasticityFCC1Builder::PolyCrystalHEPlasticityFCC1Builder() {
  ModelDictionary::add("FCC_POLY_CRYSTAL_FINITE_PLASTICITY_NEW",*this);
}

// build model
ConstitutiveModel* PolyCrystalHEPlasticityFCC1Builder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new PolyCrystalHEPlasticityFCCNew3D();
      break;
    default:
      return 0;
      break;
  }
}
