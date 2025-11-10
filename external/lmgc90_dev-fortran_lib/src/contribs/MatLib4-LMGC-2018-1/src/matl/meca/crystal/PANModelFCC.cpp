/*
 *  $Id: PANModelFCC.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "PANModelFCC.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>


/*
 * Methods for FCCHardeningPAN
 */

// check consistency of material properties
void FCCHardeningPAN::checkProperties(MaterialProperties& material,
                                      std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***FCC hardening model (PAN style)***" << std::endl;

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
  
  double gs = material.getDoubleProperty("SATURATION_YIELD_STRESS");
  if (gs < 0.0e0) {
    if (os) (*os) << "ERROR: saturation yield stress must be positive or zero." << std::endl;
    throw InvalidPropertyException("saturation yield stress");
  }
  
  double h0 = material.getDoubleProperty("INITIAL_HARDENING_MODULUS");
  if (h0 <= 0.0e0) {
    if (os) (*os) << "ERROR: initial hardening rate must be positive." << std::endl;
    throw InvalidPropertyException("initial hardening rate");
  }

  double hs;
  try {
    hs = material.getDoubleProperty("SATURATION_HARDENING_MODULUS");
    if (hs < 0.0e0) {
      if (os) (*os) << "ERROR: saturation hardening rate must be positive or zero." << std::endl;
      throw InvalidPropertyException("saturation hardening rate");
    }
  }
  catch (NoSuchPropertyException) {
    hs = 0.0e0;
    material.setProperty("SATURATION_HARDENING_MODULUS",hs);
  }
   
  double q = material.getDoubleProperty("LATENT_HARDENING_COEFFICIENT");
  if (q < 0.0e0) {
    if (os) (*os) << "ERROR: latent hardening coefficient must be positive or zero." << std::endl;
    throw InvalidPropertyException("latent hardening coefficient");
  }

  // print-out properties
  if (os) {
    (*os) << "\tinitial yield stress (stored part)     = " <<   g0   << std::endl;
    (*os) << "\tinitial yield stress (dissipated part) = " <<   g1   << std::endl;
    (*os) << "\tsaturation yield stress                = " <<   gs   << std::endl;
    (*os) << "\tinitial hardening rate                 = " <<   h0   << std::endl;
    (*os) << "\tsaturation hardening rate              = " <<   hs   << std::endl;
    (*os) << "\tlatent hardening coefficient           = " <<   q    << std::endl;
  }
}

// plastically stored energy
double FCCHardeningPAN::storedEnergy(const MaterialProperties& material,
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
void FCCHardeningPAN::yieldStress(const MaterialProperties& material,
                                  const ParameterSet& extPar,
                                  const MatLibArray& intPar0,
                                  MatLibArray& intPar1,
                                  const MatLibArray& gamma0,
                                  const MatLibArray& gamma1,
                                  const MatLibArray& gamma,
                                  MatLibArray& tau,MatLibMatrix& H,
                                  std::vector<MatLibMatrix>& dH,
                                  bool first,bool second) {
  
  // compute hardening moduli
  MatLibArray h(SingleCrystalFCC::NSYS/2);
  MatLibMatrix dh(SingleCrystalFCC::NSYS/2);
  std::vector<MatLibMatrix> d2h(SingleCrystalFCC::NSYS/2,dh);
  hardeningModuli(material,gamma,intPar0,h,dh,d2h,first || second,second);
  
  // get yield stress
  double g1 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  double q  = material.getDoubleProperty("LATENT_HARDENING_COEFFICIENT");
  
  // compute CRSS on all systems
  unsigned int nSys2 = SingleCrystalFCC::NSYS/2;
  MatLibArray dGam(SingleCrystalFCC::NSYS/2);
  for (unsigned int k=0; k < nSys2; k++) {
    dGam[k] = gamma[k]-gamma0[k]
             +gamma[k+nSys2]-gamma0[k+nSys2];
  }
  for (unsigned int k=0; k < nSys2; k++) {
    unsigned int idx = k/3;
    double val = g1+intPar0[k];
    for (unsigned int l=0; l < nSys2; l++) {
      if (l/3 == idx)
        val += h[l]*dGam[l];
      else
        val += q*h[l]*dGam[l];
    }
    tau[k]       = val;
    tau[k+nSys2] = val;
    if (finalize) intPar1[k] = val-g1;
  }
  
  // tangents
  if (first) {
    H = 0.0e0;
    for (unsigned int k=0; k < nSys2; k++) {
      unsigned int idx = k/3;
      for (unsigned int l=0; l < nSys2; l++) {
        double val;
        if (l/3 == idx)
          val = h[l];
        else
          val = q*h[l];

        for (unsigned int m=0; m < nSys2; m++) {
          if (m/3 == idx)
            val += dh[m][l]*dGam[m];
          else
            val += q*dh[m][l]*dGam[m];
        }

        H[k      ][l]       = val;
        H[k      ][l+nSys2] = val;
        H[k+nSys2][l]       = val;
        H[k+nSys2][l+nSys2] = val;
      }
    }
  }
  if (second) {
    for (unsigned int k=0; k < nSys2; k++) {
      unsigned int idx = k/3;
      dH[k]       = 0.0e0;
      dH[k+nSys2] = 0.0e0;
      for (unsigned int l=0; l < nSys2; l++)
        for (unsigned int m=0; m < nSys2; m++) {
          double val = 0.0e0;
          if (l/3 == idx)
            val += dh[l][m];
          else
            val += q*dh[l][m];
          if (m/3 == idx)
            val += dh[m][l];
          else
            val += q*dh[m][l];
          
          for (unsigned int n=0; n < nSys2; n++) {
            if (n/3 == idx)
              val += d2h[n][l][m]*dGam[n];
            else
              val += q*d2h[n][l][m]*dGam[n];
          }

          dH[k][l      ][m]       = val;
          dH[k][l      ][m+nSys2] = val;
          dH[k][l+nSys2][m]       = val;
          dH[k][l+nSys2][m+nSys2] = val;
          dH[k+nSys2][l      ][m]       = val;
          dH[k+nSys2][l      ][m+nSys2] = val;
          dH[k+nSys2][l+nSys2][m]       = val;
          dH[k+nSys2][l+nSys2][m+nSys2] = val;
        }
    }
  }
}

// The heart of the model: the hardening moduli.
void FCCHardeningPAN::hardeningModuli(const MaterialProperties& material,
                                      const MatLibArray& gamma,const MatLibArray& g,
                                      MatLibArray& h,MatLibMatrix& dh,
                                      std::vector<MatLibMatrix>& d2h,
                                      bool first,bool second) {
  // material properties
  double h0 = material.getDoubleProperty("INITIAL_HARDENING_MODULUS");
  double hs = material.getDoubleProperty("SATURATION_HARDENING_MODULUS");
  double g1 = material.getDoubleProperty("INITIAL_YIELD_STRESS_DISSIPATED");
  double gs = material.getDoubleProperty("SATURATION_YIELD_STRESS");

  // compute total accumulated slip
  double gam = 0.0e0;
  for (unsigned int k=0; k < SingleCrystalFCC::NSYS; k++) gam += gamma[k];

  // compute hardening moduli
  double coef = (h0-hs)/(gs-g1);
  double val0 = coef*gam;
  double val1 = std::cosh(val0);
  double val2 = val1*val1;
  double val = (h0-hs)/val2;
  double valh = val+hs;
  for (unsigned int k=0; k < SingleCrystalFCC::NSYS/2; k++) h[k] = valh;

  // and derivatives
  double val3 = 0.0e0;
  if (first || second) val3 = std::tanh(val0);
  if (first) {
    double val4 = -2*coef*val3*val;
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS/2; k++)
      for (unsigned int l=0; l < SingleCrystalFCC::NSYS/2; l++) dh[k][l] = val4;
  }
  if (second) {
    double val4 = -2*coef*coef*(1.0-3*val3*val3)*val;
    for (unsigned int k=0; k < SingleCrystalFCC::NSYS/2; k++)
      for (unsigned int l=0; l < SingleCrystalFCC::NSYS/2; l++)
        for (unsigned int m=0; m < SingleCrystalFCC::NSYS/2; m++) d2h[k][l][m] = val4;
  }
}


/*
 * Methods for class PANCrystalHEPlasticityFCCBuilder.
 */

// the instance
PANCrystalHEPlasticityFCCBuilder const* PANCrystalHEPlasticityFCCBuilder::BUILDER 
= new PANCrystalHEPlasticityFCCBuilder();

// constructor
PANCrystalHEPlasticityFCCBuilder::PANCrystalHEPlasticityFCCBuilder() {
  ModelDictionary::add("FCC_CRYSTAL_FINITE_PLASTICITY_PAN",*this);
}

// build model
ConstitutiveModel* PANCrystalHEPlasticityFCCBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new PANCrystalHEPlasticityFCC3D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class PANPolyCrystalHEPlasticityFCCBuilder.
 */

// the instance
PANPolyCrystalHEPlasticityFCCBuilder const* PANPolyCrystalHEPlasticityFCCBuilder::BUILDER 
= new PANPolyCrystalHEPlasticityFCCBuilder();

// constructor
PANPolyCrystalHEPlasticityFCCBuilder::PANPolyCrystalHEPlasticityFCCBuilder() {
  ModelDictionary::add("FCC_POLY_CRYSTAL_FINITE_PLASTICITY_PAN",*this);
}

// build model
ConstitutiveModel* PANPolyCrystalHEPlasticityFCCBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new PANPolyCrystalHEPlasticityFCC3D();
      break;
    default:
      return 0;
      break;
  }
}
