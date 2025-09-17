/*
 *  $Id: SmithFerranteDamagePotential.cpp 245 2017-07-20 12:49:41Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "SmithFerranteDamagePotential.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class SmithFerranteDamagePotential.
 */
// check consistency of material properties
void SmithFerranteDamagePotential::checkProperties(MaterialProperties& material,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***Smith-Ferrante isotropic damage potential***" << std::endl;
   
  // Young's modulus (must have been defined in elasticity)
  double E = material.getDoubleProperty("YOUNG_MODULUS");

  // get critical stress
  const double e = 2.71828182845904523536e0;
  double epsC,sigC,Ec;
  try {
    sigC = material.getDoubleProperty("CRITICAL_EQUIVALENT_STRESS");
    if (sigC <= 0.0e0) {
      if (os) (*os) << "ERROR: critical equivalent stress must be positive." << std::endl;
      throw InvalidPropertyException("critical equivalent stress");
    }
    epsC = e*sigC/E;
    Ec = e*sigC*epsC;
    material.setProperty("CRITICAL_EQUIVALENT_STRAIN",epsC);
    material.setProperty("CRITICAL_ENERGY_DENSITY",Ec);
  }
  catch (NoSuchPropertyException) {
    try {
      epsC = material.getDoubleProperty("CRITICAL_EQUIVALENT_STRAIN");
      if (epsC <= 0.0e0) {
        if (os) (*os) << "ERROR: critical equivalent strain must be positive." << std::endl;
        throw InvalidPropertyException("critical equivalent stress");
      }
      sigC = E*epsC/e;
      Ec = sigC*epsC*e;
      material.setProperty("CRITICAL_EQUIVALENT_STRESS",sigC);
      material.setProperty("CRITICAL_ENERGY_DENSITY",Ec);
    }
    catch (NoSuchPropertyException) {
      try {
        Ec = material.getDoubleProperty("CRITICAL_ENERGY_DENSITY");
        if (Ec <= 0.0e0) {
          if (os) (*os) << "ERROR: critical energy must be positive." << std::endl;
          throw InvalidPropertyException("critical energy");
        }
        epsC = std::sqrt(Ec/E);
        sigC = E*epsC/e;
        material.setProperty("CRITICAL_EQUIVALENT_STRAIN",epsC);
        material.setProperty("CRITICAL_EQUIVALENT_STRESS",sigC);
      }
      catch (NoSuchPropertyException ex) {
        if (os) (*os) << "ERROR: critical energy density is not defined." << std::endl;
        throw ex;
      }
    }
  }
   
  if (os) {
    (*os) << "\tcritical equivalent strain   = " << epsC << std::endl;
    (*os) << "\tcritical equivalent stress   = " << sigC << std::endl;
    (*os) << "\tcritical energy density      = " << Ec << std::endl;
  }
}

// dissipated energy
double SmithFerranteDamagePotential::dissipatedEnergy(const MaterialProperties& material,
                                                      const ParameterSet& extPar,
                                                      const MatLibArray& intPar0,
                                                      MatLibArray& intPar1,
                                                      double dDot,double d,
                                                      double& Y,double& Yd,
                                                      double& K,double& Kd,double& Kdd,
                                                      bool first,bool second) {
  // quick return
  if (dDot <= 0.0e0) {
    if (first) {
      Y  = 0.0e0;
      Yd = 0.0e0;
    }
    if (second) {
      K = 0.0e0;
      Kd = 0.0e0;
      Kdd = 0.0e0;
    }
    return 0.0e0;
  }
  
  // get material parameters
  double Ec   = material.getDoubleProperty("CRITICAL_ENERGY_DENSITY");
  
  // intermediate quantities
  double coef = std::log(1./(1.-d));
  double Yc = 0.5*Ec*coef*coef;

  // compute dissipation pseudo-potential and derivative
  double phi = Yc*dDot;
  if (first) {
    Y = Yc;
    Yd = Ec*coef/(1.-d)*dDot;
  }
  if (second) {
    double val = Ec/(1.-d);
    K = 0.0e0;
    Kd = coef*val;
    Kdd = val*(1.+coef)*dDot/(1.-d);
  }
  
  return phi;
}

/*
 * Methods for class SmithFerranteElasticDamageBuilder.
 */

// the instance
SmithFerranteElasticDamageBuilder const* SmithFerranteElasticDamageBuilder::BUILDER
 = new SmithFerranteElasticDamageBuilder();

// constructor
SmithFerranteElasticDamageBuilder::SmithFerranteElasticDamageBuilder() {
  ModelDictionary::add("SMITH_FERRANTE_ELASTIC_DAMAGE",*this);
}

// build model
ConstitutiveModel* SmithFerranteElasticDamageBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new SmithFerranteElasticDamage3D();
      break;
    case 2:
      return new SmithFerranteElasticDamage2D();
      break;
    case 1:
      return new SmithFerranteElasticDamage1D();
      break;
    default:
      return 0;
      break;
  }
}

