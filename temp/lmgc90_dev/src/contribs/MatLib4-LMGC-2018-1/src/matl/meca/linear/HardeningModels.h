/*
 *  $Id: HardeningModels.h 147 2014-08-01 14:46:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_HARDENING_MODELS_H
#define ZORGLIB_MATL_MECA_LINEAR_HARDENING_MODELS_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/linear/ViscoPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for linear isotropic hardening.
 */
class LinearIsotropicHardeningModel : virtual public IsotropicHardeningModel {
  
 public:
  
  // constructor
  LinearIsotropicHardeningModel() {}
  
  // copy constructor
  LinearIsotropicHardeningModel(const LinearIsotropicHardeningModel&) {}

  // destructor
  virtual ~LinearIsotropicHardeningModel() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of internal parameters
  unsigned int nIntPar() const {return 0;}
  
  // plastically stored energy
  double storedEnergy(const MaterialProperties&,const ParameterSet&,
                      const MatLibArray&,MatLibArray&,
                      double,double,double,double&,double&,bool,bool);
  
  // yield stress
  double yieldStress(const MaterialProperties&,const ParameterSet&,
                     const MatLibArray&,MatLibArray&,
                     double,double&,bool);
};


/**
 * Class for nonlinear isotropic hardening.
 * Mix of Swift and Voce hardening laws.
 */
class NonLinearIsotropicHardeningModel : virtual public IsotropicHardeningModel {
  
 public:
  
  // constructor
  NonLinearIsotropicHardeningModel() {}
  
  // copy constructor
  NonLinearIsotropicHardeningModel(const NonLinearIsotropicHardeningModel&) {}
  
  // destructor
  virtual ~NonLinearIsotropicHardeningModel() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of internal parameters
  unsigned int nIntPar() const {return 0;}
  
  // plastically stored energy
  double storedEnergy(const MaterialProperties&,const ParameterSet&,
                      const MatLibArray&,MatLibArray&,
                      double,double,double,double&,double&,bool,bool);
  
  // yield stress
  double yieldStress(const MaterialProperties&,const ParameterSet&,
                     const MatLibArray&,MatLibArray&,
                     double,double&,bool);
};


/**
 * Class for Ludwik isotropic hardening.
 */
class LudwikIsotropicHardeningModel : virtual public IsotropicHardeningModel {
  
 public:
  
  // constructor
  LudwikIsotropicHardeningModel() {}
  
  // copy constructor
  LudwikIsotropicHardeningModel(const LudwikIsotropicHardeningModel&) {}
  
  // destructor
  virtual ~LudwikIsotropicHardeningModel() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of internal parameters
  unsigned int nIntPar() const {return 0;}
  
  // plastically stored energy
  double storedEnergy(const MaterialProperties&,const ParameterSet&,
                      const MatLibArray&,MatLibArray&,
                      double,double,double,double&,double&,bool,bool);
  
  // yield stress
  double yieldStress(const MaterialProperties&,const ParameterSet&,
                     const MatLibArray&,MatLibArray&,
                     double,double&,bool);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
