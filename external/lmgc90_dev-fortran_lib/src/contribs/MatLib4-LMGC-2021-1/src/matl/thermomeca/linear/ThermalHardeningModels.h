/*
 *  $Id: ThermalHardeningModels.h 142 2014-02-07 12:51:54Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_HARDENING_MODELS_H
#define ZORGLIB_MATL_MECA_THERMO_HARDENING_MODELS_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/linear/HardeningModels.h>
#include <matl/thermomeca/linear/ThermoViscoPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for linear isotropic hardening with temperature dependence.
 */
class ThermalLinearIsotropicHardeningModel 
: virtual public ThermalIsotropicHardeningModel, 
  virtual public LinearIsotropicHardeningModel {
  
 public:
  
  // constructor
  ThermalLinearIsotropicHardeningModel() {}
  
  // copy constructor
  ThermalLinearIsotropicHardeningModel(const ThermalLinearIsotropicHardeningModel&) {}

  // destructor
  virtual ~ThermalLinearIsotropicHardeningModel() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
    
  // update properties in function of external parameters
  void updateProperties(MaterialProperties&,const ParameterSet&);
    
  // plastically stored energy
  double storedThMEnergy(const MaterialProperties&,const ParameterSet&,
                         const MatLibArray&,MatLibArray&,double,double,
                         double,double,double&,double&,
                         double&,double&,double&,bool,bool) ;
  
  // yield stress
  double yieldThMStress(const MaterialProperties&,const ParameterSet&,
                        const MatLibArray&,MatLibArray&,double,double,
                        double&,double&,bool);
};


/**
 * Class for nonlinear isotropic hardening with temperature dependence.
 */
class ThermalNonLinearIsotropicHardeningModel 
: virtual public ThermalIsotropicHardeningModel,
  virtual public NonLinearIsotropicHardeningModel {
  
 public:
  
  // constructor
  ThermalNonLinearIsotropicHardeningModel() {}
  
  // copy constructor
  ThermalNonLinearIsotropicHardeningModel(const ThermalNonLinearIsotropicHardeningModel&) {}
  
  // destructor
  virtual ~ThermalNonLinearIsotropicHardeningModel() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties&,const ParameterSet&);
  
  // plastically stored energy
  double storedThMEnergy(const MaterialProperties&,const ParameterSet&,
                         const MatLibArray&,MatLibArray&,double,double,
                         double,double,double&,double&,
                         double&,double&,double&,bool,bool) ;
  
  // yield stress
  double yieldThMStress(const MaterialProperties&,const ParameterSet&,
                        const MatLibArray&,MatLibArray&,double,double,
                        double&,double&,bool);
};


#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
