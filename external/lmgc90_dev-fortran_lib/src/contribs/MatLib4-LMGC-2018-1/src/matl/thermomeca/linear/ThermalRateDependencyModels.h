/*
 *  $Id: ThermalRateDependencyModels.h 153 2014-10-03 09:12:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_RATE_DEPENDENCY_MODELS_H
#define ZORGLIB_MATL_MECA_THERMO_RATE_DEPENDENCY_MODELS_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/linear/RateDependencyModels.h>
#include <matl/thermomeca/linear/ThermoViscoPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Scalar power-law rate-dependency model with temperature dependence.
 */
class ThermalPowerLawRateDependencyModel 
: virtual public ThermalScalarRateDependencyModel,
  virtual public PowerLawRateDependencyModel {
  
 public:
  
  // constructor
  ThermalPowerLawRateDependencyModel() {}
  
  // copy constructor
  ThermalPowerLawRateDependencyModel(const ThermalPowerLawRateDependencyModel&) {}
  
  // destructor
  virtual ~ThermalPowerLawRateDependencyModel() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
    
  // update properties in function of external parameters
  void updateProperties(MaterialProperties&,const ParameterSet&);
    
  // dissipated energy
  double dissipatedThMEnergy(const MaterialProperties&,const ParameterSet&,
                             const MatLibArray&,MatLibArray&,
                             double,double,double,double&,double&,double&,
                             double&,double&,double&,
                             double&,double&,double&,bool,bool);
};


/**
 * Scalar asinh (thermal activation) rate-dependency model with temperature dependence.
 */
class ThermalASinhRateDependencyModel 
: virtual public ThermalScalarRateDependencyModel,
  virtual public ASinhRateDependencyModel {
  
 public:
  
  // constructor
  ThermalASinhRateDependencyModel() {}
  
  // copy constructor
  ThermalASinhRateDependencyModel(const ThermalASinhRateDependencyModel&) {}
  
  // destructor
  virtual ~ThermalASinhRateDependencyModel() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // dissipated energy
  double dissipatedThMEnergy(const MaterialProperties&,const ParameterSet&,
                             const MatLibArray&,MatLibArray&,
                             double,double,double,double&,double&,double&,
                             double&,double&,double&,
                             double&,double&,double&,bool,bool);
};


/**
 * Scalar Norton-Hoff rate-dependency model with temperature dependence (thermal activation).
 */
class ThermalNortonHoffRateDependencyModel
: virtual public ThermalScalarRateDependencyModel,
  virtual public NortonHoffRateDependencyModel {
  
 public:
  
  // constructor
  ThermalNortonHoffRateDependencyModel() {}
  
  // copy constructor
  ThermalNortonHoffRateDependencyModel(const ThermalNortonHoffRateDependencyModel&) {}
  
  // destructor
  virtual ~ThermalNortonHoffRateDependencyModel() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // dissipated energy
  double dissipatedThMEnergy(const MaterialProperties&,const ParameterSet&,
                             const MatLibArray&,MatLibArray&,
                             double,double,double,double&,double&,double&,
                             double&,double&,double&,
                             double&,double&,double&,bool,bool);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
