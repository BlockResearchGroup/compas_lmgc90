/*
 *  $Id: RateDependencyModels.h 147 2014-08-01 14:46:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_RATE_DEPENDENCY_MODELS_H
#define ZORGLIB_MATL_MECA_LINEAR_RATE_DEPENDENCY_MODELS_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/linear/ViscoPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Scalar power-law rate-dependency model.
 */
class PowerLawRateDependencyModel : virtual public ScalarRateDependencyModel {
  
 public:
  
  // constructor
  PowerLawRateDependencyModel() {}
  
  // copy constructor
  PowerLawRateDependencyModel(const PowerLawRateDependencyModel&) {}
  
  // destructor
  virtual ~PowerLawRateDependencyModel() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of internal parameters
  unsigned int nIntPar() const {return 0;}
  
  // dissipated energy
  double dissipatedEnergy(const MaterialProperties&,const ParameterSet&,
                          const MatLibArray&,MatLibArray&,
                          double,double,double&,double&,double&,double&,
                          double&,bool,bool);
};


/**
 * Scalar asinh (thermal activation) rate-dependency model.
 */
class ASinhRateDependencyModel : virtual public ScalarRateDependencyModel {
  
 public:
  
  // constructor
  ASinhRateDependencyModel() {}
  
  // copy constructor
  ASinhRateDependencyModel(const ASinhRateDependencyModel&) {}
  
  // destructor
  virtual ~ASinhRateDependencyModel() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of internal parameters
  unsigned int nIntPar() const {return 0;}
  
  // dissipated energy
  double dissipatedEnergy(const MaterialProperties&,const ParameterSet&,
                          const MatLibArray&,MatLibArray&,
                          double,double,double&,double&,double&,double&,
                          double&,bool,bool);
};


/**
 * Norton-Hoff rate-dependency model.
 */
class NortonHoffRateDependencyModel : virtual public ScalarRateDependencyModel {
  
public:
  
  // constructor
  NortonHoffRateDependencyModel() {}
  
  // copy constructor
  NortonHoffRateDependencyModel(const NortonHoffRateDependencyModel&) {}
  
  // destructor
  virtual ~NortonHoffRateDependencyModel() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of internal parameters
  unsigned int nIntPar() const {return 0;}
  
  // dissipated energy
  double dissipatedEnergy(const MaterialProperties&,const ParameterSet&,
                          const MatLibArray&,MatLibArray&,
                          double,double,double&,double&,double&,double&,
                          double&,bool,bool);
};


/**
 * Johnson-Cook rate-dependency model.
 */
class JohnsonCookRateDependencyModel : virtual public ScalarRateDependencyModel {
  
 public:
  
  // constructor
  JohnsonCookRateDependencyModel() {}
  
  // copy constructor
  JohnsonCookRateDependencyModel(const JohnsonCookRateDependencyModel&) {}
  
  // destructor
  virtual ~JohnsonCookRateDependencyModel() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of internal parameters
  unsigned int nIntPar() const {return 0;}
  
  // dissipated energy
  double dissipatedEnergy(const MaterialProperties&,const ParameterSet&,
                          const MatLibArray&,MatLibArray&,
                          double,double,double&,double&,double&,double&,
                          double&,bool,bool);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
