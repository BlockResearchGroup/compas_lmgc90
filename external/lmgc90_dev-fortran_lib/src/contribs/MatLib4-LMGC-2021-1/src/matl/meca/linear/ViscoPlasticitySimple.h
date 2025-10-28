/*
 *  $Id: ViscoPlasticitySimple.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_VISCO_PLASTICITY_SIMPLE_H
#define ZORGLIB_MATL_MECA_LINEAR_VISCO_PLASTICITY_SIMPLE_H

// config
#include <matlib_macros.h>

// local
#include <matl/ConstitutiveModel.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for describing a J2 visco-plasticity model with isotropic hardening.
 * Also works for Tresca.
 */
class ViscoPlasticitySimple {
  
 public:
  
  // flag indicating if the model needs initialization
  bool initialize;
  
  // flag indicating if the model needs finalization (update internal parameters)
  bool finalize;
  
  // destructor
  virtual ~ViscoPlasticitySimple() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // number of internal parameters (should be at least 1 for the stored plastic energy)
  virtual unsigned int nIntPar() const = 0;
  
  // compute irreversible energy and derivatives
  virtual double irreversibleEnergy(const MaterialProperties&,const ParameterSet&,
                                    const MatLibArray&,MatLibArray&,double,double,
                                    double&,double&,double,bool,bool) = 0;
};


/**
 * Isotropic hardening model.
 */
class IsotropicHardeningModel {
  
 public:
  
  // flag indicating if the model needs initialization
  bool initialize;
  
  // flag indicating if the model needs finalization (update internal parameters)
  bool finalize;
  
  // destructor
  virtual ~IsotropicHardeningModel() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // number of internal parameters
  virtual unsigned int nIntPar() const = 0;

  // plastically stored energy
  virtual double storedEnergy(const MaterialProperties&,const ParameterSet&,
                              const MatLibArray&,MatLibArray&,double,double,
                              double,double&,double&,bool,bool) = 0;
  
  // yield stress
  virtual double yieldStress(const MaterialProperties&,const ParameterSet&,
                             const MatLibArray&,MatLibArray&,double,
                             double&,bool) = 0;
};


/**
 * Scalar rate-dependency model.
 */
class ScalarRateDependencyModel {
  
 public:
  
  // flag indicating if the model needs initialization
  bool initialize;
  
  // flag indicating if the model needs finalization (update internal parameters)
  bool finalize;
  
  // destructor
  virtual ~ScalarRateDependencyModel() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // number of internal parameters
  virtual unsigned int nIntPar() const = 0;
  
  // dissipated energy
  virtual double dissipatedEnergy(const MaterialProperties&,const ParameterSet&,
                                  const MatLibArray&,MatLibArray&,
                                  double,double,double&,double&,double&,double&,
                                  double&,bool,bool) = 0;
};


/**
 * Standard visco-plasticity model (with isotropic hardening).
 */
class StdViscoPlasticitySimple : public ViscoPlasticitySimple {
  
 protected:
  
  // plastic hardening part (stored and dissipated)
  IsotropicHardeningModel *hardening;
  
  // viscous dissipation part
  ScalarRateDependencyModel *viscous;
  
  // instance counter
  unsigned int *count;
  
 public:

  // constructor
  StdViscoPlasticitySimple(IsotropicHardeningModel*,
                           ScalarRateDependencyModel*);
  
  // copy constructor
  StdViscoPlasticitySimple(const StdViscoPlasticitySimple&);
  
  // destructor
  virtual ~StdViscoPlasticitySimple();
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties&,const ParameterSet&);
  
  // number of internal parameters
  unsigned int nIntPar() const;
  
  // compute irreversible energy and derivatives
  double irreversibleEnergy(const MaterialProperties&,const ParameterSet&,
                            const MatLibArray&,MatLibArray&,double,double,
                            double&,double&,double,bool,bool);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
