/*
 *  $Id: ThermoViscoPlasticitySimple.h 142 2014-02-07 12:51:54Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_VISCO_PLASTICITY_SIMPLE_H
#define ZORGLIB_MATL_MECA_THERMO_VISCO_PLASTICITY_SIMPLE_H

// config
#include <matlib_macros.h>

// local
#include <matl/ConstitutiveModel.h>
#include <matl/meca/linear/ViscoPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for describing a J2 thermo-visco-plasticity model with isotropic hardening.
 * Also works for Tresca.
 */
class ThermoViscoPlasticitySimple {
  
 public:
  
  // flag indicating if the model needs initialization
  bool initialize;
  
  // flag indicating if the model needs finalization (update internal parameters)
  bool finalize;

  // destructor
  virtual ~ThermoViscoPlasticitySimple() {}
  
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
                                    double,double,double,double,double&,double&,
                                    double&,double&,double&,double&,double,bool,bool) = 0;
};


/**
 * Isotropic hardening model with temperature dependence.
 */
class ThermalIsotropicHardeningModel : virtual public IsotropicHardeningModel {

 protected:

  // constructor
  ThermalIsotropicHardeningModel() {}

 public:

  // destructor
  virtual ~ThermalIsotropicHardeningModel() {}

  // plastically stored energy
  virtual double storedThMEnergy(const MaterialProperties&,const ParameterSet&,
                                 const MatLibArray&,MatLibArray&,double,double,
                                 double,double,double&,double&,
                                 double&,double&,double&,bool,bool) = 0;
  
  // yield stress
  virtual double yieldThMStress(const MaterialProperties&,const ParameterSet&,
                                const MatLibArray&,MatLibArray&,double,double,
                                double&,double&,bool) = 0;
};


/**
 * Scalar rate-dependency model with temperature dependence.
 */
class ThermalScalarRateDependencyModel : virtual public ScalarRateDependencyModel {

 protected:

  // constructor
  ThermalScalarRateDependencyModel() {}
  
 public:
  
  // destructor
  virtual ~ThermalScalarRateDependencyModel() {}
  
  // dissipated energy
  virtual double dissipatedThMEnergy(const MaterialProperties&,const ParameterSet&,
                                     const MatLibArray&,MatLibArray&,
                                     double,double,double,double&,double&,double&,
                                     double&,double&,double&,
                                     double&,double&,double&,bool,bool) = 0;
};


/**
 * Standard thermo-visco-plasticity model (with isotropic hardening).
 */
class StdThermoViscoPlasticitySimple : public ThermoViscoPlasticitySimple {
  
 protected:
  
  // plastic hardening part (stored and dissipated)
  ThermalIsotropicHardeningModel *hardening;
  
  // viscous dissipation part
  ThermalScalarRateDependencyModel *viscous;
  
  // instance counter
  unsigned int *count;
  
 public:

  // constructor
  StdThermoViscoPlasticitySimple(ThermalIsotropicHardeningModel*,
                                 ThermalScalarRateDependencyModel*);
  
  // copy constructor
  StdThermoViscoPlasticitySimple(const StdThermoViscoPlasticitySimple&);
  
  // destructor
  virtual ~StdThermoViscoPlasticitySimple();
  
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
                            double,double,double,double,double&,double&,
                            double&,double&,double&,double&,double,bool,bool);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
