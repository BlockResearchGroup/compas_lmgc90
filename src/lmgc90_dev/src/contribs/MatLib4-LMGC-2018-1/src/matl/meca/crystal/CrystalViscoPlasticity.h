/*
 *  $Id: CrystalViscoPlasticity.h 202 2016-03-31 11:51:40Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_CRYSTAL_VISCO_PLASTICITY_H
#define ZORGLIB_MATL_MECA_CRYSTAL_VISCO_PLASTICITY_H

// config
#include <matlib_macros.h>

// local
#include <matl/ConstitutiveModel.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Generic class for describing a crystal visco-plasticity model.
 */
class CrystalViscoPlasticity {
  
 public:
  
  // flag indicating if the model needs initialization
  bool initialize;
  
  // flag indicating if the model needs finalization (update internal parameters)
  bool finalize;
  
  // destructor
  virtual ~CrystalViscoPlasticity() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // number of additional internal parameters (should be at least 1 for the stored plastic energy)
  virtual unsigned int nIntPar() const = 0;
  
  // compute plastic potential and derivatives
  virtual double irreversibleEnergy(const MaterialProperties&,const ParameterSet&,
                                    const MatLibArray&,MatLibArray&,
                                    const MatLibArray&,const MatLibArray&,
                                    MatLibArray&,MatLibMatrix&,double,bool,bool) = 0;
};


/**
 * Crystal plasticity hardening model.
 */
class CrystalHardeningModel {
  
 public:
  
  // flag indicating if the model needs initialization
  bool initialize;
  
  // flag indicating if the model needs finalization (update internal parameters)
  bool finalize;

  // destructor
  virtual ~CrystalHardeningModel() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // number of internal parameters
  virtual unsigned int nIntPar() const = 0;
  
  // plastically stored energy
  virtual double storedEnergy(const MaterialProperties&,const ParameterSet&,
                              const MatLibArray&,MatLibArray&,double,
                              const MatLibArray&,const MatLibArray&,
                              MatLibArray&,MatLibMatrix&,bool,bool) = 0;
  
  // yield stress
  virtual void yieldStress(const MaterialProperties&,const ParameterSet&,
                           const MatLibArray&,MatLibArray&,
                           const MatLibArray&,const MatLibArray&,
                           const MatLibArray&,MatLibArray&,MatLibMatrix&,
                           std::vector<MatLibMatrix>&,bool,bool) = 0;
};


/**
 * Crystal plasticity rate-dependency model.
 */
class CrystalRateDependencyModel {
  
 public:
  
  // flag indicating if the model needs initialization
  bool initialize;
  
  // flag indicating if the model needs finalization (update internal parameters)
  bool finalize;
  
  // destructor
  virtual ~CrystalRateDependencyModel() {}
  
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
                                  const MatLibArray&,const MatLibArray&,
                                  MatLibArray&,MatLibArray&,MatLibMatrix&,
                                  MatLibMatrix&,MatLibMatrix&,bool,bool) = 0;
};


/**
 * Standard class for describing a crystal visco-plasticity model.
 */
class StdCrystalViscoPlasticity : virtual public CrystalViscoPlasticity {

 protected:
  
  // rate-independent hardening part
  CrystalHardeningModel *hardening;
  
  // rate-dependent dissipation
  CrystalRateDependencyModel *viscous;
  
  // instance counter
  unsigned int *count;
  
 public:

  // constructor
  StdCrystalViscoPlasticity(CrystalHardeningModel*,
                            CrystalRateDependencyModel*);
  
  // copy constructor
  StdCrystalViscoPlasticity(const StdCrystalViscoPlasticity&);
  
  // destructor
  virtual ~StdCrystalViscoPlasticity();
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties&,const ParameterSet&);
  
  // number of additional internal parameters
  unsigned int nIntPar() const;
  
  // compute plastic potential and derivatives
  double irreversibleEnergy(const MaterialProperties&,const ParameterSet&,
                            const MatLibArray&,MatLibArray&,
                            const MatLibArray&,const MatLibArray&,
                            MatLibArray&,MatLibMatrix&,double,bool,bool);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
