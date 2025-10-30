/*
 *  $Id: DilatantViscoPlasticity.h 233 2017-03-30 20:12:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_DILATANT_VISCO_PLASTICITY_H
#define ZORGLIB_MATL_MECA_LINEAR_DILATANT_VISCO_PLASTICITY_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/linear/ViscoPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for describing a dilatant visco-plasticity model.
 * Also works for Tresca.
 */
class DilatantViscoPlasticity {
  
 public:
  
  // flag indicating if the model needs initialization
  bool initialize;
  
  // flag indicating if the model needs finalization (update internal parameters)
  bool finalize;
  
  // destructor
  virtual ~DilatantViscoPlasticity() {}
  
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
                                    double,double,double&,double&,double&,
                                    double&,double&,double,bool,bool) = 0;

  // compute steepest gradient direction (at origin)
  virtual double steepestGradient(const MaterialProperties&,const ParameterSet&,
                                  const MatLibArray&,MatLibArray&,double,double,
                                  double,double,double,double,double&,double&,double) = 0;
};


/**
 * Visco-plasticity model with elliptic yield function (with isotropic hardening).
 */
class EllipticViscoPlasticity : public DilatantViscoPlasticity {
  
 protected:
  
  // plastic hardening part (stored and dissipated)
  IsotropicHardeningModel *hardening;
  
  // viscous dissipation part
  ScalarRateDependencyModel *viscous;
  
  // instance counter
  unsigned int *count;
  
 public:

  // constructor
  EllipticViscoPlasticity(IsotropicHardeningModel*,
                          ScalarRateDependencyModel*);
  
  // copy constructor
  EllipticViscoPlasticity(const EllipticViscoPlasticity&);
  
  // destructor
  virtual ~EllipticViscoPlasticity();
  
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
                            double,double,double&,double&,
                            double&,double&,double&,double,bool,bool);
  
  // compute steepest gradient direction (at origin)
  double steepestGradient(const MaterialProperties&,const ParameterSet&,
                          const MatLibArray&,MatLibArray&,double,double,
                          double,double,double,double,double&,double&,double);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
