/*
 *  $Id: StdThMConvectionPotential.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_COUPLED_STD_THERMO_MECHANICAL_CONVECTION_POTENTIAL_H
#define ZORGLIB_MATL_COUPLED_STD_THERMO_MECHANICAL_CONVECTION_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <matl/ModelDictionary.h>
#include <matl/thermomeca/coupled/CoupledThMConvection.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for standard variational (non-linear) thermomechanical convection potential.
 */
class StdThMConvectionPotential 
: virtual public CoupledThMConvection::ConvectionPotential {
  
 public:
  
  // default constructor
  StdThMConvectionPotential() {}
  
  // copy constructor
  StdThMConvectionPotential(const StdThMConvectionPotential&) {}
  
  // destructor
  virtual ~StdThMConvectionPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // compute diffusion energy
  double diffusionEnergy(const MaterialProperties&,const ParameterSet&,
                         double,double,double,double&,double&,double&,
                         double&,double&,double&,double&,double&,double&,
                         bool,bool);
};


/**
 * Implementation of the model.
 */
class StdThMConvection : public CoupledThMConvection {
  
 public:
  
  // constructor
  StdThMConvection()
  : CoupledThMConvection(new StdThMConvectionPotential()) {}
  
  // copy constructor
  StdThMConvection(const StdThMConvection& src) 
  : CoupledThMConvection(src) {}
  
  // destructor
  virtual ~StdThMConvection() {}
};

/**
 * The associated model builder
 */
class StdThMConvectionBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  StdThMConvectionBuilder();
  
  // the instance
  static StdThMConvectionBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~StdThMConvectionBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
