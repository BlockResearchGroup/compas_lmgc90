/*
 *  $Id: StdConvectionPotential.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_THERMO_NONLINEAR_STD_CONVECTION_POTENTIAL_H
#define ZORGLIB_MATL_THERMO_NONLINEAR_STD_CONVECTION_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <matl/ModelDictionary.h>
#include <matl/thermo/nonlinear/VariationalConvection.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for standard variational (non-linear) convection potential.
 */
class StdConvectionPotential 
: virtual public VariationalConvection::ConvectionPotential {

 public:
  
  // default constructor
  StdConvectionPotential() {}
  
  // copy constructor
  StdConvectionPotential(const StdConvectionPotential&) {}

  // destructor
  virtual ~StdConvectionPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // compute diffusion energy
  double diffusionEnergy(const MaterialProperties&,const ParameterSet&,
                         double,double,double&,double&,
                         double&,double&,double&,bool,bool);
};


/**
 * Implementation of the model.
 */
class StdVariationalConvection : public VariationalConvection {
  
 public:
  
  // constructor
  StdVariationalConvection()
  : VariationalConvection(new StdConvectionPotential()) {}
  
  // copy constructor
  StdVariationalConvection(const StdVariationalConvection& src) 
  : VariationalConvection(src) {}
  
  // destructor
  virtual ~StdVariationalConvection() {}
};

/**
 * The associated model builder
 */
class StdVariationalConvectionBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  StdVariationalConvectionBuilder();
  
  // the instance
  static StdVariationalConvectionBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~StdVariationalConvectionBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
