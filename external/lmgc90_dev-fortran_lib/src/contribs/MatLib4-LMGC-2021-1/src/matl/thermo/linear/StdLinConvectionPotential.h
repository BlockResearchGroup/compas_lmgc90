/*
 *  $Id: StdLinConvectionPotential.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_THERMO_LINEAR_STD_CONVECTION_POTENTIAL_H
#define ZORGLIB_MATL_THERMO_LINEAR_STD_CONVECTION_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <matl/ModelDictionary.h>
#include <matl/thermo/linear/LinVariationalConvection.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for standard (linearized) convection potential.
 */
class StdLinConvectionPotential 
: virtual public LinVariationalConvection::ConvectionPotential {

 public:
  
  // default constructor
  StdLinConvectionPotential() {}
  
  // copy constructor
  StdLinConvectionPotential(const StdLinConvectionPotential&) {}

  // destructor
  virtual ~StdLinConvectionPotential() {}
  
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
class StdLinVariationalConvection : public LinVariationalConvection {
  
 public:
  
  // constructor
  StdLinVariationalConvection()
  : LinVariationalConvection(new StdLinConvectionPotential()) {}
  
  // copy constructor
  StdLinVariationalConvection(const StdLinVariationalConvection& src) 
  : LinVariationalConvection(src) {}
  
  // destructor
  virtual ~StdLinVariationalConvection() {}
};

/**
 * The associated model builder
 */
class StdLinVariationalConvectionBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  StdLinVariationalConvectionBuilder();
  
  // the instance
  static StdLinVariationalConvectionBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~StdLinVariationalConvectionBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
