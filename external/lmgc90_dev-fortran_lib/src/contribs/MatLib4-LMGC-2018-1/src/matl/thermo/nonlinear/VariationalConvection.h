/*
 *  $Id: VariationalConvection.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_THERMO_NONLINEAR_VARIATIONAL_CONVECTION_H
#define ZORGLIB_MATL_THERMO_NONLINEAR_VARIATIONAL_CONVECTION_H

// config
#include <matlib_macros.h>

// local
#include <matl/ConstitutiveModel.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif


/**
 * Base class for standard (non-linear) variational convection models.
 */
class VariationalConvection : virtual public StandardMaterial {

 public:

  // nested class
  class ConvectionPotential;
  
 protected:
  
  // associated convection potential
  ConvectionPotential *convection;
  
  // instance counter
  unsigned int *count;
  
  // empty constructor
  VariationalConvection(ConvectionPotential* = 0);

 public:

  // constructor
  VariationalConvection(ConvectionPotential&);
  
  // copy constructor
  VariationalConvection(const VariationalConvection&);
  
  // destructor
  virtual ~VariationalConvection();
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties&,const ParameterSet&);
  
  // how many external variables ?
  unsigned int nExtVar() const {return 2;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 2;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int) const;
  unsigned int indexExtVar(unsigned int) const;
  std::string labelExtVar(unsigned int) const;
  std::string labelExtForce(unsigned int) const;
  
  // how many internal variables ?
  unsigned int nIntVar() const {return 0;}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 0;}
  unsigned int getIntVar(const std::string&) const {return 0;}
  ConstitutiveModel::VariableType typeIntVar(unsigned int) const {
    return ConstitutiveModel::TYPE_NONE;
  }
  unsigned int indexIntVar(unsigned int) const {return 0;}
  std::string labelIntVar(unsigned int) const {return "";}
  
  // initialize the state of the material
  void initState(const MaterialProperties&,MaterialState&);
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties&,const ParameterSet&,
                              const MaterialState&,MaterialState&,double,
                              MatLibMatrix&,bool,bool) 
    throw (UpdateFailedException);
};


/**
 * Base class for (non-linear) convection potentials.
 */
class VariationalConvection::ConvectionPotential {
  
 protected:
  
  // default constructor
  ConvectionPotential() {}

 public:
  
  // destructor
  virtual ~ConvectionPotential() {}

  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // compute diffusion potential
  virtual double diffusionEnergy(const MaterialProperties&,const ParameterSet&,
                                 double,double,double&,double&,
                                 double&,double&,double&,bool,bool) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

