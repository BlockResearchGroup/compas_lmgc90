/*
 *  $Id: LinearConvection.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_THERMO_LINEAR_CLASSICAL_CONVECTION_H
#define ZORGLIB_MATL_THERMO_LINEAR_CLASSICAL_CONVECTION_H

// config
#include <matlib_macros.h>

// local
#include <matl/ConstitutiveModel.h>
#include <matl/ModelDictionary.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif


/**
 * Base class for classical linear convection models.
 */
class LinearConvection : virtual public StandardMaterial {

 public:
  
  // constructor
  LinearConvection() {};
  
  // copy constructor
  LinearConvection(const LinearConvection&) {}
  
  // destructor
  virtual ~LinearConvection() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // how many external variables ?
  unsigned int nExtVar() const {return 1;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 1;}
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
  
  // check if the material behaviour is linear ?
  bool isLinear() const {return true;}
  
  // initialize the state of the material
  void initState(const MaterialProperties&,MaterialState&);
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties&,const ParameterSet&,
                              const MaterialState&,MaterialState&,double,
                              MatLibMatrix&,bool,bool) 
    throw (UpdateFailedException);
};


/**
 * The associated model builder
 */
class LinearConvectionBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  LinearConvectionBuilder();
  
  // the instance
  static LinearConvectionBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~LinearConvectionBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
