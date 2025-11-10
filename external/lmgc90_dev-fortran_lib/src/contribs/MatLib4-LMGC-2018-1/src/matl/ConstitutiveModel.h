/*
 *  $Id: ConstitutiveModel.h 191 2015-12-02 12:44:07Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_CONSTITUTIVE_MODEL_H
#define ZORGLIB_MATL_CONSTITUTIVE_MODEL_H

// config
#include <matlib_macros.h>

// std C++ library
#include <iostream>
#include <string>
// local
#ifndef WITH_MATLIB_H
#include <data/Exceptions.h>
#include <data/FileException.h>
#include <data/Rotation.h>
#include <data/ShortSqrMatrix.h>
#include <matl/MaterialProperties.h>
#include <matl/MaterialState.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/*
 * Define MatLib types (for interface).
 */
typedef ShortSqrMatrix MatLibMatrix;


/**
 * Exception thrown when a property has been given an invalid value.
 */
class InvalidPropertyException : public ZException {
  
 public:
  
  // default constructor
  InvalidPropertyException(const std::string& msg = "invalid property")
  : ZException(msg) {}
  
  // copy constructor
  InvalidPropertyException(const InvalidPropertyException& src)
  : ZException(src) {}
};


/**
 * Exception thrown when the constitutive update fails.
 */
class UpdateFailedException : public ZException {
  
 public:
  
  // default constructor
  UpdateFailedException(const std::string& msg = "update failed")
  : ZException(msg) {}
  
  // copy constructor
  UpdateFailedException(const UpdateFailedException& src)
  : ZException(src) {}
};


/**
 * Define new type ParameterSet
 */
typedef StringMap<double>::Type ParameterSet;


/**
 * Virtual base class for constitutive models.
 */
class ConstitutiveModel {

 public:
  
  // define variable types handled by const. models
  enum VariableType {
    TYPE_NONE,
    TYPE_SCALAR,
    TYPE_VECTOR,
    TYPE_SYM_TENSOR,
    TYPE_TENSOR,
    TYPE_STD_SYM_TENSOR,
    TYPE_STD_TENSOR
  };

 protected:
  
  // constructor
  ConstitutiveModel() {}

 public:
  
  // virtual destructor
  virtual ~ConstitutiveModel() {}

  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream*) 
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  void checkProperties(MaterialProperties&,const char* = 0) 
    throw (FileException, InvalidPropertyException, NoSuchPropertyException);
  
  // apply rotation to material properties
  virtual void rotateProperties(MaterialProperties&,const Rotation&) {}
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // how many external variables ?
  virtual unsigned int nExtVar() const = 0;
  
  // self-documenting utilities
  virtual unsigned int nExtVarBundled() const = 0;
  virtual VariableType typeExtVar(unsigned int) const = 0;
  virtual unsigned int indexExtVar(unsigned int) const = 0;
  virtual std::string labelExtVar(unsigned int) const = 0;
  virtual std::string labelExtForce(unsigned int) const = 0;
  
  // how many internal variables ?
  virtual unsigned int nIntVar() const = 0;
  
  // self-documenting utilities
  virtual unsigned int nIntVarBundled() const = 0;
  virtual unsigned int getIntVar(const std::string&) const = 0;
  virtual VariableType typeIntVar(unsigned int) const = 0;
  virtual unsigned int indexIntVar(unsigned int) const = 0;
  virtual std::string labelIntVar(unsigned int) const = 0;
  
  // utility function
  static unsigned int dimension(VariableType,unsigned int);

  // check if the material behaviour is linear ?
  virtual bool isLinear() const {return false;}
  
  // check if the material is "standard" ?
  virtual bool isStandard() const {return false;}
  
  // initialize the state of the material
  virtual void initState(const MaterialProperties&,MaterialState& state) {
    state.grad.resize(nExtVar());
    state.flux.resize(nExtVar());
    state.internal.resize(nIntVar());
  }
  
  // update the state of the material (with the ability to compute tangents)
  virtual void updateState(const MaterialProperties&,const ParameterSet&,
                           const MaterialState&,MaterialState&,double,
                           MatLibMatrix&,bool) 
    throw (UpdateFailedException) = 0;
  
  // compute material tangents (without updating)
  virtual void computeTangent(const MaterialProperties& mater,const ParameterSet& extPar,
                              const MaterialState& state0,const MaterialState& state1,
                              double dTime,MatLibMatrix& tgt) {
    computeNumericalTangent(mater,extPar,state0,state1,dTime,tgt);
  }
  
  // compute material tangents by numerical perturbation
  void computeNumericalTangent(const MaterialProperties&,const ParameterSet&,
                               const MaterialState&,const MaterialState&,double,
                               MatLibMatrix&);
};


/**
 * Additional interface for standard materials.
 */
class StandardMaterial : virtual public ConstitutiveModel {
  
 public:
  
  // destructor
  virtual ~StandardMaterial() {}
  
  // check if the material is "standard" ?
  bool isStandard() const {return true;}
  
  // compute the incremental potential
  virtual double incrementalPotential(const MaterialProperties&,
                                      const ParameterSet&,
                                      const MaterialState&,MaterialState&,
                                      double,MatLibMatrix&,bool,bool) 
    throw (UpdateFailedException) = 0;
  
  // update the state of the material
  void updateState(const MaterialProperties& mater,const ParameterSet& extPar,
                   const MaterialState& state0,MaterialState& state1,
                   double dTime,MatLibMatrix& tgt,bool compTgt) 
   throw (UpdateFailedException) {
    incrementalPotential(mater,extPar,state0,state1,dTime,tgt,true,compTgt);
  }
  
  // compute material tangents (without updating)
  void computeTangent(const MaterialProperties& mater,const ParameterSet& extPar,
                      const MaterialState& state0,const MaterialState& state1,
                      double dTime,MatLibMatrix& tgt) {
    incrementalPotential(mater,extPar,state0,const_cast<MaterialState&>(state1),
                         dTime,tgt,false,true);
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
