/*
 *  $Id: LinearConduction.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_THERMO_LINEAR_CLASSICAL_CONDUCTION_H
#define ZORGLIB_MATL_THERMO_LINEAR_CLASSICAL_CONDUCTION_H

// config
#include <matlib_macros.h>

// local
#include <matl/ConstitutiveModel.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif


/**
 * Base class for classical linear conduction models.
 */
template <class ALG>
class LinearConduction : virtual public StandardMaterial {

 public:
  
  // define new types
  typedef typename ALG::SymTensor SYM_TENSOR;
  typedef typename ALG::Vector    VECTOR;

  // nested classes
  class ConductionPotential;
  
 protected:
  
  // associated conduction potential
  ConductionPotential *conduction;
  
  // instance counter
  unsigned int *count;
  
  // empty constructor
  LinearConduction(ConductionPotential* k = 0) {
    count = new unsigned int(1);
    conduction = k;
  };

 public:

  // constructor
  LinearConduction(ConductionPotential& k) {
    count = new unsigned int(1);
    conduction = &k;
  }
  
  // copy constructor
  LinearConduction(const LinearConduction& src) {
    count = src.count;
    (*count)++;
    conduction = src.conduction;
  }
  
  // destructor
  virtual ~LinearConduction() {
    if (--(*count) > 0) return;
    delete count;
    if (conduction) delete conduction;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nLinear classical conduction:" << std::endl;
     
    // heat capacity
    try {
      double Cv = material.getDoubleProperty("VOLUMIC_HEAT_CAPACITY");
      if (os) (*os) << "\n\tvolumic capacity = " << Cv << std::endl;
    }
    catch (NoSuchPropertyException) {
      try{
        double C = material.getDoubleProperty("SPECIFIC_HEAT_CAPACITY");
        if (os) (*os) << "\n\tspecific capacity = " << C << std::endl;
        double rho = material.getDoubleProperty("MASS_DENSITY");
        double Cv = rho*C;
        material.setProperty("VOLUMIC_HEAT_CAPACITY",Cv);
        if (os) (*os) << "\tvolumic capacity  = " << Cv << std::endl;
      }
      catch (NoSuchPropertyException) {
        if (os) (*os) << "\n\tvolumic capacity is not defined" << std::endl;
      }
    }
     
    // conduction part
    conduction->checkProperties(material,os);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    conduction->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    conduction->updateProperties(mater,extPar);
  }
  
  // how many external variables ?
  unsigned int nExtVar() const {return VECTOR::MEMSIZE;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 1;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_VECTOR;
        break;
      default:
        return ConstitutiveModel::TYPE_NONE;
        break;
    }
  }
  unsigned int indexExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return 0;
        break;
      default:
        return VECTOR::MEMSIZE;
        break;
    }
  }
  std::string labelExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "temperature gradient";
        break;
      default:
        return "";
        break;
    }
  }
  std::string labelExtForce(unsigned int i) const {
    switch (i) {
      case 0:
        return "heat flux";
        break;
      default:
        return "";
        break;
    }
  }
  
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
  void initState(const MaterialProperties& material,MaterialState& state) {
    ConstitutiveModel::initState(material,state);
    state.grad = 0.e0;
    state.flux = 0.e0;
    state.internal = 0.e0;
  }
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& M,
                              bool update,bool tangent) 
   throw (UpdateFailedException) {
     
    // update ?
    if (update) state.internal = state0.internal;
     
    // extract temperature gradient
    VECTOR g(state.grad);

    // compute diffusion energy
    VECTOR h(state.flux);
    SYM_TENSOR k;
    double X = conduction->diffusionEnergy(material,extPar,g,h,k,
                                           update,tangent);
    if (tangent) M = k.toMatrix();
      
    return X;
  }
};

/**
 * Base class for classical conduction potentials.
 */
template <class ALG>
class LinearConduction<ALG>::ConductionPotential {
  
 protected:
  
  // default constructor
  ConductionPotential() {}
  
 public:
  
  // destructor
  virtual ~ConductionPotential() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // apply rotation to material properties
  virtual void rotateProperties(MaterialProperties&,const Rotation&) {}
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // compute 
  virtual double diffusionEnergy(const MaterialProperties&,const ParameterSet&,
                                 const VECTOR&,VECTOR&,SYM_TENSOR&,
                                 bool,bool) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
