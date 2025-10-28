/*
 *  $Id: LinearDiffusion.h 236 2017-06-06 09:11:19Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFF_LINEAR_CLASSICAL_DIFFUSION_H
#define ZORGLIB_MATL_DIFF_LINEAR_CLASSICAL_DIFFUSION_H

// config
#include <matlib_macros.h>

// local
#include <matl/ConstitutiveModel.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif


/**
 * Base class for classical linear diffusion models.
 */
template <class ALG>
class LinearDiffusion : virtual public StandardMaterial {

 public:
  
  // define new types
  typedef typename ALG::SymTensor SYM_TENSOR;
  typedef typename ALG::Vector    VECTOR;

  // nested classes
  class DiffusionPotential;
  
 protected:
  
  // associated diffusion potential
  DiffusionPotential *diffusion;
  
  // instance counter
  unsigned int *count;
  
  // empty constructor
  LinearDiffusion(DiffusionPotential* D = 0) {
    count = new unsigned int(1);
    diffusion = D;
  };

 public:

  // constructor
  LinearDiffusion(DiffusionPotential& D) {
    count = new unsigned int(1);
    diffusion = &D;
  }
  
  // copy constructor
  LinearDiffusion(const LinearDiffusion& src) {
    count = src.count;
    (*count)++;
    diffusion = src.diffusion;
  }
  
  // destructor
  virtual ~LinearDiffusion() {
    if (--(*count) > 0) return;
    delete count;
    if (diffusion) delete diffusion;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nLinear classical diffusion:" << std::endl;
     
    // chemical modulus
    try {
      double C = material.getDoubleProperty("CHEMICAL_MODULUS");
      double Cinv = 1.0e0/C;
      material.setProperty("CHEMICAL_COMPLIANCE",Cinv);
      if (os) (*os) << "\n\tchemical modulus = " << C << std::endl;
    }
    catch (NoSuchPropertyException) {
      try{
        // compute from gas constant, absolute temperature and reference concentration
        double R;
        try {
          R = material.getDoubleProperty("UNIVERSAL_GAS_CONSTANT");
          if (R <= 0.0e0) {
            if (os) (*os) << "ERROR: gas constant must be strictly positive." << std::endl;
            throw InvalidPropertyException("gas constant");
          }
        }
        catch (NoSuchPropertyException) {
          R = 8.31446; // default value in S.I. units (J.K^-1.mol^-1)
          material.setProperty("UNIVERSAL_GAS_CONSTANT",R);
        }
        double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
        if (T0 <= 0.0e0) {
          if (os) (*os) << "ERROR: reference temperature must be strictly positive." << std::endl;
          throw InvalidPropertyException("reference temperature");
        }
        double c0 = material.getDoubleProperty("REFERENCE_CONCENTRATION");
        if (c0 <= 0.0e0) {
          if (os) (*os) << "ERROR: reference concentration must be strictly positive." << std::endl;
          throw InvalidPropertyException("reference concentration");
        }
        double C = (R*T0)/c0;
        material.setProperty("CHEMICAL_MODULUS",C);
        double Cinv = 1.0e0/C;
        material.setProperty("CHEMICAL_COMPLIANCE",Cinv);

        // print-out
        if (os) {
          (*os) << "\n\tuniversal gas constant  = " << R;
          (*os) << "\n\treference temperature   = " << T0;
          (*os) << "\n\treference concentration = " << c0;
          (*os) << "\n\tchemical modulus        = " << C << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        if (os) (*os) << "\n\tchemical modulus cannot be defined" << std::endl;
      }
    }
     
    // diffusion part
    diffusion->checkProperties(material,os);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    diffusion->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    diffusion->updateProperties(mater,extPar);
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
        return "chemical gradient";
        break;
      default:
        return "";
        break;
    }
  }
  std::string labelExtForce(unsigned int i) const {
    switch (i) {
      case 0:
        return "flux";
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
    double X = diffusion->diffusionEnergy(material,extPar,g,h,k,
                                          update,tangent);
    if (tangent) M = k.toMatrix();
      
    return X;
  }
};

/**
 * Base class for classical diffusion potentials.
 */
template <class ALG>
class LinearDiffusion<ALG>::DiffusionPotential {
  
 protected:
  
  // default constructor
  DiffusionPotential() {}
  
 public:
  
  // destructor
  virtual ~DiffusionPotential() {}
  
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
