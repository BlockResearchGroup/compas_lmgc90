/*
 *  $Id: LinVariationalDiffusion.h 207 2016-08-19 16:52:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFFUSION_LINEAR_VARIATIONAL_CONDUCTION_H
#define ZORGLIB_MATL_DIFFUSION_LINEAR_VARIATIONAL_CONDUCTION_H

// config
#include <matlib_macros.h>

// local
#include <matl/ConstitutiveModel.h>
#include <matl/diff/nonlinear/VariationalDiffusion.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for (linearized) chemical capacity potentials.
 */
class LinChemicalCapacity : virtual public ChemicalCapacity {
  
 protected:
  
  // default constructor
  LinChemicalCapacity() {}
  
 public:
  
  // destructor
  virtual ~LinChemicalCapacity() {}
};


/**
 * Base class for (linearized) variational diffusion models.
 */
template <class ALG>
class LinVariationalDiffusion : virtual public StandardMaterial {

 public:
  
  // define new types
  typedef typename ALG::SymTensor SYM_TENSOR;
  typedef typename ALG::Vector    VECTOR;

  // nested classes
  class DiffusionPotential;
  
 protected:
    
  // associated capacity potential (Gibbs energy)
  LinChemicalCapacity *capacity;
  
  // associated conduction potential
  DiffusionPotential *diffusion;
  
  // instance counter
  unsigned int *count;
  
  // empty constructor
  LinVariationalDiffusion(DiffusionPotential* k = 0,LinChemicalCapacity* c = 0) {
    count = new unsigned int(1);
    capacity = c;
    diffusion = k;
  }

 public:

  // constructors
  LinVariationalDiffusion(DiffusionPotential& k) {
    count = new unsigned int(1);
    capacity = 0;
    diffusion = &k;
  };
  LinVariationalDiffusion(DiffusionPotential& k,LinChemicalCapacity& c) {
    count = new unsigned int(1);
    capacity = &c;
    diffusion = &k;
  };
  
  // copy constructor
  LinVariationalDiffusion(const LinVariationalDiffusion& src) {
    count = src.count;
    (*count)++;
    capacity = src.capacity;
    diffusion = src.diffusion;
  }
  
  // destructor
  virtual ~LinVariationalDiffusion() {
    if (--(*count) > 0) return;
    delete count;
    if (capacity) delete capacity;
    if (diffusion) delete diffusion;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nLinear variational diffusion:" << std::endl;
     
    // look for algorithmic parameter
    double alpha = 0.5;
    try {
      alpha = material.getDoubleProperty("DIFF_ALGORITHMIC_PARAMETER");
    }
    catch (NoSuchPropertyException) {
      material.setProperty("DIFF_ALGORITHMIC_PARAMETER",alpha);
    }
    if (os) (*os) << "\n\talgorithmic parameter = " << alpha << std::endl;
     
    // chemical capacity part
    if (capacity) capacity->checkProperties(material,os);
    
    // diffusion part
    if (diffusion) diffusion->checkProperties(material,os);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    if (diffusion) diffusion->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    if (capacity) capacity->updateProperties(mater,extPar);
    if (diffusion) diffusion->updateProperties(mater,extPar);
  }
  
  // how many external variables ?
  unsigned int nExtVar() const {return VECTOR::MEMSIZE+1;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 2;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_VECTOR;
        break;
      case 1:
        return ConstitutiveModel::TYPE_SCALAR;
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
      case 1:
        return VECTOR::MEMSIZE;
        break;
      default:
        return VECTOR::MEMSIZE+1;
        break;
    }
  }
  std::string labelExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "chemical potential gradient";
        break;
      case 1:
        return "chemical potential";
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
      case 1:
        return "concentration increment";
        break;
      default:
        return "";
        break;
    }
  }
  
  // how many internal variables ?
  unsigned int nIntVar() const {return 2;}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 2;}
  unsigned int getIntVar(const std::string& str) const {
    if (str == "CTRN")
      return 0;
    else if (str == "CNRG")
      return 1;
    else
      return 2;
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      case 1:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      default:
        return ConstitutiveModel::TYPE_NONE;
        break;
    }
  }
  unsigned int indexIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return 0;
        break;
      case 1:
        return 1;
        break;
      default:
        return 2;
        break;
    }
  }
  std::string labelIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "concentration";
        break;
      case 1:
        return "chemical stored energy";
        break;
      default:
        return "";
        break;
    }
  }
  
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
     
    // extract chemical potential gradient and value
    VECTOR g(state.grad);
    double mu0 = state0.grad[VECTOR::MEMSIZE];
    double mu1 = state.grad[VECTOR::MEMSIZE];
    
    // compute chemical potential for the step
    double alpha = material.getDoubleProperty("DIFF_ALGORITHMIC_PARAMETER");
    double coef = alpha*dTime;
    double mu = (1.0-alpha)*mu0+alpha*mu1;

    // compute diffusion energy
    double c0,C0;
    VECTOR j(state.flux),S0;
    SYM_TENSOR K;
    double X = diffusion->diffusionEnergy(material,extPar,g,mu,j,c0,K,S0,C0,
                                          update,tangent);
    if (update) {
      j *= dTime;
      state.flux[VECTOR::MEMSIZE] = coef*c0;
    }
    if (tangent) {
      MatLibMatrix Mred(M,VECTOR::MEMSIZE);
      Mred = dTime*K.toMatrix();
      for (unsigned int k=0; k < VECTOR::MEMSIZE; k++)
        M[VECTOR::MEMSIZE][k] = M[k][VECTOR::MEMSIZE] = coef*S0[k];
      M[VECTOR::MEMSIZE][VECTOR::MEMSIZE] = coef*C0;
    }

    // compute chemical stored energy
    double G = 0.e0;
    if (capacity) {
      double c,C;
      G = capacity->dualGibbsEnergy(material,extPar,mu1,c,C,update,tangent);
      if (update) {
        state.flux[VECTOR::MEMSIZE] += c-state0.internal[0];
        state.internal[0] = c;
        state.internal[1] = mu1*c-G;
      }
      if (tangent) {
        M[VECTOR::MEMSIZE][VECTOR::MEMSIZE] += C;
      }
    }
      
    return dTime*X+G+state0.internal[1]-state0.internal[0]*mu1;
  }
};

/**
 * Base class for (linearized) diffusion potentials.
 */
template <class ALG>
class LinVariationalDiffusion<ALG>::DiffusionPotential {
  
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
                                 const VECTOR&,double,VECTOR&,double&,
                                 SYM_TENSOR&,VECTOR&,double&,bool,bool) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
