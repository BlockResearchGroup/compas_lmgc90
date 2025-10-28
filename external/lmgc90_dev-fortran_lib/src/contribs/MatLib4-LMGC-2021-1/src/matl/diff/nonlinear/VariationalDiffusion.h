/*
 *  $Id: VariationalDiffusion.h 208 2016-08-19 17:16:35Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFFUSION_NONLINEAR_VARIATIONAL_CONDUCTION_H
#define ZORGLIB_MATL_DIFFUSION_NONLINEAR_VARIATIONAL_CONDUCTION_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>

// local
#include <matl/ConstitutiveModel.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for (non-linear) chemical capacity potentials.
 */
class ChemicalCapacity {
  
 protected:
  
  // default constructor
  ChemicalCapacity() {}
  
 public:
  
  // destructor
  virtual ~ChemicalCapacity() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // compute 
  virtual double GibbsEnergy(const MaterialProperties&,const ParameterSet&,
                             double,double&,double&,bool,bool) = 0;
  virtual double dualGibbsEnergy(const MaterialProperties&,const ParameterSet&,
                                 double,double&,double&,bool,bool) = 0;
};


/**
 * Base class for (non-linear) variational diffusion models.
 */
template <class ALG>
class VariationalDiffusion : virtual public StandardMaterial {

 public:
  
  // define new types
  typedef typename ALG::SymTensor SYM_TENSOR;
  typedef typename ALG::Vector    VECTOR;

  // nested classes
  class DiffusionPotential;
  
 protected:
    
  // associated capacity potential (Gibbs energy)
  ChemicalCapacity *capacity;
  
  // associated conduction potential
  DiffusionPotential *diffusion;
  
  // instance counter
  unsigned int *count;
  
  // empty constructor
  VariationalDiffusion(DiffusionPotential* k = 0,ChemicalCapacity* c = 0) {
    count = new unsigned int(1);
    capacity = c;
    diffusion = k;
  }

 public:

  // constructors
  VariationalDiffusion(DiffusionPotential& k) {
    count = new unsigned int(1);
    capacity = 0;
    diffusion = &k;
  };
  VariationalDiffusion(DiffusionPotential& k,ChemicalCapacity& c) {
    count = new unsigned int(1);
    capacity = &c;
    diffusion = &k;
  };
  
  // copy constructor
  VariationalDiffusion(const VariationalDiffusion& src) {
    count = src.count;
    (*count)++;
    capacity = src.capacity;
    diffusion = src.diffusion;
  }
  
  // destructor
  virtual ~VariationalDiffusion() {
    if (--(*count) > 0) return;
    delete count;
    if (capacity) delete capacity;
    if (diffusion) delete diffusion;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nVariational diffusion:" << std::endl;
     
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
  bool isLinear() const {return false;}
  
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
    double mu = state.grad[VECTOR::MEMSIZE];

    // update concentration
    double c = state.internal[0];
    double G,C;
    VECTOR j(state.flux),S;
    SYM_TENSOR K;
    double dG = updateConcentration(material,extPar,dTime,g,mu,state0.internal[0],state0.internal[1],
                                    G,j,c,K,S,C,update,update,tangent);

    // update fluxes
    if (update) {
      state.flux[VECTOR::MEMSIZE] = c-state0.internal[0];
      state.internal[0] = c;
      state.internal[1] = G;
    }
    if (tangent) {
      MatLibMatrix Mred(M,VECTOR::MEMSIZE);
      Mred = K.toMatrix();
      for (unsigned int k=0; k < VECTOR::MEMSIZE; k++)
        M[VECTOR::MEMSIZE][k] = M[k][VECTOR::MEMSIZE] = S[k];
      M[VECTOR::MEMSIZE][VECTOR::MEMSIZE] = C;
    }
     
    return dG;
  }
      
  // update concentrations
  double updateConcentration(const MaterialProperties& material,
                             const ParameterSet& extPar,
                             double dTime,const VECTOR& g,double mu,
                             double c0,double G0,double& G1,
                             VECTOR& j,double& c1,
                             SYM_TENSOR& K,VECTOR& S,double& C,
                             bool update,bool first,bool second) {
    
    static const unsigned int ITMAX = 10;
    static const double PRECISION = 1.e-12;
    static const double TOLERANCE = 1.e-08;
    
    // compute chemical potential
    double mu0,C0;
    G1 = capacity->GibbsEnergy(material,extPar,c1,mu0,C0,update || first,update || second);

    // compute concentration for the step
    double alpha = material.getDoubleProperty("DIFF_ALGORITHMIC_PARAMETER");
    double coef = alpha*dTime;
    double c = (1.0-alpha)*c0+alpha*c1;

    // compute dissipation function
    double muX,CX;
    double X = diffusion->diffusionEnergy(material,extPar,g,c,j,muX,K,S,CX,update || first,update || second);

    // update concentration
    if (update) {

      // iterate over concentration
      double test = -mu0+mu+coef*muX;
      double test0 = TOLERANCE*std::fabs(test);
      if (test0 < PRECISION) test0 = PRECISION;
      unsigned int iter;
      for (iter=0; iter < ITMAX && std::fabs(test) > test0; iter++) {
        
        // compute correction on concentration
        double H = -C0+alpha*coef*CX;
        double dc = -test/H;
        if (std::fabs(dc) < PRECISION) break;

        // compute chemical potential
        c1 += dc;
        G1 = capacity->GibbsEnergy(material,extPar,c1,mu0,C0,true,true);
        
        // compute dissipation function
        c += alpha*dc;
        X = diffusion->diffusionEnergy(material,extPar,g,c,j,muX,K,S,CX,true,true);
        
        test = -mu0+mu+coef*muX;
      }
      // check convergence
      if (iter == ITMAX) {
        throw UpdateFailedException("no convergence in internal update");
      }
    }
    
    // derivatives
    if (first) j *= dTime;
    if (second) {
      C = 1.0e0/(C0-alpha*coef*CX);
      S *= coef*C;
      K += (alpha*coef*C)*SYM_TENSOR::outerProd(S);
      K *= dTime;
    }

    return -G1+G0+mu*(c-c0)+dTime*X;
  }
};

/**
 * Base class for diffusion potentials.
 */
template <class ALG>
class VariationalDiffusion<ALG>::DiffusionPotential {
  
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
