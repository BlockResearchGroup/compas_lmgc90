/*
 *  $Id: ChemoElasticity.h 172 2015-08-24 14:44:39Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFFMECA_LINEAR_CHEMO_ELASTICITY_H
#define ZORGLIB_MATL_DIFFMECA_LINEAR_CHEMO_ELASTICITY_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/linear/Elasticity.h>
#include <matl/diff/linear/LinVariationalDiffusion.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for linear chemo-elasticity models.
 */
template <class ALG>
class ChemoElasticity : virtual public StandardMaterial {

 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // nested classes
  class Potential;
  class Dilatancy;
  
 protected:
    
  // associated potential
  Potential *potential;
  
  // associated heat capacity
  LinChemicalCapacity *capacity;
  
  // associated dilatancy
  Dilatancy *dilatancy;

  // instance counter
  unsigned int *count;
  
  // empty constructor
  ChemoElasticity(Potential* p = 0,LinChemicalCapacity* c = 0,Dilatancy* d = 0) {
    count = new unsigned int(1);
    potential = p;
    capacity  = c;
    dilatancy = d;
  }
  
 public:
    
  // constructors
  ChemoElasticity(Potential& p,LinChemicalCapacity& c) {
    count = new unsigned int(1);
    potential = &p;
    capacity  = &c;
    dilatancy = 0;
  }
  ChemoElasticity(Potential& p,LinChemicalCapacity& c,Dilatancy& d) {
    count = new unsigned int(1);
    potential = &p;
    capacity  = &c;
    dilatancy = &d;
  }
  
  // copy constructor
  ChemoElasticity(const ChemoElasticity& src) {
    count = src.count;
    (*count)++;
    potential = src.potential;
    capacity  = src.capacity;
    dilatancy = src.dilatancy;
  }
  
  // destructor
  virtual ~ChemoElasticity() {
    if (--(*count) > 0) return;
    delete count;
    if (potential) delete potential;
    if (capacity)  delete capacity;
    if (dilatancy) delete dilatancy;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nLinear chemo-elastic material:" << std::endl;
      
    // density
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      if (os) (*os) << "\n\tmass density = " << rho << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tmass density is not defined" << std::endl;
    }

    // check potential
    if (potential) potential->checkProperties(material,os);

    // check capacity
    if (capacity) capacity->checkProperties(material,os);

    // check dilatancy
    if (dilatancy) dilatancy->checkProperties(material,os);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    if (potential) potential->rotateProperties(material,R);
    if (dilatancy) dilatancy->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    if (potential) potential->updateProperties(mater,extPar);
    if (capacity) capacity->updateProperties(mater,extPar);
    if (dilatancy) dilatancy->updateProperties(mater,extPar);
  }
  
  // how many external variables ?
  unsigned int nExtVar() const {return SYM_TENSOR::MEMSIZE+1;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 2;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_SYM_TENSOR;
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
        return SYM_TENSOR::MEMSIZE;
        break;
      default:
        return SYM_TENSOR::MEMSIZE+1;
        break;
    }
  }
  std::string labelExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "deformation";
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
        return "stress";
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
  unsigned int nIntVar() const {return 3;}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 3;}
  unsigned int getIntVar(const std::string& str) const {
    if (str == "CTRN")
      return 0;
    else if (str == "ENRG")
      return 1;
    else if (str == "CNRG")
      return 2;
    else
      return 3;
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      case 1:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      case 2:
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
      case 2:
        return 2;
        break;
      default:
        return 3;
        break;
    }
  }
  std::string labelIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "concentration";
        break;
      case 1:
        return "elastically stored energy";
        break;
      case 2:
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
      
    // get tensors
    SYM_TENSOR eps(state.grad);
    SYM_TENSOR sig(state.flux);
    SYM_TENSOR4 K(M);
    
    // get chemical potential
    double mu0 = state0.grad[SYM_TENSOR::MEMSIZE];
    double mu1 = state.grad[SYM_TENSOR::MEMSIZE];
    double c,C;
    SYM_TENSOR dSig;

    // extract internal parameters
    unsigned int nIntPar = nIntVar()-1;
    MatLibArray intPar(state.internal,nIntPar,1);
    c = -state0.internal[0]; // initialization

    // compute stored energy (dual)
    double W = chemoElasticUpdate(material,extPar,intPar,eps,mu1,
                                  sig,c,K,dSig,C,update,tangent);
     
    // update
    if (update) {
      state.flux[SYM_TENSOR::MEMSIZE] = c+state0.internal[0];
      state.internal[0] = -c;
    }
    if (tangent) {
      M[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE] = C;
      for (unsigned int i=0; i < SYM_TENSOR::MEMSIZE; i++)
        M[i][SYM_TENSOR::MEMSIZE] = M[SYM_TENSOR::MEMSIZE][i] = dSig[i];
    }
    
    return W-state0.internal[1]-state0.internal[2]
           +state0.internal[0]*(mu1-mu0);
  }
  
 protected:
    
  // compute stored energy (as function of strain and concentration, i.e. internal energy)
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,MatLibArray& intPar,
                      const SYM_TENSOR& eps,double c,
                      SYM_TENSOR& sig,double& mu,
                      SYM_TENSOR4& M,SYM_TENSOR& dSig,double& C,
                      bool first,bool second) {

    // elastic energy
    double W = 0.0e0;
    if (potential) 
      W = potential->storedChMEnergy(material,extPar,eps,c,sig,mu,
                                     M,dSig,C,first,second);
    else {
      if (first) {
        sig = 0.0e0;
        mu = 0.0e0;
      }
      if (second) {
        M = 0.0e0;
        dSig = 0.0e0;
        C = 0.0e0;
      }
    }

    // dilatancy term
    if (dilatancy) {
      double mu0,C0;
      SYM_TENSOR sigT,dSigT;
      SYM_TENSOR4 MT;
      W += dilatancy->couplingChMEnergy(material,extPar,eps,c,sigT,mu0,
                                        MT,dSigT,C0,first,second);
      if (first) {
        sig += sigT;
        mu += mu0;
      }
      if (second) {
        M += MT;
        dSig += dSigT;
        C += C0;
      }
    }
    intPar[0] = W;
    
    // chemical capacity
    if (capacity) {
      double muC,CC;
      double WC = capacity->GibbsEnergy(material,extPar,c,muC,
                                        CC,first,second);
      W += WC;
      if (first) {
        mu += muC;
      }
      if (second) C += CC;
    }
    intPar[1] = W-intPar[0];

    return W;
  }
        
  // compute stored energy (as a function of strain and chemical potential)
  double chemoElasticUpdate(const MaterialProperties& material,
                            const ParameterSet& extPar,MatLibArray& intPar,
                            const SYM_TENSOR& eps,double mu,
                            SYM_TENSOR& sig,double& c,
                            SYM_TENSOR4& M,SYM_TENSOR& dSig,double& C,
                            bool first,bool second) {
    
    // find c = arg inf [G(eps,-c) + mu.c]
    double m,CC;
    SYM_TENSOR ds;
    SYM_TENSOR4 MM;
    double W = this->storedEnergy(material,extPar,intPar,eps,-c,sig,m,
                                  MM,ds,CC,first,first || second);
    if (first) {
      static const unsigned int ITMAX = 20;
      static const double PRECISION = 1.e-12;
      static const double TOLERANCE = 1.e-08;
      unsigned int iter = 0;
      double norm0;
      while (iter < ITMAX) {
        
        // evaluate residual
        double res = mu-m;
        double norm1 = std::fabs(res);
        if (norm1 < PRECISION) break;
        if (iter > 0) {
          if (norm1 < TOLERANCE*norm0) break;
        }
        else
          norm0 = norm1;
        
        // solve
        c -= res/CC;
        
        // compute incremental energy
        W = this->storedEnergy(material,extPar,intPar,eps,-c,sig,m,
                               MM,ds,CC,true,true);
        iter++;
      }
    }
    
    // tangents
    if (second) {
      
      // correct material tangents
      C = -1.0/CC;
      dSig = -C*ds;
      M = MM+C*outerProd(ds,ds);
    }
    
    return W+mu*c;
  }
};


/**
 * Base class for chemoelastic potentials.
 */
template <class ALG>
class ChemoElasticity<ALG>::Potential : virtual public Elasticity<ALG>::Potential {
  
 protected:
  
  // constructor
  Potential() {}
  
 public:
  
  // destructor
  virtual ~Potential() {}
  
  // compute stored energy
  virtual double storedChMEnergy(const MaterialProperties&,const ParameterSet&,
                                 const SYM_TENSOR&,double,SYM_TENSOR&,double&,
                                 SYM_TENSOR4&,SYM_TENSOR&,double&,bool,bool) = 0;
};

/**
 * Base class for thermoelastic dilatancy models.
 */
template <class ALG>
class ChemoElasticity<ALG>::Dilatancy : virtual public Elasticity<ALG>::Dilatancy {
  
 protected:
  
  // constructor
  Dilatancy() {}
  
 public:
  
  // destructor
  virtual ~Dilatancy() {}
  
  // compute coupling energy
  virtual double couplingChMEnergy(const MaterialProperties&,const ParameterSet&,
                                   const SYM_TENSOR&,double,SYM_TENSOR&,double&,
                                   SYM_TENSOR4&,SYM_TENSOR&,double&,bool,bool) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
