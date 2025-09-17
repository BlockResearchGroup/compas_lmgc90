/*
 *  $Id: Elasticity.h 169 2015-08-10 09:34:30Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ELASTICITY_H
#define ZORGLIB_MATL_MECA_LINEAR_ELASTICITY_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
// local
#include <matl/ConstitutiveModel.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

// forward declaration
template <class ALG> class ConvexElasticity;

/**
 * Base class for linear elasticity models.
 */
template <class ALG>
class Elasticity : virtual public StandardMaterial {

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // nested classes
  class Potential;
  class Dilatancy;
  
  // friend class
  friend class ConvexElasticity<ALG>;

 protected:
    
  // associated potential
  Potential *potential;

  // associated dilatancy
  Dilatancy *dilatancy;

  // instance counter
  unsigned int *count;
  
  // empty constructor
  Elasticity(Potential* p = 0,Dilatancy* d = 0) {
    count = new unsigned int(1);
    potential = p;
    dilatancy = d;
  }

 public:

  // constructors
  Elasticity(Potential& p) {
    count = new unsigned int(1);
    potential = &p;
    dilatancy = 0;
  }
  Elasticity(Potential& p,Dilatancy& d) {
    count = new unsigned int(1);
    potential = &p;
    dilatancy = &d;
  }
  
  // copy constructor
  Elasticity(const Elasticity& src) {
    count = src.count;
    (*count)++;
    potential = src.potential;
    dilatancy = src.dilatancy;
  }

  // destructor
  virtual ~Elasticity() {
    if (--(*count) > 0) return;
    delete count;
    if (potential) delete potential;
    if (dilatancy) delete dilatancy;
  }

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nLinear elastic material:" << std::endl;
     
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
    if (dilatancy) dilatancy->updateProperties(mater,extPar);
  }
  
  // how many external variables ?
  unsigned int nExtVar() const {return SYM_TENSOR::MEMSIZE;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 1;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_SYM_TENSOR;
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
        return SYM_TENSOR::MEMSIZE;
        break;
    }
  }
  std::string labelExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "deformation";
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
      default:
        return "";
        break;
    }
  }

  // how many internal variables ?
  unsigned int nIntVar() const {return 1;}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 1;}
  unsigned int getIntVar(const std::string& str) const {
    if (str == "ENRG")
      return 0;
    else
      return 1;
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    switch (i) {
      case 0:
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
      default:
        return 1;
        break;
    }
  }
  std::string labelIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "elastically stored energy";
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
	 
    // compute stored energy
    double W = storedEnergy(material,extPar,eps,sig,K,update,tangent);
    if (update) state.internal[0] = W;

    return W-state0.internal[0];
  }


 protected:
    
  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& eps,SYM_TENSOR& sig,
                      SYM_TENSOR4& M,bool first,bool second) {
    
    // elastic energy
    double W = 0.0e0;
    if (potential) 
      W = potential->storedEnergy(material,extPar,eps,sig,M,first,second);
    else {
      if (first) sig = 0.0e0;
      if (second) M = 0.0e0;
    }
    
    // dilatancy term
    if (dilatancy) {
      SYM_TENSOR sigT;
      SYM_TENSOR4 MT;
      W += dilatancy->couplingEnergy(material,extPar,eps,sigT,MT,first,second);
      if (first) sig += sigT;
      if (second) M += MT;
    }
    
    return W;
  }

  // compute the dual/conjugate energy
  double dualEnergy(const MaterialProperties& material,
                    const ParameterSet& extPar,
                    const SYM_TENSOR& sig,SYM_TENSOR& eps,
                    SYM_TENSOR4& M,bool first,bool second) {
    
    // find eps = arg sup [eps.sig - W(eps)]
    SYM_TENSOR s;
    SYM_TENSOR4 MM;
    double W = this->storedEnergy(material,extPar,eps,s,MM,first,first || second);
    if (first) {
      static const unsigned int ITMAX = 20;
      static const double PRECISION = 1.e-12;
      static const double TOLERANCE = 1.e-08;
      unsigned int iter = 0;
      double norm0;
      while (iter < ITMAX) {
        
        // evaluate residual
        SYM_TENSOR R = sig-s;
        double norm1 = normL2(R);
        //std::cout << "iter=" << iter << "-R=" << R << "-norm=" << norm1 << "(norm0=" << norm0 << ")" << std::endl;
        if (norm1 < PRECISION) break;
        if (iter > 0) {
          if (norm1 < TOLERANCE*norm0) break;
        }
        else
          norm0 = norm1;
        
        // solve
        SYM_TENSOR dEps;
        MM.solve(dEps,R,true);
        eps += dEps;
        
        // compute incremental energy
        W = this->storedEnergy(material,extPar,eps,s,MM,true,true);
        iter++;
      }
    }

    // tangents
    if (second) {
      
      // invert material tangent
      MM.invert();
      M = MM;
    }
      
    return innerProd(eps,sig)-W;
  }
};  


/**
 * Base class for elastic potentials.
 */
template <class ALG>
class Elasticity<ALG>::Potential {

 protected:
  
  // constructor
  Potential() {}

 public:

  // destructor
  virtual ~Potential() {}

  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // apply rotation to material properties
  virtual void rotateProperties(MaterialProperties&,const Rotation&) {}
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // compute stored energy
  virtual double storedEnergy(const MaterialProperties&,const ParameterSet&,
                              const SYM_TENSOR&,SYM_TENSOR&,
                              SYM_TENSOR4&,bool,bool) = 0;
  
  // compute material stiffness (Hooke) tensor
  virtual void computeStiffness(const MaterialProperties&,const ParameterSet&,
                                SYM_TENSOR4&) = 0;
};

/**
 * Base class for elastic dilatancy models.
 */
template <class ALG>
class Elasticity<ALG>::Dilatancy {
  
 protected:
  
  // constructor
  Dilatancy() {}
  
 public:
  
  // destructor
  virtual ~Dilatancy() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // apply rotation to material properties
  virtual void rotateProperties(MaterialProperties&,const Rotation&) {}
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // compute coupling energy
  virtual double couplingEnergy(const MaterialProperties&,const ParameterSet&,
                                const SYM_TENSOR&,SYM_TENSOR&,
                                SYM_TENSOR4&,bool,bool) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
