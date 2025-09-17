/*
 *  $Id: LinVariationalConduction.h 183 2015-10-18 17:19:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_THERMO_LINEAR_VARIATIONAL_CONDUCTION_H
#define ZORGLIB_MATL_THERMO_LINEAR_VARIATIONAL_CONDUCTION_H

// config
#include <matlib_macros.h>

// local
#include <matl/ConstitutiveModel.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for (linearized) thermal capacity potentials.
 */
class LinThermalCapacity {
  
 protected:
  
  // default constructor
  LinThermalCapacity() {}
  
 public:
  
  // destructor
  virtual ~LinThermalCapacity() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0) 
  throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // compute 
  virtual double internalEnergy(const MaterialProperties&,const ParameterSet&,
                                double,double&,double&,bool,bool) = 0;
};


/**
 * Base class for (linearized) variational conduction models.
 */
template <class ALG>
class LinVariationalConduction : virtual public StandardMaterial {

 public:
  
  // define new types
  typedef typename ALG::SymTensor SYM_TENSOR;
  typedef typename ALG::Vector    VECTOR;

  // nested classes
  class ConductionPotential;
  
 protected:
    
  // associated capacity potential (internal energy)
  LinThermalCapacity *capacity;
  
  // associated conduction potential
  ConductionPotential *conduction;
  
  // instance counter
  unsigned int *count;
  
  // empty constructor
  LinVariationalConduction(ConductionPotential* k = 0,LinThermalCapacity* c = 0) {
    count = new unsigned int(1);
    capacity = c;
    conduction = k;
  }

 public:

  // constructors
  LinVariationalConduction(ConductionPotential& k) {
    count = new unsigned int(1);
    capacity = 0;
    conduction = &k;
  };
  LinVariationalConduction(ConductionPotential& k,LinThermalCapacity& c) {
    count = new unsigned int(1);
    capacity = &c;
    conduction = &k;
  };
  
  // copy constructor
  LinVariationalConduction(const LinVariationalConduction& src) {
    count = src.count;
    (*count)++;
    capacity = src.capacity;
    conduction = src.conduction;
  }
  
  // destructor
  virtual ~LinVariationalConduction() {
    if (--(*count) > 0) return;
    delete count;
    if (capacity) delete capacity;
    if (conduction) delete conduction;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nLinear variational conduction:" << std::endl;
     
    // look for algorithmic parameter
    double alpha = 0.5;
    try {
      alpha = material.getDoubleProperty("TH_ALGORITHMIC_PARAMETER");
    }
    catch (NoSuchPropertyException) {
      material.setProperty("TH_ALGORITHMIC_PARAMETER",alpha);
    }
    if (os) (*os) << "\n\talgorithmic parameter = " << alpha << std::endl;

    // reference temperature
    try {
      double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
      if (TRef <= 0.e0) {
        if (os) (*os) << "ERROR: reference temperature must be strictly positive." << std::endl;
        throw InvalidPropertyException("reference temperature");
      }
      if (os) (*os) << "\n\treference temperature = " << TRef << std::endl;
    }
    catch (NoSuchPropertyException) {
      // use initial temperature
      try {
        double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
        if (T0 <= 0.e0) {
          if (os) (*os) << "ERROR: initial temperature must be strictly positive." << std::endl;
          throw InvalidPropertyException("initial temperature");
        }
        material.setProperty("REFERENCE_TEMPERATURE",T0);
        if (os) (*os) << "\n\treference temperature = " << T0 << std::endl;
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: reference temperature cannot be set." << std::endl;
        throw e;
      }
    }

    // heat capacity part
    if (capacity) capacity->checkProperties(material,os);
    
    // conduction part
    if (conduction) conduction->checkProperties(material,os);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    if (conduction) conduction->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    if (capacity) capacity->updateProperties(mater,extPar);
    if (conduction) conduction->updateProperties(mater,extPar);
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
        return "generalized temperature gradient";
        break;
      case 1:
        return "temperature increment";
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
      case 1:
        return "entropy increment";
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
    if (str == "ENTP")
      return 0;
    else if (str == "TNRG")
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
        return "entropy";
        break;
      case 1:
        return "thermally stored energy";
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

    // extract temperature gradient and temperature
    VECTOR g(state.grad);
    double Th0 = state0.grad[VECTOR::MEMSIZE];
    double Th1 = state.grad[VECTOR::MEMSIZE];
    
    // compute (relative) temperature for the step
    double alpha = material.getDoubleProperty("TH_ALGORITHMIC_PARAMETER");
    double Th = (1.0-alpha)*Th0+alpha*Th1;

    // get reference temperature
    double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    double dT = Th1-Th0;
    double T1 = T0+dT;
    double coef2 = alpha*dT;

    // get temperature ratio(s)
    double coef0 = T0/T1;
    double coef1 = 1.0-coef0;

    // compute diffusion energy
    double N0,C0;
    VECTOR h0,h1,h(state.flux),S0;
    SYM_TENSOR K0,K1;
    double X0 = conduction->diffusionEnergy(material,extPar,g,Th0,h0,N0,K0,S0,C0,
                                            update,tangent);
    double X1 = conduction->diffusionEnergy(material,extPar,g,Th,h1,N0,K1,S0,C0,
                                            update,tangent);
    if (update) {
      h = dTime*(coef0*h0+coef1*h1);
      state.flux[VECTOR::MEMSIZE] = dTime*(coef2*N0+coef0*(X1-X0))/T1;
    }
    if (tangent) {
      MatLibMatrix Mred(M,VECTOR::MEMSIZE);
      Mred = dTime*(coef0*K0.toMatrix()+coef1*K1.toMatrix());
      double val = dTime/T1;
      for (unsigned int k=0; k < VECTOR::MEMSIZE; k++)
        M[VECTOR::MEMSIZE][k] = M[k][VECTOR::MEMSIZE] = val*(coef2*S0[k]+coef0*(h1[k]-h0[k]));
      M[VECTOR::MEMSIZE][VECTOR::MEMSIZE] = val*(alpha*coef2*C0+2*alpha*coef0*N0-2*coef0*(X1-X0)/T1);
    }

    // compute thermally stored energy
    double W = 0.e0;
    if (capacity) {
      double N,C;
      W = capacity->internalEnergy(material,extPar,Th1,N,C,update,tangent);
      if (update) {
        state.flux[VECTOR::MEMSIZE] -= N+state0.internal[0];
        state.internal[0] = -N;
        state.internal[1] = W;
      }
      if (tangent) {
        M[VECTOR::MEMSIZE][VECTOR::MEMSIZE] -= C;
      }
    }
      
    return dTime*(coef0*X0+coef1*X1)-(W-state0.internal[1])-state0.internal[0]*dT;
  }
};

/**
 * Base class for (linearized) conduction potentials.
 */
template <class ALG>
class LinVariationalConduction<ALG>::ConductionPotential {
  
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
                                 const VECTOR&,double,VECTOR&,double&,
                                 SYM_TENSOR&,VECTOR&,double&,bool,bool) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
