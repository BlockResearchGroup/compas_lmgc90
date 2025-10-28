/*
 *  $Id: ThermoHyperElasticity.h 142 2014-02-07 12:51:54Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_HYPER_ELASTICITY_H
#define ZORGLIB_MATL_MECA_THERMO_HYPER_ELASTICITY_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
// local
#include <matl/meca/hyper/HyperElasticity.h>
#include <matl/thermo/nonlinear/VariationalConduction.h>
#include <matl/thermomeca/eos/ThermalEOS.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for thermohyperelasticity models.
 */
template <class ALG>
class ThermoHyperElasticity : virtual public StandardMaterial {

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor4          TENSOR4;

  // nested classes
  class Potential;
  class Dilatancy;

 protected:
  
  // associated potential
  Potential *potential;
  
  // associated equation-of-state
  ThermalEOS *eos;
  
  // associated heat capacity
  ThermalCapacity *capacity;
  
  // associated dilatancy
  Dilatancy *dilatancy;
  
  // instance counter
  unsigned int *count;
  
  // default constructor
  ThermoHyperElasticity(Potential* p = 0,ThermalEOS* e = 0,
                        ThermalCapacity* c = 0,Dilatancy* d = 0) {
    count = new unsigned int(1);
    potential = p;
    eos       = e;
    capacity  = c;
    dilatancy = d;
  }
  
 public:
    
  // constructors
  ThermoHyperElasticity(Potential& p,ThermalCapacity& c) {
    count = new unsigned int(1);
    potential = &p;
    eos       =  0;
    capacity  = &c;
    dilatancy =  0;
  }
  ThermoHyperElasticity(Potential& p,ThermalCapacity& c,ThermalEOS& e) {
    count = new unsigned int(1);
    potential = &p;
    eos       = &e;
    capacity  = &c;
    dilatancy =  0;
  }
  ThermoHyperElasticity(Potential& p,ThermalCapacity& c,Dilatancy& d) {
    count = new unsigned int(1);
    potential = &p;
    eos       =  0;
    capacity  = &c;
    dilatancy = &d;
  }
  ThermoHyperElasticity(Potential& p,ThermalCapacity& c,
                        ThermalEOS& e,Dilatancy& d) {
    count = new unsigned int(1);
    potential = &p;
    eos       = &e;
    capacity  = &c;
    dilatancy = &d;
  }
  
  // copy constructor
  ThermoHyperElasticity(const ThermoHyperElasticity& src) {
    count = src.count;
    (*count)++;
    potential = src.potential;
    eos = src.eos;
    capacity = src.capacity;
    dilatancy = src.dilatancy;
  }
  
  // destructor
  virtual ~ThermoHyperElasticity() {
    if (--(*count) > 0) return;
    delete count;
    if (potential) delete potential;
    if (eos) delete eos;
    if (capacity) delete capacity;
    if (dilatancy) delete dilatancy;
  }

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
     if (os) (*os) << "\nThermohyperelastic material:" << std::endl;
     
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
     
     // eos
     if (eos) eos->checkProperties(material,os);
     
     // check capacity
     if (capacity) capacity->checkProperties(material,os);
     
     // check dilatancy
     if (dilatancy) dilatancy->checkProperties(material,os);
     
     // initial temperature
     double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
     if (T0 <= 0.e0) {
       if (os) (*os) << "ERROR: Initial temperature must be strictly positive." << std::endl;
       throw InvalidPropertyException("initial temperature");
     }
     if (os) (*os) << "\n\tinitial temperature = " << T0 << std::endl;
   }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    if (potential) potential->rotateProperties(material,R);
    if (dilatancy) dilatancy->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    if (potential) potential->updateProperties(mater,extPar);
    if (eos) eos->updateProperties(mater,extPar);
    if (capacity) capacity->updateProperties(mater,extPar);
    if (dilatancy) dilatancy->updateProperties(mater,extPar);
  }
  
  // how many external variables ?
  unsigned int nExtVar() const {return TENSOR::MEMSIZE+1;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 2;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_TENSOR;
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
        return TENSOR::MEMSIZE;
        break;
      default:
        return TENSOR::MEMSIZE+1;
        break;
    }
  }
  std::string labelExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "deformation";
        break;
      case 1:
        return "temperature";
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
        return "entropy difference";
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
    if (str == "ENTP")
      return 0;
    else if (str == "ENRG")
      return 1;
    else if (str == "TNRG")
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
        return "entropy";
        break;
      case 1:
        return "elastically stored energy";
        break;
      case 2:
        return "thermally stored energy";
        break;
      default:
        return "";
        break;
    }
  }
  
  // initialize the state of the material
  void initState(const MaterialProperties& material,MaterialState& state) {
    ConstitutiveModel::initState(material,state);
    
    // set the gradient of deformation
    TENSOR F(state.grad);
    F = TENSOR::identity();
    
    // set the initial temperature
    double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
    state.grad[TENSOR::MEMSIZE] = T0;
    
    // set everything else to zero
    state.flux = 0.e0;
    state.internal = 0.e0;
  }
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& T,
                              bool update,bool tangent) 
   throw (UpdateFailedException) {
    
    // get tensors
    TENSOR F(state.grad);
    TENSOR P(state.flux);
    TENSOR4 K(T);

    // right Cauchy-Green tensor
    SYM_TENSOR C;
    ALG::RightCauchyGreen(F,C);
    
    // temperature
    double T0 = state0.grad[TENSOR::MEMSIZE];
    double T1 = state.grad[TENSOR::MEMSIZE];

    // compute stored energy
    double N,Cm;
    SYM_TENSOR S,dS;
    SYM_TENSOR4 M;
    double W = storedEnergy(material,extPar,C,T1,S,N,M,dS,Cm,
                            update || tangent,tangent);
    state.internal[1] = W;
    
    // thermal capacity
    if (capacity) {
      double NT,CT;
      double WT = capacity->internalEnergy(material,extPar,T1,NT,
                                           CT,update,tangent);
      W += WT;
      if (update) {
        state.internal[2] = WT;
        N += NT;
      }
      if (tangent) Cm += CT;
    }
    
    // update (compute Piola tensor and Lagrangian tangents)
    if (update) {
      ALG::PK2ToPK1(S,F,P);
      state.flux[TENSOR::MEMSIZE] = N+state0.internal[0];
      state.internal[0] = -N;
    }
    if (tangent) {
      TENSOR dP;
      ALG::PK2ToPK1(dS,F,dP);
      ALG::MaterialToLagrangian(M,S,F,K);
      T[TENSOR::MEMSIZE][TENSOR::MEMSIZE] = Cm;
      for (unsigned int i=0; i < TENSOR::MEMSIZE; i++)
        T[i][TENSOR::MEMSIZE] = T[TENSOR::MEMSIZE][i] = dP[i];
    }
    
    return W-state0.internal[1]-state0.internal[2]
          +state0.internal[0]*(T1-T0);
  }

 protected:

  // compute stored energy, accounting for EOS
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& C,double T,
                      SYM_TENSOR& S,double& N,
                      SYM_TENSOR4& M,SYM_TENSOR& dS,double& Cm,
                      bool first,bool second) {
    double W = 0.e0;
    
    static const double ONE_THIRD = 1./3.;
    static const double TWO_THIRD = 2./3.;
    
    // if there is an e.o.s.
    if (potential && eos) {
      
      // compute distortion strain
      double J;
      SYM_TENSOR Cinv,Cbar;
      if (first || second)
        Cinv = C.inverse(J);
      else
        J = determinant(C);
      J = std::sqrt(J);
      double coef = std::pow(J,-TWO_THIRD);
      Cbar = coef*C;
      
      // deviatoric part
      SYM_TENSOR Sbar,dSbar;
      SYM_TENSOR4 Mbar;
      W = potential->storedThMEnergy(material,extPar,Cbar,T,Sbar,N,
                                     Mbar,dSbar,Cm,first,second);
      
      // volumic part
      double p,K,Nv,dp,Cmv;
      W += eos->storedThMEnergy(material,extPar,J,T,p,Nv,K,dp,Cmv,
                                first || second,second);

      // add contributions
      double press=0.e0,trS=0.e0;
      if (first || second) {
        
        // Lagrangian pressure
        press = J*p;
        
        // compute stress (Lagrangian) trace
        trS = ONE_THIRD*innerProd2(Sbar,C);
        
        // compute stresses (PK2)
        S = coef*Sbar+(press-coef*trS)*Cinv;
      }

      if (first) N += Nv;

      if (second) {
        double coef1 = TWO_THIRD*coef;
        double coef2 = coef*coef;
        
        SYM_TENSOR tmp = ONE_THIRD*innerProd2(Mbar,C);
        double CMC = ONE_THIRD*innerProd2(C,tmp);
        
        double val1 = coef*trS-press;
        double val2 = press+K*J*J;
        M = coef2*(Mbar-outerProd(Cinv,tmp)-outerProd(tmp,Cinv))
           -coef1*(outerProd(Cinv,Sbar)+outerProd(Sbar,Cinv))
           +(coef2*CMC+coef1*trS+val2)*outerProd(Cinv,Cinv);
        M.addIJKL(val1,Cinv);
        
        double trdS = ONE_THIRD*innerProd2(Sbar,Cbar);
        dS = coef*dSbar+(dp*J-trdS)*Cinv;
        Cm += Cmv;
      }
    }
    // no e.o.s.
    else if (potential) {
      W = potential->storedThMEnergy(material,extPar,C,T,S,N,M,dS,Cm,first,second);
    }
    // e.o.s. only (makes little sense here)
    else if (eos) {
      // TO DO
    }
    else {
      if (first) {
        S = 0.0e0;
        N = 0.0e0;
      }
      if (second) {
        M = 0.0e0;
        dS = 0.0e0;
        Cm = 0.0e0;
      }
    }
    
    // dilatancy term
    if (dilatancy) {
      double NT,CmT;
      SYM_TENSOR ST,dST;
      SYM_TENSOR4 MT;
      W += dilatancy->couplingThMEnergy(material,extPar,C,T,ST,NT,
                                        MT,dST,CmT,first,second);
      if (first) {
        S += ST;
        N += NT;
      }
      if (second) {
        M += MT;
        dS += dST;
        Cm += CmT;
      }
    }
    
    return W;
  }
};  


/**
 * Base class for thermohyperelastic potentials.
 */
template <class ALG>
class ThermoHyperElasticity<ALG>::Potential
: virtual public HyperElasticity<ALG>::Potential {

 protected:
  
  // constructor
  Potential() {}

 public:

  // destructor
  virtual ~Potential() {}

  // compute stored energy (with explicit temperature dependence)
  virtual double storedThMEnergy(const MaterialProperties&,const ParameterSet&,
                                 const SYM_TENSOR&,double,SYM_TENSOR&,double&,
                                 SYM_TENSOR4&,SYM_TENSOR&,double&,bool,bool) = 0;
};


/**
 * Base class for temperature-dependent dilatancy potentials.
 */
template <class ALG>
class ThermoHyperElasticity<ALG>::Dilatancy
: virtual public HyperElasticity<ALG>::Dilatancy {
  
 protected:
  
  // constructor
  Dilatancy() {}
  
 public:
  
  // destructor
  virtual ~Dilatancy() {}
  
  // definition of the coupling energy (with explicit temperature dependence)
  virtual double couplingThMEnergy(const MaterialProperties&,const ParameterSet&,
                                   const SYM_TENSOR&,double,SYM_TENSOR&,double&,
                                   SYM_TENSOR4&,SYM_TENSOR&,double&,bool,bool) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
