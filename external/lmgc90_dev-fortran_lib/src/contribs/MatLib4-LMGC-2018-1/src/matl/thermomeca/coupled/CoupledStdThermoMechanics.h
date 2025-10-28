/*
 *  $Id: CoupledStdThermoMechanics.h 237 2017-06-06 09:13:56Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_COUPLED_STANDARD_THERMO_MECHANICS_H
#define ZORGLIB_MATL_MECA_COUPLED_STANDARD_THERMO_MECHANICS_H

// config
#include <matlib_macros.h>

// local
#include <matl/thermomeca/hyper/ThermoHyperElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for finite strains thermo-mechanical models
 * coupled with non-linear variational conduction.
 */
template <class ALG1,class ALG2>
class CoupledStdThermoMechanics : virtual public StandardMaterial {
  
 public:
  
  // define new types
  typedef typename ALG1::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG1::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG1::Tensor::TYPE     TENSOR;
  typedef typename ALG1::Tensor4          TENSOR4;
  typedef typename ALG2::Vector           VECTOR;

  // nested class
  class ConductionPotential;
  
 protected:
    
  // thermo-mechanical model
  ThermoHyperElasticity<ALG1> *mechanics;
  
  // thermo-mechanical conduction model
  ConductionPotential *conduction;
  
  // instance counter
  unsigned int *count;
  
  // empty constructor
  CoupledStdThermoMechanics(ThermoHyperElasticity<ALG1>* m = 0,
                            ConductionPotential* k = 0) {
    count = new unsigned int(1);
    mechanics  = m;
    conduction = k;
  }
  
 public:
    
  // constructor
  CoupledStdThermoMechanics(ThermoHyperElasticity<ALG1>& m,
                            ConductionPotential& k) {
    count = new unsigned int(1);
    mechanics  = &m;
    conduction = &k;
  }
  
  // copy constructor
  CoupledStdThermoMechanics(const CoupledStdThermoMechanics& src) {
    count = src.count;
    (*count)++;
    mechanics  = src.mechanics;
    conduction = src.conduction;
  }
  
  // destructor
  virtual ~CoupledStdThermoMechanics() {
    if (--(*count) > 0) return;
    delete count;
    if (mechanics) delete mechanics;
    if (conduction) delete conduction;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nCoupled nonlinear thermo-mechanical material:" << std::endl;

    // check mechanics
    mechanics->checkProperties(material,os);

    // look for algorithmic parameter
    double alpha = 0.5;
    try {
      alpha = material.getDoubleProperty("THM_ALGORITHMIC_PARAMETER");
    }
    catch (NoSuchPropertyException) {
      try {
        alpha = material.getDoubleProperty("TH_ALGORITHMIC_PARAMETER");
        material.setProperty("THM_ALGORITHMIC_PARAMETER",alpha);
      }
      catch (NoSuchPropertyException) {
        material.setProperty("THM_ALGORITHMIC_PARAMETER",alpha);
      }
    }
    if (os) (*os) << "\n\talgorithmic parameter = " << alpha << std::endl;
      
    // conduction part
    conduction->checkProperties(material,os);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    mechanics->rotateProperties(material,R);
    conduction->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    mechanics->updateProperties(mater,extPar);
    conduction->updateProperties(mater,extPar);
  }
  
  // how many external variables ?
  unsigned int nExtVar() const {return TENSOR::MEMSIZE+1+VECTOR::MEMSIZE;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 3;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_TENSOR;
        break;
      case 1:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      case 2:
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
      case 1:
        return TENSOR::MEMSIZE;
        break;
      case 2:
        return TENSOR::MEMSIZE+1;
        break;
      default:
        return TENSOR::MEMSIZE+1+VECTOR::MEMSIZE;
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
      case 2:
        return "generalized temperature gradient";
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
      case 2:
        return "heat flux";
        break;
      default:
        return "";
        break;
    }
  }
  
  // how many internal variables ?
  unsigned int nIntVar() const {return mechanics->nIntVar();}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return mechanics->nIntVarBundled();}
  unsigned int getIntVar(const std::string& str) const {
    return mechanics->getIntVar(str);
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    return mechanics->typeIntVar(i);
  }
  unsigned int indexIntVar(unsigned int i) const {
    return mechanics->indexIntVar(i);
  }
  std::string labelIntVar(unsigned int i) const {
    return mechanics->labelIntVar(i);
  }
  
  // check if the material behaviour is linear ?
  bool isLinear() const {return false;}
  
  // initialize the state of the material
  void initState(const MaterialProperties& material,MaterialState& state) {
    ConstitutiveModel::initState(material,state);
    // mechanical part
    MaterialState mechState;
    mechanics->initState(material,mechState);
    MatLibArray grad(state.grad,TENSOR::MEMSIZE+1);
    MatLibArray flux(state.flux,TENSOR::MEMSIZE+1);
    grad = mechState.grad;
    flux = mechState.flux;
    state.internal = mechState.internal;
    // thermal part
    VECTOR g(state.grad,TENSOR::MEMSIZE+1);
    VECTOR h(state.flux,TENSOR::MEMSIZE+1);
    g = 0.e0;
    h = 0.e0;
  }
      
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& M,
                              bool update,bool tangent)
   throw (UpdateFailedException) {
        
    // initialize
    double W = 0.0e0;
    if (tangent) M = 0.0e0;
     
    // switch between adiabatic and full incremental potential
    if (extPar.find("ADIABATIC_SOURCE") != extPar.end())
      W = adiabaticIncrementalPotential(material,extPar,state0,state,dTime,M,update,tangent);
    else
      W = fullIncrementalPotential(material,extPar,state0,state,dTime,M,update,tangent);

    return W;
  }
      
  // compute the incremental potential
  double adiabaticIncrementalPotential(const MaterialProperties& material,
                                       const ParameterSet& extPar,
                                       const MaterialState& state0,MaterialState& state,
                                       double dTime,MatLibMatrix& M,
                                       bool update,bool tangent)
   throw (UpdateFailedException) {

    static const unsigned int ITMAX = 10;
    static const double PRECISION = 1.e-08;
    static const double TOLERANCE = 1.e-06;
    double W;

    // get adiabatic heat source term
    double Q0 = 0.0e0;
    if (extPar.count("ADIABATIC_SOURCE"))
      Q0 = extPar.find("ADIABATIC_SOURCE")->second;
     
    // iterate over adiabatic temperature
    if (update) {
      
      // get algorithmic parameter
      unsigned int maxIt;
      if (material.checkProperty("ADIAB_MAX_ITER_PARAMETER"))
        maxIt = material.getIntegerProperty("ADIAB_MAX_ITER_PARAMETER");
      else
        maxIt = ITMAX;
      
      unsigned int iter;
      double dN,dT,test,test0 = 1.0e0;
      for (iter=0; iter < maxIt; iter++) {
         
        // compute incremental potential
        W = mechanics->incrementalPotential(material,extPar,state0,state,
                                            dTime,M,true,true);
         
        // check net entropy increase
        dN = state.flux[TENSOR::MEMSIZE];
        test = std::fabs(dN+Q0);
        if (test > test0) test0 = test;
        if (test < TOLERANCE*test0) break;

        // compute correction to temperature
        dT = -(dN+Q0)/M[TENSOR::MEMSIZE][TENSOR::MEMSIZE];
        if (std::fabs(dT) < PRECISION) break;
         
        // apply correction and iterate
        state.grad[TENSOR::MEMSIZE] += dT;
      }
      if (iter == maxIt) {
        std::cerr << "no convergence in adiabatic update" << std::endl;
        throw UpdateFailedException("no convergence in adiabatic update");
      }
    }
     
    // compute incremental potential and derivatives
    W = mechanics->incrementalPotential(material,extPar,state0,state,
                                        dTime,M,update,tangent);
    if (tangent) {
      double coef = 1.0e0/M[TENSOR::MEMSIZE][TENSOR::MEMSIZE];
      for (unsigned int k=0; k < TENSOR::MEMSIZE; k++)
        for (unsigned int l=0; l < TENSOR::MEMSIZE; l++)
          M[k][l] -= coef*M[k][TENSOR::MEMSIZE]*M[TENSOR::MEMSIZE][l];
    }

    return W;
  }

  // compute the incremental potential
  double fullIncrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& M,
                              bool update,bool tangent) 
   throw (UpdateFailedException) {

    // thermo-mechanical incremental potential
    double W = mechanics->incrementalPotential(material,extPar,state0,state,
                                               dTime,M,update,tangent);
     
    // get mechanical tensors
    TENSOR F0(state0.grad);
    TENSOR F1(state.grad);
    TENSOR P1,P(state.flux);
    TENSOR4 K1,M11(M);

    // extract temperature and temperature gradient
    double T0 = state0.grad[TENSOR::MEMSIZE];
    double T1 = state.grad[TENSOR::MEMSIZE];
    double coef0 = T0/T1;
    double coef1 = 1.0-coef0;
    VECTOR G(state.grad,TENSOR::MEMSIZE+1);
    VECTOR H(state.flux,TENSOR::MEMSIZE+1);
    MatLibMatrix K2(M,VECTOR::MEMSIZE,TENSOR::MEMSIZE+1);
    
    // compute right Cauchy-Green tensor for the step
    SYM_TENSOR C0,C1,C;
    ALG1::RightCauchyGreen(F0,C0);
    ALG1::RightCauchyGreen(F1,C1);
    double alpha = material.getDoubleProperty("THM_ALGORITHMIC_PARAMETER");
    double coef2 = alpha*(T1-T0);
    double coef3 = alpha*dTime;
    C = (1.0-alpha)*C0+alpha*C1;

    // compute temperature for the step
    double T = (1.0-alpha)*T0+alpha*T1;

    // compute diffusion energy
    SYM_TENSOR4 Km0,Km1,K;
    double N,CT;
    SYM_TENSOR S0,S1,S,ST;
    SYM_TENSOR MGC0[VECTOR::MEMSIZE],MGC1[VECTOR::MEMSIZE];
    VECTOR H0,H1,HT;
    MatLibMatrix Kt0(VECTOR::MEMSIZE),Kt1(VECTOR::MEMSIZE);
    double X0 = conduction->diffusionEnergy(material,extPar,C,T0,G,S0,N,H0,Km0,CT,Kt0,ST,MGC0,HT,
                                            update || tangent,tangent);
    double X1 = conduction->diffusionEnergy(material,extPar,C,T,G,S1,N,H1,Km1,CT,Kt1,ST,MGC1,HT,
                                            update || tangent,tangent);

    // compute true Lagrangian quantities
    if (update || tangent) S = coef0*S0+coef1*S1;
    if (update) {
      ALG1::PK2ToPK1(S,F1,P1);
      P -= coef3*P1;
      state.flux[TENSOR::MEMSIZE] -= (coef0*(X1-X0)+coef2*N)*dTime/T1;
      H = -dTime*(coef0*H0+coef1*H1);
    }
    if (tangent) {
      double val1 = dTime/T1;
      double val2 = alpha*val1;
      K = alpha*(coef0*Km0+coef1*Km1); // meca-meca
      ALG1::MaterialToLagrangian(K,S,F1,K1);
      M11 -= coef3*K1;
      M[TENSOR::MEMSIZE][TENSOR::MEMSIZE] -= val1*(alpha*coef2*CT+2*alpha*coef0*N-2*coef0*(X1-X0)/T1); // T-T
      K2 = -dTime*(coef0*Kt0+coef1*Kt1); // G-G
      TENSOR dP,PT; // meca-T
      SYM_TENSOR dS;
      dS = S1-S0;
      ALG1::PK2ToPK1(dS,F1,dP);
      ALG1::PK2ToPK1(ST,F1,PT);
      for (unsigned int k=0; k < TENSOR::MEMSIZE; k++)
        M[TENSOR::MEMSIZE][k] = M[k][TENSOR::MEMSIZE] -= val2*(coef0*dP[k]+coef2*PT[k]);
      for (unsigned int l=0; l < VECTOR::MEMSIZE; l++) { // meca-G
        TENSOR K12;
        SYM_TENSOR M12;
        M12 = coef0*MGC0[l]+coef1*MGC1[l];
        ALG1::PK2ToPK1(M12,F1,K12);
        for (unsigned int k=0; k < TENSOR::MEMSIZE; k++)
          M[TENSOR::MEMSIZE+1+l][k] = M[k][TENSOR::MEMSIZE+1+l] = -coef3*K12[k];
      }
      for (unsigned int k=0; k < VECTOR::MEMSIZE; k++) // G-T
        M[TENSOR::MEMSIZE][TENSOR::MEMSIZE+1+k] 
        = M[TENSOR::MEMSIZE+1+k][TENSOR::MEMSIZE] = -val1*(coef0*(H1[k]-H0[k])+coef2*HT[k]);
    }

    return W-dTime*(coef0*X0+coef1*X1);
  }
};

/**
 * Base class for (non-linear) thermo-mechanical conduction potentials.
 */
template <class ALG1,class ALG2>
class CoupledStdThermoMechanics<ALG1,ALG2>::ConductionPotential {
  
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
                                 const SYM_TENSOR&,double,const VECTOR&,
                                 SYM_TENSOR&,double&,VECTOR&,SYM_TENSOR4&,
                                 double&,MatLibMatrix&,SYM_TENSOR&,SYM_TENSOR[],
                                 VECTOR&,bool,bool) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
