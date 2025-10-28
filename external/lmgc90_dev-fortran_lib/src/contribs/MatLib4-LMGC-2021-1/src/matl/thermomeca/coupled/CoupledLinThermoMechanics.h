/*
 *  $Id: CoupledLinThermoMechanics.h 237 2017-06-06 09:13:56Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_COUPLED_LINEAR_THERMO_MECHANICS_H
#define ZORGLIB_MATL_MECA_COUPLED_LINEAR_THERMO_MECHANICS_H

// config
#include <matlib_macros.h>

// local
#include <matl/thermo/linear/IsotropicLinConductionPotential.h>
#include <matl/thermomeca/linear/IsotropicThermalViscousPotential.h>
#include <matl/thermomeca/linear/J2ThermoPlasticitySimple.h>
#include <matl/thermomeca/linear/ThermoElasticSMAModel1.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for linearized thermo-mechanical models
 * coupled with conduction.
 */
template <class ALG1,class ALG2>
class CoupledLinThermoMechanics : virtual public StandardMaterial {
  
 public:
  
  // define new types
  typedef typename ALG1::SymTensor::TYPE SYM_TENSOR;
  typedef typename ALG2::Vector          VECTOR;
  
  typedef typename LinVariationalConduction<ALG2>::ConductionPotential ConductionPotential;

 protected:
    
  // thermo-mechanical model
  ThermoElasticity<ALG1> *mechanics;
  
  // associated conduction potential
  ConductionPotential *conduction;
  
  // instance counter
  unsigned int *count;
  
  // empty constructor
  CoupledLinThermoMechanics(ThermoElasticity<ALG1>* m = 0,
                            ConductionPotential* k = 0) {
    count = new unsigned int(1);
    mechanics  = m;
    conduction = k;
  }

 public:

  // constructor
  CoupledLinThermoMechanics(ThermoElasticity<ALG1>& m,ConductionPotential& k) {
    count = new unsigned int(1);
    mechanics  = &m;
    conduction = &k;
  }
  
  // copy constructor
  CoupledLinThermoMechanics(const CoupledLinThermoMechanics& src) {
    count = src.count;
    (*count)++;
    mechanics  = src.mechanics;
    conduction = src.conduction;
  }
  
  // destructor
  virtual ~CoupledLinThermoMechanics() {
    if (--(*count) > 0) return;
    delete count;
    if (mechanics) delete mechanics;
    if (conduction) delete conduction;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nCoupled linear thermo-mechanical material:" << std::endl;

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
  unsigned int nExtVar() const {return SYM_TENSOR::MEMSIZE+1+VECTOR::MEMSIZE;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 3;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_SYM_TENSOR;
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
        return SYM_TENSOR::MEMSIZE;
        break;
      case 2:
        return SYM_TENSOR::MEMSIZE+1;
        break;
      default:
        return SYM_TENSOR::MEMSIZE+1+VECTOR::MEMSIZE;
        break;
    }
  }
  std::string labelExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "deformation";
        break;
      case 1:
        return "temperature increment";
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
  bool isLinear() const {return mechanics->isLinear();}
  
  // initialize the state of the material
  void initState(const MaterialProperties& material,MaterialState& state) {
    ConstitutiveModel::initState(material,state);
    // mechanical part
    MaterialState mechState;
    mechanics->initState(material,mechState);
    MatLibArray grad(state.grad,SYM_TENSOR::MEMSIZE+1);
    MatLibArray flux(state.flux,SYM_TENSOR::MEMSIZE+1);
    grad = mechState.grad;
    flux = mechState.flux;
    state.internal = mechState.internal;
    // thermal part
    VECTOR g(state.grad,SYM_TENSOR::MEMSIZE+1);
    VECTOR h(state.flux,SYM_TENSOR::MEMSIZE+1);
    g = 0.e0;
    h = 0.e0;
  }
  
  // compute the incremental potential (standard or adiabatic)
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& M,
                              bool update,bool tangent) 
   throw (UpdateFailedException) {

    // initialize
    double W = 0.e0;
    if (tangent) M = 0.0e0;
     
    // switch between adiabatic and full incremental potential
    if (extPar.find("ADIABATIC_SOURCE") != extPar.end())
      W = adiabaticIncrementalPotential(material,extPar,state0,state,dTime,M,update,tangent);
    else
      W = fullIncrementalPotential(material,extPar,state0,state,dTime,M,update,tangent);

    return W;
  }
      
  // compute the incremental potential (adiabatic)
  double adiabaticIncrementalPotential(const MaterialProperties& material,
                                       const ParameterSet& extPar,
                                       const MaterialState& state0,MaterialState& state,
                                       double dTime,MatLibMatrix& M,
                                       bool update,bool tangent)
   throw (UpdateFailedException) {
     
    static const unsigned int ITMAX = 10;
    static const double PRECISION = 1.e-12;
    static const double TOLERANCE = 1.e-08;
    double W;

    // get adiabatic entropy source term
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
      double test0 = 1.0e0;
      for (iter=0; iter < maxIt; iter++) {
         
        // compute incremental potential
        W = mechanics->incrementalPotential(material,extPar,state0,state,
                                            dTime,M,true,true);
         
        // check net entropy increase
        double dN = state.flux[SYM_TENSOR::MEMSIZE];
        double test = std::fabs(dN+Q0);
        if (test > test0) test0 = test;
        if (test < TOLERANCE*test0) break;
         
        // compute correction to temperature
        double dT = -(dN+Q0)/M[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
        if (std::fabs(dT) < PRECISION) break;
        
        // apply correction and iterate
        state.grad[SYM_TENSOR::MEMSIZE] += dT;
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
      double coef = 1.0e0/M[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
      for (unsigned int k=0; k < SYM_TENSOR::MEMSIZE; k++)
        for (unsigned int l=0; l < SYM_TENSOR::MEMSIZE; l++)
           M[k][l] -= coef*M[k][SYM_TENSOR::MEMSIZE]*M[SYM_TENSOR::MEMSIZE][l];
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

    // extract temperature and temperature gradient
    double Th0 = state0.grad[SYM_TENSOR::MEMSIZE];
    double Th1 = state.grad[SYM_TENSOR::MEMSIZE];
    VECTOR g(state.grad,SYM_TENSOR::MEMSIZE+1);

    // compute temperature for the step
    double alpha = material.getDoubleProperty("THM_ALGORITHMIC_PARAMETER");
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
    VECTOR h0,h1,h(state.flux,SYM_TENSOR::MEMSIZE+1),S0;
    typename ALG2::SymTensor K0,K1;
    double X0 = conduction->diffusionEnergy(material,extPar,g,Th0,h0,N0,K0,S0,C0,
                                            update,tangent);
    double X1 = conduction->diffusionEnergy(material,extPar,g,Th,h1,N0,K1,S0,C0,
                                            update,tangent);
    if (update) {
      state.flux[SYM_TENSOR::MEMSIZE] -= dTime*(coef2*N0+coef0*(X1-X0))/T1;
      h = -dTime*(coef0*h0+coef1*h1);
    }
    if (tangent) {
      double val = dTime/T1;
      M[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE] -= val*(alpha*coef2*C0+2*alpha*coef0*N0-2*coef0*(X1-X0)/T1);
      for (unsigned int k=0; k < VECTOR::MEMSIZE; k++) {
        M[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE+1+k]
        = M[SYM_TENSOR::MEMSIZE+1+k][SYM_TENSOR::MEMSIZE] = -val*(coef2*S0[k]+coef0*(h1[k]-h0[k]));
      }
      MatLibMatrix Mred(M,VECTOR::MEMSIZE,SYM_TENSOR::MEMSIZE+1);
      Mred = -dTime*(coef0*K0.toMatrix()+coef1*K1.toMatrix());
    }

    return W-dTime*(coef0*X0+coef1*X1);
  }
};


/**
 * Coupled linear thermo-elasticity
 */
class CoupledIsotropicThermoElasticity3D
: public CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledIsotropicThermoElasticity3D()
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new IsotropicThermoElasticity3D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledIsotropicThermoElasticity3D(const CoupledIsotropicThermoElasticity3D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledIsotropicThermoElasticity3D() {}
};
class CoupledIsotropicThermoElasticity2D 
: public CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledIsotropicThermoElasticity2D()
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new IsotropicThermoElasticity2D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledIsotropicThermoElasticity2D(const CoupledIsotropicThermoElasticity2D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledIsotropicThermoElasticity2D() {}
};
class CoupledIsotropicThermoElasticity1D 
: public CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledIsotropicThermoElasticity1D()
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new IsotropicThermoElasticity1D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledIsotropicThermoElasticity1D(const CoupledIsotropicThermoElasticity1D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledIsotropicThermoElasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledIsotropicThermoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledIsotropicThermoElasticityBuilder();
  
  // the instance
  static CoupledIsotropicThermoElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CoupledIsotropicThermoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Coupled small-strains J2 thermo-plasticity (linear isotropic hardening)
 */
class CoupledLinearIsotropicJ2ThermoPlasticity3D
: public CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledLinearIsotropicJ2ThermoPlasticity3D()
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new LinearIsotropicJ2ThermoPlasticity3D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledLinearIsotropicJ2ThermoPlasticity3D(const CoupledLinearIsotropicJ2ThermoPlasticity3D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledLinearIsotropicJ2ThermoPlasticity3D() {}
};
class CoupledLinearIsotropicJ2ThermoPlasticity2D 
: public CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledLinearIsotropicJ2ThermoPlasticity2D()
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new LinearIsotropicJ2ThermoPlasticity2D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledLinearIsotropicJ2ThermoPlasticity2D(const CoupledLinearIsotropicJ2ThermoPlasticity2D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledLinearIsotropicJ2ThermoPlasticity2D() {}
};
class CoupledLinearIsotropicJ2ThermoPlasticity1D 
: public CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledLinearIsotropicJ2ThermoPlasticity1D()
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new LinearIsotropicJ2ThermoPlasticity1D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledLinearIsotropicJ2ThermoPlasticity1D(const CoupledLinearIsotropicJ2ThermoPlasticity1D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledLinearIsotropicJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledLinearIsotropicJ2ThermoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledLinearIsotropicJ2ThermoPlasticityBuilder();
  
  // the instance
  static CoupledLinearIsotropicJ2ThermoPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CoupledLinearIsotropicJ2ThermoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Coupled small-strains J2 thermo-plasticity (nonlinear isotropic hardening)
 */
class CoupledNonLinearIsotropicJ2ThermoPlasticity3D
: public CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledNonLinearIsotropicJ2ThermoPlasticity3D()
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new NonLinearIsotropicJ2ThermoPlasticity3D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledNonLinearIsotropicJ2ThermoPlasticity3D(const CoupledNonLinearIsotropicJ2ThermoPlasticity3D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledNonLinearIsotropicJ2ThermoPlasticity3D() {}
};
class CoupledNonLinearIsotropicJ2ThermoPlasticity2D 
: public CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledNonLinearIsotropicJ2ThermoPlasticity2D()
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new NonLinearIsotropicJ2ThermoPlasticity2D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledNonLinearIsotropicJ2ThermoPlasticity2D(const CoupledNonLinearIsotropicJ2ThermoPlasticity2D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledNonLinearIsotropicJ2ThermoPlasticity2D() {}
};
class CoupledNonLinearIsotropicJ2ThermoPlasticity1D 
: public CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledNonLinearIsotropicJ2ThermoPlasticity1D()
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new NonLinearIsotropicJ2ThermoPlasticity1D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledNonLinearIsotropicJ2ThermoPlasticity1D(const CoupledNonLinearIsotropicJ2ThermoPlasticity1D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledNonLinearIsotropicJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledNonLinearIsotropicJ2ThermoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledNonLinearIsotropicJ2ThermoPlasticityBuilder();
  
  // the instance
  static CoupledNonLinearIsotropicJ2ThermoPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CoupledNonLinearIsotropicJ2ThermoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};
      
      
/**
 * Coupled small-strains J2 thermo-plasticity (nonlinear isotropic hardening + asinh rate-dependency)
 */
class CoupledNonLinearASinhIsotropicJ2ThermoPlasticity3D
: public CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {

 public:

  // constructor
  CoupledNonLinearASinhIsotropicJ2ThermoPlasticity3D()
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new NonLinearASinhIsotropicJ2ThermoPlasticity3D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra3D>()) {}

  // copy constructor
  CoupledNonLinearASinhIsotropicJ2ThermoPlasticity3D(const CoupledNonLinearASinhIsotropicJ2ThermoPlasticity3D& src)
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}

  // destructor
  virtual ~CoupledNonLinearASinhIsotropicJ2ThermoPlasticity3D() {}
};
class CoupledNonLinearASinhIsotropicJ2ThermoPlasticity2D
: public CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {

 public:

  // constructor
  CoupledNonLinearASinhIsotropicJ2ThermoPlasticity2D()
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new NonLinearASinhIsotropicJ2ThermoPlasticity2D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra2D>()) {}

  // copy constructor
  CoupledNonLinearASinhIsotropicJ2ThermoPlasticity2D(const CoupledNonLinearASinhIsotropicJ2ThermoPlasticity2D& src)
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}

  // destructor
  virtual ~CoupledNonLinearASinhIsotropicJ2ThermoPlasticity2D() {}
};
class CoupledNonLinearASinhIsotropicJ2ThermoPlasticity1D
: public CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {

 public:

  // constructor
  CoupledNonLinearASinhIsotropicJ2ThermoPlasticity1D()
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new NonLinearASinhIsotropicJ2ThermoPlasticity1D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra1D>()) {}

  // copy constructor
  CoupledNonLinearASinhIsotropicJ2ThermoPlasticity1D(const CoupledNonLinearASinhIsotropicJ2ThermoPlasticity1D& src)
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}

  // destructor
  virtual ~CoupledNonLinearASinhIsotropicJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledNonLinearASinhIsotropicJ2ThermoPlasticityBuilder : public ModelBuilder {

 private:

  // constructor
  CoupledNonLinearASinhIsotropicJ2ThermoPlasticityBuilder();

  // the instance
  static CoupledNonLinearASinhIsotropicJ2ThermoPlasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~CoupledNonLinearASinhIsotropicJ2ThermoPlasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Coupled small-strains J2 thermo-plasticity (Norton-Hoff isotropic hardening / rate-dependency)
 */
class CoupledNortonHoffIsotropicJ2ThermoPlasticity3D
: public CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledNortonHoffIsotropicJ2ThermoPlasticity3D()
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new NortonHoffIsotropicJ2ThermoPlasticity3D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledNortonHoffIsotropicJ2ThermoPlasticity3D(const CoupledNortonHoffIsotropicJ2ThermoPlasticity3D& src)
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledNortonHoffIsotropicJ2ThermoPlasticity3D() {}
};
class CoupledNortonHoffIsotropicJ2ThermoPlasticity2D
: public CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledNortonHoffIsotropicJ2ThermoPlasticity2D()
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new NortonHoffIsotropicJ2ThermoPlasticity2D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledNortonHoffIsotropicJ2ThermoPlasticity2D(const CoupledNortonHoffIsotropicJ2ThermoPlasticity2D& src)
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledNortonHoffIsotropicJ2ThermoPlasticity2D() {}
};
class CoupledNortonHoffIsotropicJ2ThermoPlasticity1D
: public CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledNortonHoffIsotropicJ2ThermoPlasticity1D()
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new NortonHoffIsotropicJ2ThermoPlasticity1D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledNortonHoffIsotropicJ2ThermoPlasticity1D(const CoupledNortonHoffIsotropicJ2ThermoPlasticity1D& src)
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledNortonHoffIsotropicJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledNortonHoffIsotropicJ2ThermoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledNortonHoffIsotropicJ2ThermoPlasticityBuilder();
  
  // the instance
  static CoupledNortonHoffIsotropicJ2ThermoPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CoupledNortonHoffIsotropicJ2ThermoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Coupled linear thermo-visco-elasticity (Kelvin-Voigt)
 */
class CoupledIsotropicKelvinThermoViscoElasticity3D
: public CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledIsotropicKelvinThermoViscoElasticity3D()
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new IsotropicKelvinThermoViscoElasticity3D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledIsotropicKelvinThermoViscoElasticity3D(const CoupledIsotropicKelvinThermoViscoElasticity3D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledIsotropicKelvinThermoViscoElasticity3D() {}
};
class CoupledIsotropicKelvinThermoViscoElasticity2D 
: public CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledIsotropicKelvinThermoViscoElasticity2D()
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new IsotropicKelvinThermoViscoElasticity2D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledIsotropicKelvinThermoViscoElasticity2D(const CoupledIsotropicKelvinThermoViscoElasticity2D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledIsotropicKelvinThermoViscoElasticity2D() {}
};
class CoupledIsotropicKelvinThermoViscoElasticity1D 
: public CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledIsotropicKelvinThermoViscoElasticity1D()
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new IsotropicKelvinThermoViscoElasticity1D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledIsotropicKelvinThermoViscoElasticity1D(const CoupledIsotropicKelvinThermoViscoElasticity1D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledIsotropicKelvinThermoViscoElasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledIsotropicKelvinThermoViscoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledIsotropicKelvinThermoViscoElasticityBuilder();
  
  // the instance
  static CoupledIsotropicKelvinThermoViscoElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CoupledIsotropicKelvinThermoViscoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Coupled linear thermo-elastic SMA
 */
class CoupledThermoElasticSMAModel1_3D
: public CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledThermoElasticSMAModel1_3D()
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new ThermoElasticSMAModel1_3D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledThermoElasticSMAModel1_3D(const CoupledThermoElasticSMAModel1_3D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledThermoElasticSMAModel1_3D() {}
};
class CoupledThermoElasticSMAModel1_2D 
: public CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledThermoElasticSMAModel1_2D()
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new ThermoElasticSMAModel1_2D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledThermoElasticSMAModel1_2D(const CoupledThermoElasticSMAModel1_2D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledThermoElasticSMAModel1_2D() {}
};
class CoupledThermoElasticSMAModel1_1D 
: public CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledThermoElasticSMAModel1_1D()
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new ThermoElasticSMAModel1_1D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledThermoElasticSMAModel1_1D(const CoupledThermoElasticSMAModel1_1D& src) 
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledThermoElasticSMAModel1_1D() {}
};

/**
 * The associated model builder
 */
class CoupledThermoElasticSMAModel1Builder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledThermoElasticSMAModel1Builder();
  
  // the instance
  static CoupledThermoElasticSMAModel1Builder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CoupledThermoElasticSMAModel1Builder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
