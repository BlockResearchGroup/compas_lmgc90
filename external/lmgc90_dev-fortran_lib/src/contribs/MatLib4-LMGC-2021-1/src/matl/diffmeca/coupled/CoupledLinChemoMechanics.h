/*
 *  $Id: CoupledLinChemoMechanics.h 239 2017-06-09 15:01:17Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFF_MECA_COUPLED_LINEAR_CHEMO_MECHANICS_H
#define ZORGLIB_MATL_DIFF_MECA_COUPLED_LINEAR_CHEMO_MECHANICS_H

// config
#include <matlib_macros.h>

// local
#include <matl/diff/linear/IsotropicLinDiffusionPotential.h>
#include <matl/diffmeca/linear/J2ChemoPlasticity.h>
#include <matl/diffmeca/linear/J2ChemoPlasticityCoupled.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for chemo-mechanical (linearized kinematics) models
 * coupled with diffusion.
 */
template <class ALG1,class ALG2>
class CoupledLinChemoMechanics : virtual public StandardMaterial {
  
 public:
  
  // define new types
  typedef typename ALG1::SymTensor::TYPE SYM_TENSOR;
  typedef typename ALG2::Vector          VECTOR;
  
  typedef typename LinVariationalDiffusion<ALG2>::DiffusionPotential DiffusionPotential;

 protected:
    
  // chemo-mechanical model
  ChemoElasticity<ALG1> *mechanics;
  
  // associated diffusion potential
  DiffusionPotential *diffusion;
  
  // instance counter
  unsigned int *count;
  
  // empty constructor
  CoupledLinChemoMechanics(ChemoElasticity<ALG1>* m = 0,
                           DiffusionPotential* k = 0) {
    count = new unsigned int(1);
    mechanics = m;
    diffusion = k;
  }

 public:

  // constructor
  CoupledLinChemoMechanics(ChemoElasticity<ALG1>& m,DiffusionPotential& k) {
    count = new unsigned int(1);
    mechanics = &m;
    diffusion = &k;
  }
  
  // copy constructor
  CoupledLinChemoMechanics(const CoupledLinChemoMechanics& src) {
    count = src.count;
    (*count)++;
    mechanics = src.mechanics;
    diffusion = src.diffusion;
  }
  
  // destructor
  virtual ~CoupledLinChemoMechanics() {
    if (--(*count) > 0) return;
    delete count;
    if (mechanics) delete mechanics;
    if (diffusion) delete diffusion;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nCoupled (linearized kinematics) chemo-mechanical material:" << std::endl;

    // check mechanics
    mechanics->checkProperties(material,os);
    
    // look for algorithmic parameter
    double alpha = 0.5;
    try {
      alpha = material.getDoubleProperty("CHM_ALGORITHMIC_PARAMETER");
    }
    catch (NoSuchPropertyException) {
      try {
        alpha = material.getDoubleProperty("DIFF_ALGORITHMIC_PARAMETER");
        material.setProperty("CHM_ALGORITHMIC_PARAMETER",alpha);
      }
      catch (NoSuchPropertyException) {
        material.setProperty("CHM_ALGORITHMIC_PARAMETER",alpha);
      }
    }
    if (os) (*os) << "\n\talgorithmic parameter = " << alpha << std::endl;
    
    // conduction part
    diffusion->checkProperties(material,os);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    mechanics->rotateProperties(material,R);
    diffusion->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    mechanics->updateProperties(mater,extPar);
    diffusion->updateProperties(mater,extPar);
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
        return "chemical potential";
        break;
      case 2:
        return "chemical potential gradient";
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
      case 2:
        return "flux";
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
    // chemical/diffusion part
    VECTOR g(state.grad,SYM_TENSOR::MEMSIZE+1);
    VECTOR h(state.flux,SYM_TENSOR::MEMSIZE+1);
    g = 0.e0;
    h = 0.e0;
  }
  
  // compute the incremental potential (standard or insulated)
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& M,
                              bool update,bool tangent) 
   throw (UpdateFailedException) {

    // initialize
    double W = 0.e0;
    if (tangent) M = 0.0e0;
     
    // switch between insulated and full incremental potential
    if (extPar.find("CHEMICAL_SOURCE") != extPar.end())
      W = insulatedIncrementalPotential(material,extPar,state0,state,dTime,M,update,tangent);
    else
      W = fullIncrementalPotential(material,extPar,state0,state,dTime,M,update,tangent);

    return W;
  }
      
  // compute the incremental potential (insulated)
  double insulatedIncrementalPotential(const MaterialProperties& material,
                                       const ParameterSet& extPar,
                                       const MaterialState& state0,MaterialState& state,
                                       double dTime,MatLibMatrix& M,
                                       bool update,bool tangent)
   throw (UpdateFailedException) {
     
    static const unsigned int ITMAX = 10;
    static const double PRECISION = 1.e-12;
    static const double TOLERANCE = 1.e-08;
    double W;

    // get chemical source term
    double Q0 = 0.0e0;
    if (extPar.count("CHEMICAL_SOURCE"))
      Q0 = extPar.find("CHEMICAL_SOURCE")->second;
     
    // iterate over concentration
    if (update) {
      unsigned int iter;
      double test0 = 1.0e0;
      for (iter=0; iter < ITMAX; iter++) {
         
        // compute incremental potential
        W = mechanics->incrementalPotential(material,extPar,state0,state,
                                            dTime,M,true,true);
         
        // check net concentration increase
        double dC = state.flux[SYM_TENSOR::MEMSIZE];
        double test = std::fabs(dC+Q0);
        if (test > test0) test0 = test;
        if (test < TOLERANCE*test0) break;
         
        // compute correction to temperature
        double dMu = -(dC+Q0)/M[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
        if (std::fabs(dMu) < PRECISION) break;
        
        // apply correction and iterate
        state.grad[SYM_TENSOR::MEMSIZE] += dMu;
      }
      if (iter == ITMAX) {
        std::cerr << "no convergence in insulated chemical update" << std::endl;
        throw UpdateFailedException("no convergence in insulated chemical update");
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

    // chemo-mechanical incremental potential
    double W = mechanics->incrementalPotential(material,extPar,state0,state,
                                               dTime,M,update,tangent);

    // extract chemical potential and its gradient
    double mu0 = state0.grad[SYM_TENSOR::MEMSIZE];
    double mu1 = state.grad[SYM_TENSOR::MEMSIZE];
    VECTOR g(state.grad,SYM_TENSOR::MEMSIZE+1);

    // compute temperature for the step
    double alpha = material.getDoubleProperty("CHM_ALGORITHMIC_PARAMETER");
    double coef = alpha*dTime;
    double mu = (1.0-alpha)*mu0+alpha*mu1;

    // compute diffusion energy
    double c0,C0;
    VECTOR j(state.flux,SYM_TENSOR::MEMSIZE+1),S0;
    typename ALG2::SymTensor K;
    double X = diffusion->diffusionEnergy(material,extPar,g,mu,j,c0,K,S0,C0,
                                          update,tangent);
    if (update) {
      state.flux[SYM_TENSOR::MEMSIZE] -= coef*c0;
      j *= -dTime;
    }
    if (tangent) {
      M[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE] -= coef*C0;
      for (unsigned int k=SYM_TENSOR::MEMSIZE+1; 
           k < SYM_TENSOR::MEMSIZE+1+VECTOR::MEMSIZE; k++) {
        double val = -coef*S0[k-SYM_TENSOR::MEMSIZE-1];
        M[SYM_TENSOR::MEMSIZE][k] = M[k][SYM_TENSOR::MEMSIZE] = val;
      }
      MatLibMatrix Mred(M,VECTOR::MEMSIZE,SYM_TENSOR::MEMSIZE+1);
      Mred = -dTime*K.toMatrix();
    }

    return W-dTime*X;
  }
};


/**
 * Coupled linear chemo-elasticity
 */
class CoupledIsotropicChemoElasticity3D
: public CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledIsotropicChemoElasticity3D()
  : CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new IsotropicChemoElasticity3D(),
                new IsotropicLinDiffusionPotential<StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledIsotropicChemoElasticity3D(const CoupledIsotropicChemoElasticity3D& src)
  : CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledIsotropicChemoElasticity3D() {}
};
class CoupledIsotropicChemoElasticity2D
: public CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledIsotropicChemoElasticity2D()
  : CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new IsotropicChemoElasticity2D(),
                new IsotropicLinDiffusionPotential<StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledIsotropicChemoElasticity2D(const CoupledIsotropicChemoElasticity2D& src)
  : CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledIsotropicChemoElasticity2D() {}
};
class CoupledIsotropicChemoElasticity1D
: public CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledIsotropicChemoElasticity1D()
  : CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new IsotropicChemoElasticity1D(),
                new IsotropicLinDiffusionPotential<StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledIsotropicChemoElasticity1D(const CoupledIsotropicChemoElasticity1D& src)
  : CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledIsotropicChemoElasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledIsotropicChemoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledIsotropicChemoElasticityBuilder();
  
  // the instance
  static CoupledIsotropicChemoElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CoupledIsotropicChemoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};
      
      
/**
 * Coupled small-strains J2 chemo-plasticity (rate-independent, weakly coupled)
 */
class CoupledLinearIsotropicJ2WkChemoPlasticity3D
: public CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {

 public:

  // constructor
  CoupledLinearIsotropicJ2WkChemoPlasticity3D()
  : CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
              new LinearIsotropicJ2WkChemoPlasticity3D(),
              new IsotropicLinDiffusionPotential<StdTensorAlgebra3D>()) {}

  // copy constructor
  CoupledLinearIsotropicJ2WkChemoPlasticity3D(const CoupledLinearIsotropicJ2WkChemoPlasticity3D& src)
  : CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}

  // destructor
  virtual ~CoupledLinearIsotropicJ2WkChemoPlasticity3D() {}
};
class CoupledLinearIsotropicJ2WkChemoPlasticity2D
: public CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {

 public:

  // constructor
  CoupledLinearIsotropicJ2WkChemoPlasticity2D()
  : CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
              new LinearIsotropicJ2WkChemoPlasticity2D(),
              new IsotropicLinDiffusionPotential<StdTensorAlgebra2D>()) {}

  // copy constructor
  CoupledLinearIsotropicJ2WkChemoPlasticity2D(const CoupledLinearIsotropicJ2WkChemoPlasticity2D& src)
  : CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}

  // destructor
  virtual ~CoupledLinearIsotropicJ2WkChemoPlasticity2D() {}
};
class CoupledLinearIsotropicJ2WkChemoPlasticity1D
: public CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {

 public:

  // constructor
  CoupledLinearIsotropicJ2WkChemoPlasticity1D()
  : CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
              new LinearIsotropicJ2WkChemoPlasticity1D(),
              new IsotropicLinDiffusionPotential<StdTensorAlgebra1D>()) {}

  // copy constructor
  CoupledLinearIsotropicJ2WkChemoPlasticity1D(const CoupledLinearIsotropicJ2WkChemoPlasticity1D& src)
  : CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}

  // destructor
  virtual ~CoupledLinearIsotropicJ2WkChemoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledLinearIsotropicJ2WkChemoPlasticityBuilder : public ModelBuilder {

 private:

  // constructor
  CoupledLinearIsotropicJ2WkChemoPlasticityBuilder();

  // the instance
  static CoupledLinearIsotropicJ2WkChemoPlasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~CoupledLinearIsotropicJ2WkChemoPlasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};
      
      
/**
 * Coupled small-strains J2 chemo-visco-plasticity (rate-dependent, weakly coupled)
 */
class CoupledLinearIsotropicJ2WkChemoViscoPlasticity3D
: public CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {

 public:

  // constructor
  CoupledLinearIsotropicJ2WkChemoViscoPlasticity3D()
  : CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
              new LinearIsotropicJ2WkChemoViscoPlasticity3D(),
              new IsotropicLinDiffusionPotential<StdTensorAlgebra3D>()) {}

  // copy constructor
  CoupledLinearIsotropicJ2WkChemoViscoPlasticity3D(const CoupledLinearIsotropicJ2WkChemoViscoPlasticity3D& src)
  : CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}

  // destructor
  virtual ~CoupledLinearIsotropicJ2WkChemoViscoPlasticity3D() {}
};
class CoupledLinearIsotropicJ2WkChemoViscoPlasticity2D
: public CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {

 public:

  // constructor
  CoupledLinearIsotropicJ2WkChemoViscoPlasticity2D()
  : CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
              new LinearIsotropicJ2WkChemoViscoPlasticity2D(),
              new IsotropicLinDiffusionPotential<StdTensorAlgebra2D>()) {}

  // copy constructor
  CoupledLinearIsotropicJ2WkChemoViscoPlasticity2D(const CoupledLinearIsotropicJ2WkChemoViscoPlasticity2D& src)
  : CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}

  // destructor
  virtual ~CoupledLinearIsotropicJ2WkChemoViscoPlasticity2D() {}
};
class CoupledLinearIsotropicJ2WkChemoViscoPlasticity1D
: public CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {

 public:

  // constructor
  CoupledLinearIsotropicJ2WkChemoViscoPlasticity1D()
  : CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
              new LinearIsotropicJ2WkChemoViscoPlasticity1D(),
              new IsotropicLinDiffusionPotential<StdTensorAlgebra1D>()) {}

  // copy constructor
  CoupledLinearIsotropicJ2WkChemoViscoPlasticity1D(const CoupledLinearIsotropicJ2WkChemoViscoPlasticity1D& src)
  : CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}

  // destructor
  virtual ~CoupledLinearIsotropicJ2WkChemoViscoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledLinearIsotropicJ2WkChemoViscoPlasticityBuilder : public ModelBuilder {

 private:

  // constructor
  CoupledLinearIsotropicJ2WkChemoViscoPlasticityBuilder();

  // the instance
  static CoupledLinearIsotropicJ2WkChemoViscoPlasticityBuilder const* BUILDER;

public:

  // destructor
  virtual ~CoupledLinearIsotropicJ2WkChemoViscoPlasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Coupled small-strains J2 chemo-plasticity (rate-independent)
 */
class CoupledLinearIsotropicJ2ChemoPlasticity3D
: public CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledLinearIsotropicJ2ChemoPlasticity3D()
  : CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new LinearIsotropicJ2ChemoPlasticity3D(),
                new IsotropicLinDiffusionPotential<StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledLinearIsotropicJ2ChemoPlasticity3D(const CoupledLinearIsotropicJ2ChemoPlasticity3D& src)
  : CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledLinearIsotropicJ2ChemoPlasticity3D() {}
};
class CoupledLinearIsotropicJ2ChemoPlasticity2D
: public CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledLinearIsotropicJ2ChemoPlasticity2D()
  : CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new LinearIsotropicJ2ChemoPlasticity2D(),
                new IsotropicLinDiffusionPotential<StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledLinearIsotropicJ2ChemoPlasticity2D(const CoupledLinearIsotropicJ2ChemoPlasticity2D& src)
  : CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledLinearIsotropicJ2ChemoPlasticity2D() {}
};
class CoupledLinearIsotropicJ2ChemoPlasticity1D
: public CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledLinearIsotropicJ2ChemoPlasticity1D()
  : CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new LinearIsotropicJ2ChemoPlasticity1D(),
                new IsotropicLinDiffusionPotential<StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledLinearIsotropicJ2ChemoPlasticity1D(const CoupledLinearIsotropicJ2ChemoPlasticity1D& src)
  : CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledLinearIsotropicJ2ChemoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledLinearIsotropicJ2ChemoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledLinearIsotropicJ2ChemoPlasticityBuilder();
  
  // the instance
  static CoupledLinearIsotropicJ2ChemoPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CoupledLinearIsotropicJ2ChemoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Coupled small-strains J2 chemo-visco-plasticity (rate-dependent)
 */
class CoupledLinearIsotropicJ2ChemoViscoPlasticity3D
: public CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {

 public:

  // constructor
  CoupledLinearIsotropicJ2ChemoViscoPlasticity3D()
  : CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new LinearIsotropicJ2ChemoViscoPlasticity3D(),
                new IsotropicLinDiffusionPotential<StdTensorAlgebra3D>()) {}

  // copy constructor
  CoupledLinearIsotropicJ2ChemoViscoPlasticity3D(const CoupledLinearIsotropicJ2ChemoViscoPlasticity3D& src)
  : CoupledLinChemoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}

  // destructor
  virtual ~CoupledLinearIsotropicJ2ChemoViscoPlasticity3D() {}
};
class CoupledLinearIsotropicJ2ChemoViscoPlasticity2D
: public CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {

 public:

  // constructor
  CoupledLinearIsotropicJ2ChemoViscoPlasticity2D()
  : CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new LinearIsotropicJ2ChemoViscoPlasticity2D(),
                new IsotropicLinDiffusionPotential<StdTensorAlgebra2D>()) {}

  // copy constructor
  CoupledLinearIsotropicJ2ChemoViscoPlasticity2D(const CoupledLinearIsotropicJ2ChemoViscoPlasticity2D& src)
  : CoupledLinChemoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}

  // destructor
  virtual ~CoupledLinearIsotropicJ2ChemoViscoPlasticity2D() {}
};
class CoupledLinearIsotropicJ2ChemoViscoPlasticity1D
: public CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {

 public:

  // constructor
  CoupledLinearIsotropicJ2ChemoViscoPlasticity1D()
  : CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new LinearIsotropicJ2ChemoViscoPlasticity1D(),
                new IsotropicLinDiffusionPotential<StdTensorAlgebra1D>()) {}

  // copy constructor
  CoupledLinearIsotropicJ2ChemoViscoPlasticity1D(const CoupledLinearIsotropicJ2ChemoViscoPlasticity1D& src)
  : CoupledLinChemoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}

  // destructor
  virtual ~CoupledLinearIsotropicJ2ChemoViscoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledLinearIsotropicJ2ChemoViscoPlasticityBuilder : public ModelBuilder {

 private:

  // constructor
  CoupledLinearIsotropicJ2ChemoViscoPlasticityBuilder();

  // the instance
  static CoupledLinearIsotropicJ2ChemoViscoPlasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~CoupledLinearIsotropicJ2ChemoViscoPlasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};


#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
