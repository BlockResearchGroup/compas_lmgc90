/*
 *  $Id: AdiabaticStdThermoMechanics.h 221 2016-10-28 12:21:26Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_ADIABATIC_STANDARD_THERMO_MECHANICS_H
#define ZORGLIB_MATL_MECA_ADIABATIC_STANDARD_THERMO_MECHANICS_H

// config
#include <matlib_macros.h>

// local
#include <matl/thermomeca/hyper/J2ThermoHEPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for finite strains thermo-mechanical models
 * under adiabatic assumption.
 */
template <class ALG>
class AdiabaticStdThermoMechanics : virtual public StandardMaterial {
  
 public:
  
  // define new types
  typedef typename ALG::Tensor::TYPE TENSOR;

 protected:
    
  // thermo-mechanical model
  ThermoHyperElasticity<ALG> *mechanics;
  
  // instance counter
  unsigned int *count;
  
  // work variables (NOT THREAD-SAFE!)
  MaterialState localState0;
  MaterialState localState;
  MatLibMatrix MLoc;

  // empty constructor
  AdiabaticStdThermoMechanics(ThermoHyperElasticity<ALG>* m = 0) {
    count = new unsigned int(1);
    mechanics  = m;
  }
  
 public:
    
  // constructor
  AdiabaticStdThermoMechanics(ThermoHyperElasticity<ALG>& m) {
    count = new unsigned int(1);
    mechanics  = &m;
  }
  
  // copy constructor
  AdiabaticStdThermoMechanics(const AdiabaticStdThermoMechanics& src) {
    count = src.count;
    (*count)++;
    mechanics  = src.mechanics;
  }
  
  // destructor
  virtual ~AdiabaticStdThermoMechanics() {
    if (--(*count) > 0) return;
    delete count;
    if (mechanics) delete mechanics;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nAdiabatic nonlinear thermo-mechanical material:" << std::endl;

    // check mechanics
    mechanics->checkProperties(material,os);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    mechanics->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    mechanics->updateProperties(mater,extPar);
  }
  
  // how many external variables ?
  unsigned int nExtVar() const {return TENSOR::MEMSIZE;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 1;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_TENSOR;
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
        return TENSOR::MEMSIZE;
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
  unsigned int nIntVar() const {return mechanics->nIntVar()+1;}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return mechanics->nIntVarBundled()+1;}
  unsigned int getIntVar(const std::string& str) const {
    if (str == "TEMP")
      return mechanics->nIntVarBundled();
    else {
      unsigned int n = mechanics->getIntVar(str);
      if (n == mechanics->nIntVarBundled())
        return n+1;
      else
        return n;
    }
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    if (i < mechanics->nIntVarBundled())
      return mechanics->typeIntVar(i);
    else if (i == mechanics->nIntVarBundled())
      return ConstitutiveModel::TYPE_SCALAR;
    else
      return ConstitutiveModel::TYPE_NONE;
  }
  unsigned int indexIntVar(unsigned int i) const {
    if (i < mechanics->nIntVarBundled())
      return mechanics->indexIntVar(i);
    else if (i == mechanics->nIntVarBundled())
      return mechanics->nIntVar();
    else
      return mechanics->nIntVar()+1;
  }
  std::string labelIntVar(unsigned int i) const {
    if (i < mechanics->nIntVarBundled())
      return mechanics->labelIntVar(i);
    else if (i == mechanics->nIntVarBundled())
      return "temperature";
    else
      return "";
  }
  
  // check if the material behaviour is linear ?
  bool isLinear() const {return mechanics->isLinear();}
  
  // initialize the state of the material
  void initState(const MaterialProperties& material,MaterialState& state) {
    ConstitutiveModel::initState(material,state);
    // non-adiabatic state
    mechanics->initState(material,localState0);
    mechanics->initState(material,localState);
    MLoc.resize(mechanics->nExtVar());
    // get mechanical part
    MatLibArray grad(localState.grad,TENSOR::MEMSIZE);
    MatLibArray flux(localState.flux,TENSOR::MEMSIZE);
    state.grad = grad;
    state.flux = flux;
    // enrich internal state with temperature
    MatLibArray intVar(state.internal,mechanics->nIntVar());
    intVar = localState.internal;
    state.internal[mechanics->nIntVar()] = localState.grad[TENSOR::MEMSIZE];
  }
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& M,
                              bool update,bool tangent) 
   throw (UpdateFailedException) {

    static const unsigned int ITMAX = 10;
    static const double PRECISION = 1.e-08;
    static const double TOLERANCE = 1.e-06;
    double W;

    // initialize thermomechanical states
    for (unsigned int k=0; k < TENSOR::MEMSIZE; k++) {
      localState0.grad[k] = state0.grad[k];
      localState0.flux[k] = state0.flux[k];
      localState.grad[k] = state.grad[k];
    }
    for (unsigned int k=0; k < mechanics->nIntVar(); k++) {
      localState0.internal[k] = state0.internal[k];
    }
    localState0.grad[TENSOR::MEMSIZE] = state0.internal[mechanics->nIntVar()];
    localState0.flux[TENSOR::MEMSIZE] = 0.0e0;
    localState.grad[TENSOR::MEMSIZE]  = state0.internal[mechanics->nIntVar()];

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
        W = mechanics->incrementalPotential(material,extPar,localState0,
                                            localState,dTime,MLoc,true,true);

        // check net entropy increase
        dN = localState.flux[TENSOR::MEMSIZE];
        test = std::fabs(dN);
        if (test > test0) test0 = test;
        //std::cout << iter << "-T=" << localState.grad[TENSOR::MEMSIZE] << "-dN=" << dN << std::endl;
        if (test < TOLERANCE*test0) break;

        // compute correction to temperature
        dT = -dN/MLoc[TENSOR::MEMSIZE][TENSOR::MEMSIZE];
        //std::cout << "dT=" << dT << "-C=" << MLoc[TENSOR::MEMSIZE][TENSOR::MEMSIZE] << std::endl;
        if (std::fabs(dT) < PRECISION) break;

        // apply correction and iterate
        localState.grad[TENSOR::MEMSIZE] += dT;
      }
      if (iter == maxIt) {
        std::cerr << "no convergence in adiabatic update" << std::endl;
        /*localState.grad[TENSOR::MEMSIZE] -= dT;
        std::cout << localState.grad[TENSOR::MEMSIZE] << "," << W << "," << localState.flux[TENSOR::MEMSIZE];
        std::cout << "," << MLoc[TENSOR::MEMSIZE][TENSOR::MEMSIZE] << std::endl;
        for (iter=0; iter < 10; iter++) {
          localState.grad[TENSOR::MEMSIZE] += 0.1*dT;
          W = mechanics->incrementalPotential(material,extPar,localState0,
                                              localState,dTime,MLoc,true,true);
          std::cout << localState.grad[TENSOR::MEMSIZE] << "," << W << "," << localState.flux[TENSOR::MEMSIZE];
          std::cout << "," << MLoc[TENSOR::MEMSIZE][TENSOR::MEMSIZE] << std::endl;
        }*/
        throw UpdateFailedException("no convergence in adiabatic update");
      }
    }

    // compute incremental potential and derivatives
    W = mechanics->incrementalPotential(material,extPar,localState0,localState,
                                        dTime,MLoc,update,tangent);
    if (update) {
      for (unsigned int k=0; k < TENSOR::MEMSIZE; k++)
        state.flux[k] = localState.flux[k];
      for (unsigned int k=0; k < mechanics->nIntVar(); k++)
        state.internal[k] = localState.internal[k];
      state.internal[mechanics->nIntVar()] = localState.grad[TENSOR::MEMSIZE];
    }
    if (tangent) {
      double coef = 1.0e0/MLoc[TENSOR::MEMSIZE][TENSOR::MEMSIZE];
      for (unsigned int k=0; k < TENSOR::MEMSIZE; k++)
        for (unsigned int l=0; l < TENSOR::MEMSIZE; l++)
          M[k][l] = MLoc[k][l]
	                 -coef*MLoc[k][TENSOR::MEMSIZE]*MLoc[TENSOR::MEMSIZE][l];
    }

    return W;
  }
};


/**
 * Adiabatic thermo-hyperelasticity
 */
class AdiabaticIsotropicThermoHyperElasticity3D
: public AdiabaticStdThermoMechanics<TensorAlgebra3D> {
  
 public:
  
  // constructor
  AdiabaticIsotropicThermoHyperElasticity3D()
  : AdiabaticStdThermoMechanics<TensorAlgebra3D>(new IsotropicThermoHyperElasticity3D()) {}
  
  // copy constructor
  AdiabaticIsotropicThermoHyperElasticity3D(const AdiabaticIsotropicThermoHyperElasticity3D& src) 
  : AdiabaticStdThermoMechanics<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~AdiabaticIsotropicThermoHyperElasticity3D() {}
};
class AdiabaticIsotropicThermoHyperElasticity2D 
: public AdiabaticStdThermoMechanics<TensorAlgebra2D> {
  
 public:
  
  // constructor
  AdiabaticIsotropicThermoHyperElasticity2D()
  : AdiabaticStdThermoMechanics<TensorAlgebra2D>(new IsotropicThermoHyperElasticity2D()) {}
  
  // copy constructor
  AdiabaticIsotropicThermoHyperElasticity2D(const AdiabaticIsotropicThermoHyperElasticity2D& src) 
  : AdiabaticStdThermoMechanics<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~AdiabaticIsotropicThermoHyperElasticity2D() {}
};
class AdiabaticIsotropicThermoHyperElasticity1D 
: public AdiabaticStdThermoMechanics<TensorAlgebra1D> {
  
 public:
  
  // constructor
  AdiabaticIsotropicThermoHyperElasticity1D()
  : AdiabaticStdThermoMechanics<TensorAlgebra1D>(new IsotropicThermoHyperElasticity1D()) {}
  
  // copy constructor
  AdiabaticIsotropicThermoHyperElasticity1D(const AdiabaticIsotropicThermoHyperElasticity1D& src) 
  : AdiabaticStdThermoMechanics<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~AdiabaticIsotropicThermoHyperElasticity1D() {}
};

/**
 * The associated model builder
 */
class AdiabaticIsotropicThermoHyperElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  AdiabaticIsotropicThermoHyperElasticityBuilder();
  
  // the instance
  static AdiabaticIsotropicThermoHyperElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~AdiabaticIsotropicThermoHyperElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Adiabatic finite strains J2 thermo-plasticity (linear isotropic hardening)
 */
class AdiabaticLinearIsotropicJ2ThermoHEPlasticity3D
: public AdiabaticStdThermoMechanics<TensorAlgebra3D> {
  
 public:
  
  // constructor
  AdiabaticLinearIsotropicJ2ThermoHEPlasticity3D()
  : AdiabaticStdThermoMechanics<TensorAlgebra3D>(new LinearIsotropicJ2ThermoHEPlasticity3D()) {}
  
  // copy constructor
  AdiabaticLinearIsotropicJ2ThermoHEPlasticity3D(const AdiabaticLinearIsotropicJ2ThermoHEPlasticity3D& src) 
  : AdiabaticStdThermoMechanics<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~AdiabaticLinearIsotropicJ2ThermoHEPlasticity3D() {}
};
class AdiabaticLinearIsotropicJ2ThermoHEPlasticity2D 
: public AdiabaticStdThermoMechanics<TensorAlgebra2D> {
  
 public:
  
  // constructor
  AdiabaticLinearIsotropicJ2ThermoHEPlasticity2D()
  : AdiabaticStdThermoMechanics<TensorAlgebra2D>(new LinearIsotropicJ2ThermoHEPlasticity2D()) {}
  
  // copy constructor
  AdiabaticLinearIsotropicJ2ThermoHEPlasticity2D(const AdiabaticLinearIsotropicJ2ThermoHEPlasticity2D& src) 
  : AdiabaticStdThermoMechanics<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~AdiabaticLinearIsotropicJ2ThermoHEPlasticity2D() {}
};
class AdiabaticLinearIsotropicJ2ThermoHEPlasticity1D 
: public AdiabaticStdThermoMechanics<TensorAlgebra1D> {
  
 public:
  
  // constructor
  AdiabaticLinearIsotropicJ2ThermoHEPlasticity1D()
  : AdiabaticStdThermoMechanics<TensorAlgebra1D>(new LinearIsotropicJ2ThermoHEPlasticity1D()) {}
  
  // copy constructor
  AdiabaticLinearIsotropicJ2ThermoHEPlasticity1D(const AdiabaticLinearIsotropicJ2ThermoHEPlasticity1D& src) 
  : AdiabaticStdThermoMechanics<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~AdiabaticLinearIsotropicJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class AdiabaticLinearIsotropicJ2ThermoHEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  AdiabaticLinearIsotropicJ2ThermoHEPlasticityBuilder();
  
  // the instance
  static AdiabaticLinearIsotropicJ2ThermoHEPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~AdiabaticLinearIsotropicJ2ThermoHEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Adiabatic finite strains J2 thermo-plasticity (nonlinear isotropic hardening)
 */
class AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity3D
: public AdiabaticStdThermoMechanics<TensorAlgebra3D> {
  
 public:
  
  // constructor
  AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity3D()
  : AdiabaticStdThermoMechanics<TensorAlgebra3D>(new NonLinearIsotropicJ2ThermoHEPlasticity3D()) {}
  
  // copy constructor
  AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity3D(const AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity3D& src) 
  : AdiabaticStdThermoMechanics<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity3D() {}
};
class AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity2D 
: public AdiabaticStdThermoMechanics<TensorAlgebra2D> {
  
 public:
  
  // constructor
  AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity2D()
  : AdiabaticStdThermoMechanics<TensorAlgebra2D>(new NonLinearIsotropicJ2ThermoHEPlasticity2D()) {}
  
  // copy constructor
  AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity2D(const AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity2D& src) 
  : AdiabaticStdThermoMechanics<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity2D() {}
};
class AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity1D 
: public AdiabaticStdThermoMechanics<TensorAlgebra1D> {
  
 public:
  
  // constructor
  AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity1D()
  : AdiabaticStdThermoMechanics<TensorAlgebra1D>(new NonLinearIsotropicJ2ThermoHEPlasticity1D()) {}
  
  // copy constructor
  AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity1D(const AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity1D& src) 
  : AdiabaticStdThermoMechanics<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~AdiabaticNonLinearIsotropicJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class AdiabaticNonLinearIsotropicJ2ThermoHEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  AdiabaticNonLinearIsotropicJ2ThermoHEPlasticityBuilder();
  
  // the instance
  static AdiabaticNonLinearIsotropicJ2ThermoHEPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~AdiabaticNonLinearIsotropicJ2ThermoHEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Adiabatic finite strains J2 thermo-plasticity (nonlinear isotropic hardening + asinh rate-dependency)
 */
class AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity3D
: public AdiabaticStdThermoMechanics<TensorAlgebra3D> {
  
 public:
  
  // constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity3D()
  : AdiabaticStdThermoMechanics<TensorAlgebra3D>(new NonLinearASinhIsotropicJ2ThermoHEPlasticity3D()) {}
  
  // copy constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity3D(const AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity3D& src) 
  : AdiabaticStdThermoMechanics<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity3D() {}
};
class AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity2D 
: public AdiabaticStdThermoMechanics<TensorAlgebra2D> {
  
 public:
  
  // constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity2D()
  : AdiabaticStdThermoMechanics<TensorAlgebra2D>(new NonLinearASinhIsotropicJ2ThermoHEPlasticity2D()) {}
  
  // copy constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity2D(const AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity2D& src) 
  : AdiabaticStdThermoMechanics<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity2D() {}
};
class AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity1D 
: public AdiabaticStdThermoMechanics<TensorAlgebra1D> {
  
 public:
  
  // constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity1D()
  : AdiabaticStdThermoMechanics<TensorAlgebra1D>(new NonLinearASinhIsotropicJ2ThermoHEPlasticity1D()) {}
  
  // copy constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity1D(const AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity1D& src) 
  : AdiabaticStdThermoMechanics<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder();
  
  // the instance
  static AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~AdiabaticNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Adiabatic finite strains J2 thermo-plasticity (Norton-Hoff isotropic hardening)
 */
class AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity3D
: public AdiabaticStdThermoMechanics<TensorAlgebra3D> {
  
 public:
  
  // constructor
  AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity3D()
  : AdiabaticStdThermoMechanics<TensorAlgebra3D>(new NortonHoffIsotropicJ2ThermoHEPlasticity3D()) {}
  
  // copy constructor
  AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity3D(const AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity3D& src)
  : AdiabaticStdThermoMechanics<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity3D() {}
};
class AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity2D
: public AdiabaticStdThermoMechanics<TensorAlgebra2D> {
  
 public:
  
  // constructor
  AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity2D()
  : AdiabaticStdThermoMechanics<TensorAlgebra2D>(new NortonHoffIsotropicJ2ThermoHEPlasticity2D()) {}
  
  // copy constructor
  AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity2D(const AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity2D& src)
  : AdiabaticStdThermoMechanics<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity2D() {}
};
class AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity1D
: public AdiabaticStdThermoMechanics<TensorAlgebra1D> {
  
 public:
  
  // constructor
  AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity1D()
  : AdiabaticStdThermoMechanics<TensorAlgebra1D>(new NortonHoffIsotropicJ2ThermoHEPlasticity1D()) {}
  
  // copy constructor
  AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity1D(const AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity1D& src)
  : AdiabaticStdThermoMechanics<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticityBuilder();
  
  // the instance
  static AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~AdiabaticNortonHoffIsotropicJ2ThermoHEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

