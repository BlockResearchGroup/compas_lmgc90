/*
 *  $Id: AdiabaticLinThermoMechanics.h 221 2016-10-28 12:21:26Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_ADIABATIC_LINEAR_THERMO_MECHANICS_H
#define ZORGLIB_MATL_MECA_ADIABATIC_LINEAR_THERMO_MECHANICS_H

// config
#include <matlib_macros.h>

// local
#include <matl/thermomeca/linear/IsotropicThermalViscousPotential.h>
#include <matl/thermomeca/linear/J2ThermoPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for small strains thermo-mechanical models
 * under adiabatic assumption.
 */
template <class ALG>
class AdiabaticLinThermoMechanics : virtual public StandardMaterial {

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE SYM_TENSOR;

 protected:

  // thermo-mechanical model
  ThermoElasticity<ALG> *mechanics;
  
  // instance counter
  unsigned int *count;
  
  // work variables (NOT THREAD_SAFE!)
  MaterialState localState0;
  MaterialState localState;
  MatLibMatrix MLoc;
  
  // empty constructor
  AdiabaticLinThermoMechanics(ThermoElasticity<ALG>* m = 0) {
    count = new unsigned int(1);
    mechanics  = m;
  }

 public:

  // constructor
  AdiabaticLinThermoMechanics(ThermoElasticity<ALG>& m) {
    count = new unsigned int(1);
    mechanics  = &m;
  }
  
  // copy constructor
  AdiabaticLinThermoMechanics(const AdiabaticLinThermoMechanics& src) {
    count = src.count;
    (*count)++;
    mechanics  = src.mechanics;
  }
  
  // destructor
  virtual ~AdiabaticLinThermoMechanics() {
    if (--(*count) > 0) return;
    delete count;
    if (mechanics) delete mechanics;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nAdiabatic linear thermo-mechanical material:" << std::endl;
    
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
    MLoc.resize(SYM_TENSOR::MEMSIZE+1);
    // get mechanical part
    MatLibArray grad(localState.grad,SYM_TENSOR::MEMSIZE);
    MatLibArray flux(localState.flux,SYM_TENSOR::MEMSIZE);
    state.grad = grad;
    state.flux = flux;
    // enrich internal state with temperature
    MatLibArray intVar(state.internal,mechanics->nIntVar());
    intVar = localState.internal;
    state.internal[mechanics->nIntVar()] = localState.grad[SYM_TENSOR::MEMSIZE];
  }
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& M,
                              bool update,bool tangent) 
   throw (UpdateFailedException) {
    
    static const unsigned int ITMAX = 10;
    static const double PRECISION = 1.e-12;
    static const double TOLERANCE = 1.e-08;
    double W;
    
    // initialize thermomechanical states
    for (unsigned int k=0; k < SYM_TENSOR::MEMSIZE; k++) {
      localState0.grad[k] = state0.grad[k];
      localState0.flux[k] = state0.flux[k];
      localState.grad[k] = state.grad[k];
    }
    for (unsigned int k=0; k < mechanics->nIntVar(); k++) {
      localState0.internal[k] = state0.internal[k];
    }
    localState0.grad[SYM_TENSOR::MEMSIZE] = state0.internal[mechanics->nIntVar()];
    localState0.flux[SYM_TENSOR::MEMSIZE] = 0.0e0;
    localState.grad[SYM_TENSOR::MEMSIZE]  = state0.internal[mechanics->nIntVar()];
    
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
        W = mechanics->incrementalPotential(material,extPar,localState0,
                                            localState,dTime,MLoc,true,true);
        
        // check net entropy increase
        double dN = localState.flux[SYM_TENSOR::MEMSIZE];
        double test = std::fabs(dN);
        if (test > test0) test0 = test;
        if (test < TOLERANCE*test0) break;
        
        // compute correction to temperature
        double dT;
        double C = MLoc[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
        /*if (C > std::numeric_limits<double>::max()) {
          if (dN > 0.0e0)
            dT = -TOLERANCE;
          else
            dT = +TOLERANCE;
        }
        else if (C < -std::numeric_limits<double>::max()) {
          if (dN > 0.0e0)
            dT = +TOLERANCE;
          else
            dT = -TOLERANCE;
        }
        else*/
          dT = -dN/C;
        if (std::fabs(dT) < PRECISION) break;
        
        // apply correction and iterate
        localState.grad[SYM_TENSOR::MEMSIZE] += dT;
        //std::cout << iter << "-T=" << localState.grad[SYM_TENSOR::MEMSIZE] << "-dN=" << dN << std::endl;
      }
      if (iter == maxIt) {
        std::cerr << "no convergence in adiabatic update" << std::endl;
        throw UpdateFailedException("no convergence in adiabatic update");
      }
    }
    
    // compute incremental potential and derivatives
    W = mechanics->incrementalPotential(material,extPar,localState0,localState,
                                        dTime,MLoc,update,tangent);
    if (update) {
      for (unsigned int k=0; k < SYM_TENSOR::MEMSIZE; k++)
        state.flux[k] = localState.flux[k];
      for (unsigned int k=0; k < mechanics->nIntVar(); k++)
        state.internal[k] = localState.internal[k];
      state.internal[mechanics->nIntVar()] = localState.grad[SYM_TENSOR::MEMSIZE];
    }
    if (tangent) {
      double coef = 1.0e0/MLoc[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
      for (unsigned int k=0; k < SYM_TENSOR::MEMSIZE; k++)
        for (unsigned int l=0; l < SYM_TENSOR::MEMSIZE; l++)
          M[k][l] = MLoc[k][l]
                   -coef*MLoc[k][SYM_TENSOR::MEMSIZE]*MLoc[SYM_TENSOR::MEMSIZE][l];
    }
    
    return W;
  }
};


/**
 * Adiabatic thermo-elasticity
 */
class AdiabaticIsotropicThermoElasticity3D
: public AdiabaticLinThermoMechanics<TensorAlgebra3D> {
  
 public:
  
  // constructor
  AdiabaticIsotropicThermoElasticity3D()
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(new IsotropicThermoElasticity3D()) {}
  
  // copy constructor
  AdiabaticIsotropicThermoElasticity3D(const AdiabaticIsotropicThermoElasticity3D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~AdiabaticIsotropicThermoElasticity3D() {}
};
class AdiabaticIsotropicThermoElasticity2D 
: public AdiabaticLinThermoMechanics<TensorAlgebra2D> {
  
 public:
  
  // constructor
  AdiabaticIsotropicThermoElasticity2D()
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(new IsotropicThermoElasticity2D()) {}
  
  // copy constructor
  AdiabaticIsotropicThermoElasticity2D(const AdiabaticIsotropicThermoElasticity2D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~AdiabaticIsotropicThermoElasticity2D() {}
};
class AdiabaticIsotropicThermoElasticity1D 
: public AdiabaticLinThermoMechanics<TensorAlgebra1D> {
  
 public:
  
  // constructor
  AdiabaticIsotropicThermoElasticity1D()
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(new IsotropicThermoElasticity1D()) {}
  
  // copy constructor
  AdiabaticIsotropicThermoElasticity1D(const AdiabaticIsotropicThermoElasticity1D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~AdiabaticIsotropicThermoElasticity1D() {}
};

/**
 * The associated model builder
 */
class AdiabaticIsotropicThermoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  AdiabaticIsotropicThermoElasticityBuilder();
  
  // the instance
  static AdiabaticIsotropicThermoElasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~AdiabaticIsotropicThermoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Adiabatic small strains J2 thermo-plasticity (linear isotropic hardening)
 */
class AdiabaticLinearIsotropicJ2ThermoPlasticity3D
: public AdiabaticLinThermoMechanics<TensorAlgebra3D> {
  
 public:
  
  // constructor
  AdiabaticLinearIsotropicJ2ThermoPlasticity3D()
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(new LinearIsotropicJ2ThermoPlasticity3D()) {}
  
  // copy constructor
  AdiabaticLinearIsotropicJ2ThermoPlasticity3D(const AdiabaticLinearIsotropicJ2ThermoPlasticity3D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~AdiabaticLinearIsotropicJ2ThermoPlasticity3D() {}
};
class AdiabaticLinearIsotropicJ2ThermoPlasticity2D 
: public AdiabaticLinThermoMechanics<TensorAlgebra2D> {
  
 public:
  
  // constructor
  AdiabaticLinearIsotropicJ2ThermoPlasticity2D()
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(new LinearIsotropicJ2ThermoPlasticity2D()) {}
  
  // copy constructor
  AdiabaticLinearIsotropicJ2ThermoPlasticity2D(const AdiabaticLinearIsotropicJ2ThermoPlasticity2D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~AdiabaticLinearIsotropicJ2ThermoPlasticity2D() {}
};
class AdiabaticLinearIsotropicJ2ThermoPlasticity1D 
: public AdiabaticLinThermoMechanics<TensorAlgebra1D> {
  
 public:
  
  // constructor
  AdiabaticLinearIsotropicJ2ThermoPlasticity1D()
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(new LinearIsotropicJ2ThermoPlasticity1D()) {}
  
  // copy constructor
  AdiabaticLinearIsotropicJ2ThermoPlasticity1D(const AdiabaticLinearIsotropicJ2ThermoPlasticity1D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~AdiabaticLinearIsotropicJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class AdiabaticLinearIsotropicJ2ThermoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  AdiabaticLinearIsotropicJ2ThermoPlasticityBuilder();
  
  // the instance
  static AdiabaticLinearIsotropicJ2ThermoPlasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~AdiabaticLinearIsotropicJ2ThermoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Adiabatic small strains J2 thermo-plasticity (nonlinear isotropic hardening)
 */
class AdiabaticNonLinearIsotropicJ2ThermoPlasticity3D
: public AdiabaticLinThermoMechanics<TensorAlgebra3D> {
  
 public:
  
  // constructor
  AdiabaticNonLinearIsotropicJ2ThermoPlasticity3D()
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(new NonLinearIsotropicJ2ThermoPlasticity3D()) {}
  
  // copy constructor
  AdiabaticNonLinearIsotropicJ2ThermoPlasticity3D(const AdiabaticNonLinearIsotropicJ2ThermoPlasticity3D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~AdiabaticNonLinearIsotropicJ2ThermoPlasticity3D() {}
};
class AdiabaticNonLinearIsotropicJ2ThermoPlasticity2D 
: public AdiabaticLinThermoMechanics<TensorAlgebra2D> {
  
 public:
  
  // constructor
  AdiabaticNonLinearIsotropicJ2ThermoPlasticity2D()
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(new NonLinearIsotropicJ2ThermoPlasticity2D()) {}
  
  // copy constructor
  AdiabaticNonLinearIsotropicJ2ThermoPlasticity2D(const AdiabaticNonLinearIsotropicJ2ThermoPlasticity2D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~AdiabaticNonLinearIsotropicJ2ThermoPlasticity2D() {}
};
class AdiabaticNonLinearIsotropicJ2ThermoPlasticity1D 
: public AdiabaticLinThermoMechanics<TensorAlgebra1D> {
  
 public:
  
  // constructor
  AdiabaticNonLinearIsotropicJ2ThermoPlasticity1D()
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(new NonLinearIsotropicJ2ThermoPlasticity1D()) {}
  
  // copy constructor
  AdiabaticNonLinearIsotropicJ2ThermoPlasticity1D(const AdiabaticNonLinearIsotropicJ2ThermoPlasticity1D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~AdiabaticNonLinearIsotropicJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class AdiabaticNonLinearIsotropicJ2ThermoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  AdiabaticNonLinearIsotropicJ2ThermoPlasticityBuilder();
  
  // the instance
  static AdiabaticNonLinearIsotropicJ2ThermoPlasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~AdiabaticNonLinearIsotropicJ2ThermoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Adiabatic small strains J2 thermo-plasticity (nonlinear isotropic hardening + asinh rate-dependency)
 */
class AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity3D
: public AdiabaticLinThermoMechanics<TensorAlgebra3D> {
  
 public:
  
  // constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity3D()
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(new NonLinearASinhIsotropicJ2ThermoPlasticity3D()) {}
  
  // copy constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity3D(const AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity3D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity3D() {}
};
class AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity2D 
: public AdiabaticLinThermoMechanics<TensorAlgebra2D> {
  
 public:
  
  // constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity2D()
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(new NonLinearASinhIsotropicJ2ThermoPlasticity2D()) {}
  
  // copy constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity2D(const AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity2D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity2D() {}
};
class AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity1D 
: public AdiabaticLinThermoMechanics<TensorAlgebra1D> {
  
 public:
  
  // constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity1D()
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(new NonLinearASinhIsotropicJ2ThermoPlasticity1D()) {}
  
  // copy constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity1D(const AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity1D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticityBuilder();
  
  // the instance
  static AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~AdiabaticNonLinearASinhIsotropicJ2ThermoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Adiabatic small strains J2 thermo-plasticity (Norton-Hoff isotropic hardening / rate-dependency)
 */
class AdiabaticNortonHoffIsotropicJ2ThermoPlasticity3D
: public AdiabaticLinThermoMechanics<TensorAlgebra3D> {

 public:

  // constructor
  AdiabaticNortonHoffIsotropicJ2ThermoPlasticity3D()
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(new NortonHoffIsotropicJ2ThermoPlasticity3D()) {}

  // copy constructor
  AdiabaticNortonHoffIsotropicJ2ThermoPlasticity3D(const AdiabaticNortonHoffIsotropicJ2ThermoPlasticity3D& src)
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(src) {}

  // destructor
  virtual ~AdiabaticNortonHoffIsotropicJ2ThermoPlasticity3D() {}
};
class AdiabaticNortonHoffIsotropicJ2ThermoPlasticity2D
: public AdiabaticLinThermoMechanics<TensorAlgebra2D> {

 public:

  // constructor
  AdiabaticNortonHoffIsotropicJ2ThermoPlasticity2D()
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(new NortonHoffIsotropicJ2ThermoPlasticity2D()) {}

  // copy constructor
  AdiabaticNortonHoffIsotropicJ2ThermoPlasticity2D(const AdiabaticNortonHoffIsotropicJ2ThermoPlasticity2D& src)
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(src) {}

  // destructor
  virtual ~AdiabaticNortonHoffIsotropicJ2ThermoPlasticity2D() {}
};
class AdiabaticNortonHoffIsotropicJ2ThermoPlasticity1D
: public AdiabaticLinThermoMechanics<TensorAlgebra1D> {

 public:

  // constructor
  AdiabaticNortonHoffIsotropicJ2ThermoPlasticity1D()
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(new NortonHoffIsotropicJ2ThermoPlasticity1D()) {}

  // copy constructor
  AdiabaticNortonHoffIsotropicJ2ThermoPlasticity1D(const AdiabaticNortonHoffIsotropicJ2ThermoPlasticity1D& src)
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(src) {}

  // destructor
  virtual ~AdiabaticNortonHoffIsotropicJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class AdiabaticNortonHoffIsotropicJ2ThermoPlasticityBuilder : public ModelBuilder {

 private:

  // constructor
  AdiabaticNortonHoffIsotropicJ2ThermoPlasticityBuilder();

  // the instance
  static AdiabaticNortonHoffIsotropicJ2ThermoPlasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~AdiabaticNortonHoffIsotropicJ2ThermoPlasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Adiabatic thermo-visco-elasticity (Kelvin-Voigt)
 */
class AdiabaticIsotropicKelvinThermoViscoElasticity3D
: public AdiabaticLinThermoMechanics<TensorAlgebra3D> {
  
 public:
  
  // constructor
  AdiabaticIsotropicKelvinThermoViscoElasticity3D()
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(new IsotropicKelvinThermoViscoElasticity3D()) {}
  
  // copy constructor
  AdiabaticIsotropicKelvinThermoViscoElasticity3D(const AdiabaticIsotropicKelvinThermoViscoElasticity3D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~AdiabaticIsotropicKelvinThermoViscoElasticity3D() {}
};
class AdiabaticIsotropicKelvinThermoViscoElasticity2D 
: public AdiabaticLinThermoMechanics<TensorAlgebra2D> {
  
 public:
  
  // constructor
  AdiabaticIsotropicKelvinThermoViscoElasticity2D()
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(new IsotropicKelvinThermoViscoElasticity2D()) {}
  
  // copy constructor
  AdiabaticIsotropicKelvinThermoViscoElasticity2D(const AdiabaticIsotropicKelvinThermoViscoElasticity2D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~AdiabaticIsotropicKelvinThermoViscoElasticity2D() {}
};
class AdiabaticIsotropicKelvinThermoViscoElasticity1D 
: public AdiabaticLinThermoMechanics<TensorAlgebra1D> {
  
 public:
  
  // constructor
  AdiabaticIsotropicKelvinThermoViscoElasticity1D()
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(new IsotropicKelvinThermoViscoElasticity1D()) {}
  
  // copy constructor
  AdiabaticIsotropicKelvinThermoViscoElasticity1D(const AdiabaticIsotropicKelvinThermoViscoElasticity1D& src) 
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~AdiabaticIsotropicKelvinThermoViscoElasticity1D() {}
};

/**
 * The associated model builder
 */
class AdiabaticIsotropicKelvinThermoViscoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  AdiabaticIsotropicKelvinThermoViscoElasticityBuilder();
  
  // the instance
  static AdiabaticIsotropicKelvinThermoViscoElasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~AdiabaticIsotropicKelvinThermoViscoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
