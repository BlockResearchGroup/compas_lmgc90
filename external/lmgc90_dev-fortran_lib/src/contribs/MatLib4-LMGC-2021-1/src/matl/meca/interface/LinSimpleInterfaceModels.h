/*
 *  $Id: LinSimpleInterfaceModels.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_INTERFACE_LINEAR_SIMPLE_MODELS_H
#define ZORGLIB_MATL_MECA_INTERFACE_LINEAR_SIMPLE_MODELS_H

// config
#include <matlib_macros.h>

// std C++ library
#include <stdexcept>
// local
#include <matl/meca/linear/IsotropicElasticPotential.h>
#include <matl/meca/linear/J2PlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing interface models based on general (linearized kinematics) 3D models.
 */
template <class ALG>
class LinSimpleInterfaceModel : virtual public StandardMaterial {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor SYM_TENSOR;
  typedef typename ALG::Vector    VECTOR;
  
 protected:
  
  // associated mechanical model
  StandardMaterial *model;
  
  // instance counter
  unsigned int *count;
  
  // work variables (NOT THREAD-SAFE!)
  MaterialState localState0;
  MaterialState localState;
  MatLibMatrix MLoc;
  
  // empty constructor
  LinSimpleInterfaceModel(StandardMaterial* m = 0) {
    count = new unsigned int(1);
    model = m;
  }

 public:
  
  // constructor
  LinSimpleInterfaceModel(StandardMaterial& m) {
    count = new unsigned int(1);
    model = &m;
  }
  
  // copy constructor
  LinSimpleInterfaceModel(const LinSimpleInterfaceModel& src) {
    count = src.count;
    (*count)++;
    model = src.model;
  }
  
  // destructor
  virtual ~LinSimpleInterfaceModel() {
    if (--(*count) > 0) return;
    delete count;
    if (model) delete model;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nSimple interface model (linearized kinematics):" << std::endl;
 
    // check base model
    if (model->nExtVarBundled() != 1 || model->typeExtVar(0) != TYPE_SYM_TENSOR) {
      if (os) (*os) << "ERROR: associated model is not in linearized kinematics." << std::endl;
      throw std::runtime_error("associated model");
    }
    model->checkProperties(material,os);

    // interface thickness
    double h = material.getDoubleProperty("INTERFACE_THICKNESS");
    if (h <= 0.e0) {
      if (os) (*os) << "ERROR: interface thickness must be strictly positive." << std::endl;
      throw InvalidPropertyException("interface thickness");
    }
    if (os) {
      if (os) (*os) << "\n\tinterface thickness = " << h << std::endl;      
    }
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    model->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    model->updateProperties(mater,extPar);
  }
  
  // how many external variables ?
  unsigned int nExtVar() const {return VECTOR::MEMSIZE;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 1;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int i) const {
    switch (i) {
      case 0:
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
      default:
        return VECTOR::MEMSIZE;
        break;
    }
  }
  std::string labelExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "displacement jump";
        break;
      default:
        return "";
        break;
    }
  }
  std::string labelExtForce(unsigned int i) const {
    switch (i) {
      case 0:
        return "traction vector";
        break;
      default:
        return "";
        break;
    }
  }
  
  // how many internal variables ?
  unsigned int nIntVar() const {return model->nIntVar();}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return model->nIntVarBundled();}
  unsigned int getIntVar(const std::string& str) const {
    return model->nIntVarBundled();
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    return model->typeIntVar(i);
  }
  unsigned int indexIntVar(unsigned int i) const {
    return model->indexIntVar(i);
  }
  std::string labelIntVar(unsigned int i) const {
    return model->labelIntVar(i);
  }
  
  // check if the material behaviour is linear ?
  bool isLinear() const {return model->isLinear();}
  
  // initialize the state of the material
  void initState(const MaterialProperties& material,MaterialState& state) {
    ConstitutiveModel::initState(material,state);
    // initialize local 3D state
    model->initState(material,localState0);
    model->initState(material,localState);
    MLoc.resize(model->nExtVar());
    // initialize material state
    state.grad = 0.0e0;
    state.flux = 0.0e0;
    state.internal = localState0.internal;
  }
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& M,
                              bool update,bool tangent) 
   throw (UpdateFailedException) {

    // extract normal and tangent displacement jumps
    VECTOR dU0(state0.grad);
    VECTOR dU(state.grad);

    // get interface thickness
    double h = material.getDoubleProperty("INTERFACE_THICKNESS");
    double coef = 1.0/h;

    // normal vector
    VECTOR N;
    for (unsigned int i=0; i < ALG::DIMENSION-1; i++) N[i] = 0.0e0;
    N[ALG::DIMENSION-1] = 1.0e0;

    // compute strain tensors
    SYM_TENSOR eps0(localState0.grad);
    SYM_TENSOR eps(localState.grad);
    eps0 = coef*covariant(symTensorProd(dU0,N));
    eps  = coef*covariant(symTensorProd(dU,N));

    // initialize internal variables
    localState0.internal = state0.internal;
    if (!update) localState.internal  = state.internal;

    // update material state
    double W = model->incrementalPotential(material,extPar,localState0,localState,
                                           dTime,MLoc,update,tangent);

    // update forces and internal variables
    if (update) {
      
      // get stress tensor
      SYM_TENSOR sig(localState.flux);

      // compute local traction vector
      VECTOR T(state.flux);
      for (unsigned int i=0; i < VECTOR::MEMSIZE; i++)
        T[i] = sig[SYM_TENSOR::MAP[i][ALG::DIMENSION-1]];

      // internal variables
      state.internal = localState.internal;
    }

    // compute tangent operator
    if (tangent) {

      // project 3D tangent onto normal direction
      for (unsigned int i=0; i < VECTOR::MEMSIZE; i++)
        for (unsigned int j=0; j < VECTOR::MEMSIZE; j++)
          M[i][j] = coef*MLoc[SYM_TENSOR::MAP[i][ALG::DIMENSION-1]][SYM_TENSOR::MAP[j][ALG::DIMENSION-1]];
    }

    return h*W;
  }
};


/**
 * Isotropic elastic interface
 */
class IsotropicElasticInterface3D
: public LinSimpleInterfaceModel<StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicElasticInterface3D()
  : LinSimpleInterfaceModel<StdTensorAlgebra3D>(new IsotropicElasticity3D()) {}
  
  // copy constructor
  IsotropicElasticInterface3D(const IsotropicElasticInterface3D& src) 
  : LinSimpleInterfaceModel<StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicElasticInterface3D() {}
};
class IsotropicElasticInterface2D 
: public LinSimpleInterfaceModel<StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicElasticInterface2D()
  : LinSimpleInterfaceModel<StdTensorAlgebra2D>(new IsotropicElasticity2D()) {}
  
  // copy constructor
  IsotropicElasticInterface2D(const IsotropicElasticInterface2D& src) 
  : LinSimpleInterfaceModel<StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicElasticInterface2D() {}
};

/**
 * The associated model builder
 */
class IsotropicElasticInterfaceBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicElasticInterfaceBuilder();
  
  // the instance
  static IsotropicElasticInterfaceBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~IsotropicElasticInterfaceBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Linear elasto-plastic interface (linear isotropic hardening)
 */
class LinearIsotropicPlasticInterface3D
: public LinSimpleInterfaceModel<StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  LinearIsotropicPlasticInterface3D()
  : LinSimpleInterfaceModel<StdTensorAlgebra3D>(new LinearIsotropicJ2Plasticity3D()) {}
  
  // copy constructor
  LinearIsotropicPlasticInterface3D(const LinearIsotropicPlasticInterface3D& src) 
  : LinSimpleInterfaceModel<StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicPlasticInterface3D() {}
};
class LinearIsotropicPlasticInterface2D 
: public LinSimpleInterfaceModel<StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  LinearIsotropicPlasticInterface2D()
  : LinSimpleInterfaceModel<StdTensorAlgebra2D>(new LinearIsotropicJ2Plasticity2D()) {}
  
  // copy constructor
  LinearIsotropicPlasticInterface2D(const LinearIsotropicPlasticInterface2D& src) 
  : LinSimpleInterfaceModel<StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicPlasticInterface2D() {}
};

/**
 * The associated model builder
 */
class LinearIsotropicPlasticInterfaceBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  LinearIsotropicPlasticInterfaceBuilder();
  
  // the instance
  static LinearIsotropicPlasticInterfaceBuilder const* BUILDER;
  
public:
  
  // destructor
  virtual ~LinearIsotropicPlasticInterfaceBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
