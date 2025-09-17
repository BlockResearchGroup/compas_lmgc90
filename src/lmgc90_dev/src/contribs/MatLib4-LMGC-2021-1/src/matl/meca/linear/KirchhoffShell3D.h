/*
 *  $Id: KirchhoffShell3D.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_KIRCHHOFF_SHELL_3D_H
#define ZORGLIB_MATL_MECA_LINEAR_KIRCHHOFF_SHELL_3D_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/meca/linear/Elasticity.h>
#include <matl/meca/linear/IsotropicElasticPotential.h>
#include <matl/meca/linear/OrthotropicElasticPotential.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Resultant-based shell constitutive model (3D).
 * Kirchhoff hypothesis: no transverse shear.
 * For homogeneous linear elastic materials only.
 */
class KirchhoffShell3D : virtual public StandardMaterial {

 public:

  typedef Elasticity<TensorAlgebra3D>::Potential Potential;
  typedef StdTensorAlgebra2D::SymTensor SYM_TENSOR;

 protected:
  
  // associated elasticity
  Elasticity<TensorAlgebra3D>::Potential *potential;
  
  // compute membrane stored energy
  double membraneEnergy(const MatLibMatrix&,double,const SYM_TENSOR&,SYM_TENSOR&,
                        MatLibMatrix&,bool,bool);
  
  // compute bending stored energy
  double bendingEnergy(const MatLibMatrix&,double,const SYM_TENSOR&,SYM_TENSOR&,
                       MatLibMatrix&,bool,bool);
  
  // instance counter
  unsigned int *count;
  
  // empty constructor
  KirchhoffShell3D(Potential *p = 0) {
    count = new unsigned int(1);
    potential = p;
  }

 public:

  // constructor
  KirchhoffShell3D(Potential &p) {
    count = new unsigned int(1);
    potential = &p;
  }
  
  // copy constructor
  KirchhoffShell3D(const KirchhoffShell3D& src) {
    count = src.count;
    (*count)++;
    potential = src.potential;
  }
  
  // destructor
  virtual ~KirchhoffShell3D() {
    if (--(*count) > 0) return;
    delete count;
    if (potential) delete potential;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& mater,const Rotation& R) {
    if (potential) potential->rotateProperties(mater,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    if (potential) potential->updateProperties(mater,extPar);
  }
  
  // how many external variables ?
  unsigned int nExtVar() const {return 2*SYM_TENSOR::MEMSIZE;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 2;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int) const;
  unsigned int indexExtVar(unsigned int) const;
  std::string labelExtVar(unsigned int) const;
  std::string labelExtForce(unsigned int) const;
  
  // how many internal variables ?
  unsigned int nIntVar() const {return 2;}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 2;}
  unsigned int getIntVar(const std::string&) const;
  ConstitutiveModel::VariableType typeIntVar(unsigned int) const;
  unsigned int indexIntVar(unsigned int) const;
  std::string labelIntVar(unsigned int) const;
  
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
  double incrementalPotential(const MaterialProperties&,const ParameterSet&,
                              const MaterialState&,MaterialState&,double,
                              MatLibMatrix&,bool,bool) 
    throw (UpdateFailedException);
};


/**
 * Implementations of the model.
 */
class IsotropicKirchhoffShell3D : public KirchhoffShell3D {
  
 public:
  
  // constructor
  IsotropicKirchhoffShell3D()
  : KirchhoffShell3D(new IsotropicElasticPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicKirchhoffShell3D(const IsotropicKirchhoffShell3D& src) 
  : KirchhoffShell3D(src) {}
  
  // destructor
  virtual ~IsotropicKirchhoffShell3D() {}
};

class OrthotropicKirchhoffShell3D : public KirchhoffShell3D {
  
public:
  
  // constructor
  OrthotropicKirchhoffShell3D()
  : KirchhoffShell3D(new OrthotropicElasticPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  OrthotropicKirchhoffShell3D(const OrthotropicKirchhoffShell3D& src) 
  : KirchhoffShell3D(src) {}
  
  // destructor
  virtual ~OrthotropicKirchhoffShell3D() {}
};

/**
 * The associated model builders
 */
class IsotropicKirchhoffShellBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicKirchhoffShellBuilder();
  
  // the instance
  static IsotropicKirchhoffShellBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~IsotropicKirchhoffShellBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

class OrthotropicKirchhoffShellBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  OrthotropicKirchhoffShellBuilder();
  
  // the instance
  static OrthotropicKirchhoffShellBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~OrthotropicKirchhoffShellBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
