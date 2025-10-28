/*
 *  $Id: TaylorModelFCC.h 196 2016-02-18 14:44:05Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_CRYSTAL_TAYLOR_MODEL_FCC_H
#define ZORGLIB_MATL_MECA_CRYSTAL_TAYLOR_MODEL_FCC_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/meca/crystal/CuitinoFCC.h> /* can be changed when specific rate-dependency model is implemented */
#include <matl/meca/crystal/SingleCrystalFCC.h>
#include <matl/meca/hyper/CrystalHEPlasticity.h>
#include <matl/meca/hyper/GeneralHenckyPotential.h>
#include <matl/meca/hyper/PolyCrystalHEModel.h>
#include <matl/meca/linear/HardeningModels.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing the FCC crystal plasticity with a Taylor hardening model.
 */
class FCCHardeningTaylor : public CrystalHardeningModel {
  
 protected:
  
  // hardening model
  IsotropicHardeningModel *hardening;
  
  // instance counter
  unsigned int *count;
  
  // private constructor
  FCCHardeningTaylor(IsotropicHardeningModel*);

 public:
  
  // constructor
  FCCHardeningTaylor(IsotropicHardeningModel&);

  // copy constructor
  FCCHardeningTaylor(const FCCHardeningTaylor&);

  // destructor
  virtual ~FCCHardeningTaylor();
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of internal parameters
  unsigned int nIntPar() const {return hardening->nIntPar();}
  
  // plastically stored energy
  double storedEnergy(const MaterialProperties&,const ParameterSet&,
                      const MatLibArray&,MatLibArray&,double,
                      const MatLibArray&,const MatLibArray&,
                      MatLibArray&,MatLibMatrix&,bool,bool);
  
  // yield stress
  void yieldStress(const MaterialProperties&,const ParameterSet&,
                   const MatLibArray&,MatLibArray&,
                   const MatLibArray&,const MatLibArray&,
                   const MatLibArray&,MatLibArray&,MatLibMatrix&,
                   std::vector<MatLibMatrix>&,bool,bool);
};  


/**
 * Implementations of the model: linear hardening.
 */
class LinearCrystalHEPlasticityFCC3D : public CrystalHEPlasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  LinearCrystalHEPlasticityFCC3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
        new GeneralHenckyPotential<TensorAlgebra3D>(
                  *(new CubicElasticPotential<TensorAlgebra3D>())),eos),
    CrystalHEPlasticity<TensorAlgebra3D>(new SingleCrystalFCC(),
                                         new StdCrystalViscoPlasticity(
                                                  new FCCHardeningTaylor(*(new LinearIsotropicHardeningModel())),
                                                  new FCCRateDependency())) {}
  
  // copy constructor
  LinearCrystalHEPlasticityFCC3D(const LinearCrystalHEPlasticityFCC3D& src)
  : HyperElasticity<TensorAlgebra3D>(src), CrystalHEPlasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~LinearCrystalHEPlasticityFCC3D() {}
};

/**
 * The associated model builder
 */
class LinearCrystalHEPlasticityFCCBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  LinearCrystalHEPlasticityFCCBuilder();
  
  // the instance
  static LinearCrystalHEPlasticityFCCBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~LinearCrystalHEPlasticityFCCBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementation of the model: linear hardening, linearized kinematics.
 */
class LinearCrystalPlasticityFCC3D : public CrystalPlasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  LinearCrystalPlasticityFCC3D()
  : Elasticity<TensorAlgebra3D>(*(new CubicElasticPotential<TensorAlgebra3D>())),
    CrystalPlasticity<TensorAlgebra3D>(new SingleCrystalFCC(),
                                       new StdCrystalViscoPlasticity(
                                                new FCCHardeningTaylor(*(new LinearIsotropicHardeningModel())),
                                                new FCCRateDependency())) {}
  
  // copy constructor
  LinearCrystalPlasticityFCC3D(const LinearCrystalPlasticityFCC3D& src)
  : Elasticity<TensorAlgebra3D>(src), CrystalPlasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~LinearCrystalPlasticityFCC3D() {}
};

/**
 * The associated model builder
 */
class LinearCrystalPlasticityFCCBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  LinearCrystalPlasticityFCCBuilder();
  
  // the instance
  static LinearCrystalPlasticityFCCBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~LinearCrystalPlasticityFCCBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementations of the model: non-linear hardening.
 */
class NonLinearCrystalHEPlasticityFCC3D : public CrystalHEPlasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NonLinearCrystalHEPlasticityFCC3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
        new GeneralHenckyPotential<TensorAlgebra3D>(
                  *(new CubicElasticPotential<TensorAlgebra3D>())),eos),
  CrystalHEPlasticity<TensorAlgebra3D>(new SingleCrystalFCC(),
                                       new StdCrystalViscoPlasticity(
                                                new FCCHardeningTaylor(*(new NonLinearIsotropicHardeningModel())),
                                                new FCCRateDependency())) {}
  
  // copy constructor
  NonLinearCrystalHEPlasticityFCC3D(const NonLinearCrystalHEPlasticityFCC3D& src)
  : HyperElasticity<TensorAlgebra3D>(src), CrystalHEPlasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NonLinearCrystalHEPlasticityFCC3D() {}
};

/**
 * The associated model builder
 */
class NonLinearCrystalHEPlasticityFCCBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  NonLinearCrystalHEPlasticityFCCBuilder();
  
  // the instance
  static NonLinearCrystalHEPlasticityFCCBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~NonLinearCrystalHEPlasticityFCCBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementation of the model: non-linear hardening, linearized kinematics.
 */
class NonLinearCrystalPlasticityFCC3D : public CrystalPlasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NonLinearCrystalPlasticityFCC3D()
  : Elasticity<TensorAlgebra3D>(*(new CubicElasticPotential<TensorAlgebra3D>())),
    CrystalPlasticity<TensorAlgebra3D>(new SingleCrystalFCC(),
                                       new StdCrystalViscoPlasticity(
                                                new FCCHardeningTaylor(*(new NonLinearIsotropicHardeningModel())),
                                                new FCCRateDependency())) {}
  
  // copy constructor
  NonLinearCrystalPlasticityFCC3D(const NonLinearCrystalPlasticityFCC3D& src)
  : Elasticity<TensorAlgebra3D>(src), CrystalPlasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NonLinearCrystalPlasticityFCC3D() {}
};

/**
 * The associated model builder
 */
class NonLinearCrystalPlasticityFCCBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  NonLinearCrystalPlasticityFCCBuilder();
  
  // the instance
  static NonLinearCrystalPlasticityFCCBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~NonLinearCrystalPlasticityFCCBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementations of the model: polycrystal with linear hardening.
 */
class LinearPolyCrystalHEPlasticityFCC3D : public PolyCrystalHEModel<TensorAlgebra3D> {
  
 public:
  
  // constructor
  LinearPolyCrystalHEPlasticityFCC3D(EOS *eos = 0)
  : PolyCrystalHEModel<TensorAlgebra3D>(new LinearCrystalHEPlasticityFCC3D(),eos) {}
  
  // copy constructor
  LinearPolyCrystalHEPlasticityFCC3D(const LinearPolyCrystalHEPlasticityFCC3D& src)
  : PolyCrystalHEModel<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~LinearPolyCrystalHEPlasticityFCC3D() {}
};

/**
 * The associated model builder
 */
class LinearPolyCrystalHEPlasticityFCCBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  LinearPolyCrystalHEPlasticityFCCBuilder();
  
  // the instance
  static LinearPolyCrystalHEPlasticityFCCBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~LinearPolyCrystalHEPlasticityFCCBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementations of the model: polycrystal with non-linear hardening.
 */
class NonLinearPolyCrystalHEPlasticityFCC3D : public PolyCrystalHEModel<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NonLinearPolyCrystalHEPlasticityFCC3D(EOS *eos = 0)
  : PolyCrystalHEModel<TensorAlgebra3D>(new NonLinearCrystalHEPlasticityFCC3D(),eos) {}
  
  // copy constructor
  NonLinearPolyCrystalHEPlasticityFCC3D(const NonLinearPolyCrystalHEPlasticityFCC3D& src)
  : PolyCrystalHEModel<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NonLinearPolyCrystalHEPlasticityFCC3D() {}
};

/**
 * The associated model builder
 */
class NonLinearPolyCrystalHEPlasticityFCCBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  NonLinearPolyCrystalHEPlasticityFCCBuilder();
  
  // the instance
  static NonLinearPolyCrystalHEPlasticityFCCBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~NonLinearPolyCrystalHEPlasticityFCCBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
