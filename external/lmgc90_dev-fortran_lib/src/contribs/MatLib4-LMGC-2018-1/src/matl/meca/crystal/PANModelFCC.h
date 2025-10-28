/*
 *  $Id: PANModelFCC.h 202 2016-03-31 11:51:40Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_CRYSTAL_PAN_MODEL_FCC_H
#define ZORGLIB_MATL_MECA_CRYSTAL_PAN_MODEL_FCC_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/meca/crystal/CuitinoFCC.h> /* can be changed when specific rate-dependency model is implemented */
#include <matl/meca/crystal/SingleCrystalFCC.h>
#include <matl/meca/hyper/CrystalHEPlasticity.h>
#include <matl/meca/hyper/GeneralHenckyPotential.h>
#include <matl/meca/hyper/PolyCrystalHEModel.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing the FCC crystal plasticity hardening model,
 * Pierce-Asaro-Needleman style (revisited LS).
 */
class FCCHardeningPAN : public CrystalHardeningModel {
  
 protected:
  
  void hardeningModuli(const MaterialProperties&,const MatLibArray&,
                       const MatLibArray&,MatLibArray&,MatLibMatrix&,
                       std::vector<MatLibMatrix>&,bool,bool);
  
 public:
  
  // constructor
  FCCHardeningPAN() {}

  // copy constructor
  FCCHardeningPAN(const FCCHardeningPAN&) {}

  // destructor
  virtual ~FCCHardeningPAN() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of internal parameters
  unsigned int nIntPar() const {return SingleCrystalFCC::NSYS/2;}
  
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
 * Implementations of the model.
 */
class PANCrystalHEPlasticityFCC3D : public CrystalHEPlasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  PANCrystalHEPlasticityFCC3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
        new GeneralHenckyPotential<TensorAlgebra3D>(
                  *(new CubicElasticPotential<TensorAlgebra3D>())),eos),
    CrystalHEPlasticity<TensorAlgebra3D>(new SingleCrystalFCC(),
                                         new StdCrystalViscoPlasticity(
                                                  new FCCHardeningPAN(),
                                                  new FCCRateDependency())) {}
  
  // copy constructor
  PANCrystalHEPlasticityFCC3D(const PANCrystalHEPlasticityFCC3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src), CrystalHEPlasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~PANCrystalHEPlasticityFCC3D() {}
};

/**
 * The associated model builder
 */
class PANCrystalHEPlasticityFCCBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  PANCrystalHEPlasticityFCCBuilder();
  
  // the instance
  static PANCrystalHEPlasticityFCCBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~PANCrystalHEPlasticityFCCBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementations of the model.
 */
class PANPolyCrystalHEPlasticityFCC3D : public PolyCrystalHEModel<TensorAlgebra3D> {
  
 public:
  
  // constructor
  PANPolyCrystalHEPlasticityFCC3D(EOS *eos = 0)
  : PolyCrystalHEModel<TensorAlgebra3D>(new PANCrystalHEPlasticityFCC3D(),eos) {}
  
  // copy constructor
  PANPolyCrystalHEPlasticityFCC3D(const PANPolyCrystalHEPlasticityFCC3D& src) 
  : PolyCrystalHEModel<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~PANPolyCrystalHEPlasticityFCC3D() {}
};

/**
 * The associated model builder
 */
class PANPolyCrystalHEPlasticityFCCBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  PANPolyCrystalHEPlasticityFCCBuilder();
  
  // the instance
  static PANPolyCrystalHEPlasticityFCCBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~PANPolyCrystalHEPlasticityFCCBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
