/*
 *  $Id: CuitinoFCC.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_CRYSTAL_CUITINO_FCC_H
#define ZORGLIB_MATL_MECA_CRYSTAL_CUITINO_FCC_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/meca/crystal/SingleCrystalFCC.h>
#include <matl/meca/hyper/CrystalHEPlasticity.h>
#include <matl/meca/hyper/GeneralHenckyPotential.h>
#include <matl/meca/hyper/PolyCrystalHEModel.h>
#include <matl/meca/linear/CrystalPlasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing the FCC crystal plasticity hardening model,
 * Cuitino style (revisited LS).
 */
class FCCHardeningCuitino : public CrystalHardeningModel {
  
 protected:
  
  void hardeningMatrix(const MaterialProperties&,const MatLibArray&,
                       const MatLibArray&,MatLibArray&);
  
 public:
  
  // constructor
  FCCHardeningCuitino() {}

  // copy constructor
  FCCHardeningCuitino(const FCCHardeningCuitino&) {}

  // destructor
  virtual ~FCCHardeningCuitino() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of internal parameters
  unsigned int nIntPar() const {return SingleCrystalFCC::NSYS;}
  
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
 * Class describing the FCC crystal plasticity rate-dependency model.
 */
class FCCRateDependency : public CrystalRateDependencyModel {
  
 public:
  
  // constructor
  FCCRateDependency() {}
  
  // copy constructor
  FCCRateDependency(const FCCRateDependency&) {}
  
  // destructor
  virtual ~FCCRateDependency() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of additional internal parameters
  unsigned int nIntPar() const {return 0;}
  
  // dissipated energy
  double dissipatedEnergy(const MaterialProperties&,const ParameterSet&,
                          const MatLibArray&,MatLibArray&,
                          const MatLibArray&,const MatLibArray&,
                          MatLibArray&,MatLibArray&,MatLibMatrix&,
                          MatLibMatrix&,MatLibMatrix&,bool,bool);
};


/**
 * Implementation of the model.
 */
class CrystalHEPlasticityFCC3D : public CrystalHEPlasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  CrystalHEPlasticityFCC3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
        new GeneralHenckyPotential<TensorAlgebra3D>(
                  *(new CubicElasticPotential<TensorAlgebra3D>())),eos),
    CrystalHEPlasticity<TensorAlgebra3D>(new SingleCrystalFCC(),
                                         new StdCrystalViscoPlasticity(
                                                  new FCCHardeningCuitino(),
                                                  new FCCRateDependency())) {}
  
  // copy constructor
  CrystalHEPlasticityFCC3D(const CrystalHEPlasticityFCC3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src), CrystalHEPlasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CrystalHEPlasticityFCC3D() {}
};

/**
 * The associated model builder
 */
class CrystalHEPlasticityFCCBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CrystalHEPlasticityFCCBuilder();
  
  // the instance
  static CrystalHEPlasticityFCCBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CrystalHEPlasticityFCCBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementation of the model.
 */
class CrystalPlasticityFCC3D : public CrystalPlasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  CrystalPlasticityFCC3D()
  : Elasticity<TensorAlgebra3D>(*(new CubicElasticPotential<TensorAlgebra3D>())),
    CrystalPlasticity<TensorAlgebra3D>(new SingleCrystalFCC(),
                                       new StdCrystalViscoPlasticity(new FCCHardeningCuitino(),
                                                                     new FCCRateDependency())) {}
  
  // copy constructor
  CrystalPlasticityFCC3D(const CrystalPlasticityFCC3D& src) 
  : Elasticity<TensorAlgebra3D>(src), CrystalPlasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CrystalPlasticityFCC3D() {}
};

/**
 * The associated model builder
 */
class CrystalPlasticityFCCBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CrystalPlasticityFCCBuilder();
  
  // the instance
  static CrystalPlasticityFCCBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~CrystalPlasticityFCCBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementation of the model.
 */
class PolyCrystalHEPlasticityFCC3D : public PolyCrystalHEModel<TensorAlgebra3D> {
  
 public:
  
  // constructor
  PolyCrystalHEPlasticityFCC3D(EOS *eos = 0)
  : PolyCrystalHEModel<TensorAlgebra3D>(new CrystalHEPlasticityFCC3D(),eos) {}
  
  // copy constructor
  PolyCrystalHEPlasticityFCC3D(const PolyCrystalHEPlasticityFCC3D& src) 
  : PolyCrystalHEModel<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~PolyCrystalHEPlasticityFCC3D() {}
};

/**
 * The associated model builder
 */
class PolyCrystalHEPlasticityFCCBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  PolyCrystalHEPlasticityFCCBuilder();
  
  // the instance
  static PolyCrystalHEPlasticityFCCBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~PolyCrystalHEPlasticityFCCBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
