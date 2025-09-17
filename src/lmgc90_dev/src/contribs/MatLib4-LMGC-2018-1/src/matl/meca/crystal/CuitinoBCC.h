/*
 *  $Id: CuitinoBCC.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_CRYSTAL_CUITINO_BCC_H
#define ZORGLIB_MATL_MECA_CRYSTAL_CUITINO_BCC_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/meca/crystal/SingleCrystalBCC.h>
#include <matl/meca/hyper/CrystalHEPlasticity.h>
#include <matl/meca/hyper/GeneralHenckyPotential.h>
#include <matl/meca/hyper/PolyCrystalHEModel.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing the BCC crystal plasticity hardening model,
 * Stainier-Cuitino-Ortiz style.
 */
class BCCHardeningCuitino : public CrystalHardeningModel {

 public:

  // flags for anti-twinning systems
  static const bool ANTITWINNING[48];
  
 protected:
  
  void hardeningMatrix(const MaterialProperties&,const MatLibArray&,
                       const MatLibArray&,MatLibArray&);
  
 public:
  
  // constructor
  BCCHardeningCuitino() {}

  // copy constructor
  BCCHardeningCuitino(const BCCHardeningCuitino&) {}

  // destructor
  virtual ~BCCHardeningCuitino() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of internal parameters
  unsigned int nIntPar() const {return SingleCrystalBCC::NSYS;}
  
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
 * Class describing the BCC crystal plasticity rate-dependency model.
 */
class BCCRateDependency : public CrystalRateDependencyModel {
  
 public:
  
  // constructor
  BCCRateDependency() {}
  
  // copy constructor
  BCCRateDependency(const BCCRateDependency&) {}
  
  // destructor
  virtual ~BCCRateDependency() {}
  
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
 * Class describing the BCC crystal plasticity rate-dependency model
 * based on thermal activation.
 */
class BCCRateDependencyASinh : public CrystalRateDependencyModel {
  
 public:
  
  // constructor
  BCCRateDependencyASinh() {}
  
  // copy constructor
  BCCRateDependencyASinh(const BCCRateDependencyASinh&) {}
  
  // destructor
  virtual ~BCCRateDependencyASinh() {}
  
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
 * Implementations of the model.
 */
class CrystalHEPlasticityBCC3D : public CrystalHEPlasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  CrystalHEPlasticityBCC3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
        new GeneralHenckyPotential<TensorAlgebra3D>(
                  *(new CubicElasticPotential<TensorAlgebra3D>())),eos),
    CrystalHEPlasticity<TensorAlgebra3D>(new SingleCrystalBCC(),
                                         new StdCrystalViscoPlasticity(
                                                  new BCCHardeningCuitino(),
                                                  new BCCRateDependencyASinh())) {}
  
  // copy constructor
  CrystalHEPlasticityBCC3D(const CrystalHEPlasticityBCC3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src), CrystalHEPlasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CrystalHEPlasticityBCC3D() {}
};

/**
 * The associated model builder
 */
class CrystalHEPlasticityBCCBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CrystalHEPlasticityBCCBuilder();
  
  // the instance
  static CrystalHEPlasticityBCCBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CrystalHEPlasticityBCCBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementations of the model.
 */
class PolyCrystalHEPlasticityBCC3D : public PolyCrystalHEModel<TensorAlgebra3D> {
  
 public:
  
  // constructor
  PolyCrystalHEPlasticityBCC3D(EOS *eos = 0)
  : PolyCrystalHEModel<TensorAlgebra3D>(new CrystalHEPlasticityBCC3D(),eos) {}
  
  // copy constructor
  PolyCrystalHEPlasticityBCC3D(const PolyCrystalHEPlasticityBCC3D& src) 
  : PolyCrystalHEModel<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~PolyCrystalHEPlasticityBCC3D() {}
};

/**
 * The associated model builder
 */
class PolyCrystalHEPlasticityBCCBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  PolyCrystalHEPlasticityBCCBuilder();
  
  // the instance
  static PolyCrystalHEPlasticityBCCBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~PolyCrystalHEPlasticityBCCBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
