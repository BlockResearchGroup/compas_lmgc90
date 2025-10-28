/*
 *  $Id: CuitinoFCC1.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_CRYSTAL_CUITINO_FCC_1_H
#define ZORGLIB_MATL_MECA_CRYSTAL_CUITINO_FCC_1_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/crystal/CuitinoFCC.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing the FCC crystal plasticity hardening model,
 * Cuitino style (revisited LS).
 */
class FCCHardeningCuitino1 : public CrystalHardeningModel {
  
 public:
  
  // constructor
  FCCHardeningCuitino1() {}

  // copy constructor
  FCCHardeningCuitino1(const FCCHardeningCuitino1&) {}

  // destructor
  virtual ~FCCHardeningCuitino1() {}
  
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
 * Implementation of the model.
 */
class CrystalHEPlasticityFCCNew3D : public CrystalHEPlasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  CrystalHEPlasticityFCCNew3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
        new GeneralHenckyPotential<TensorAlgebra3D>(
                  *(new CubicElasticPotential<TensorAlgebra3D>())),eos),
    CrystalHEPlasticity<TensorAlgebra3D>(new SingleCrystalFCC(),
                                         new StdCrystalViscoPlasticity(
                                                  new FCCHardeningCuitino1(),
                                                  new FCCRateDependency())) {}
  
  // copy constructor
  CrystalHEPlasticityFCCNew3D(const CrystalHEPlasticityFCCNew3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src), CrystalHEPlasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CrystalHEPlasticityFCCNew3D() {}
};

/**
 * The associated model builder
 */
class CrystalHEPlasticityFCC1Builder : public ModelBuilder {
  
 private:
  
  // constructor
  CrystalHEPlasticityFCC1Builder();
  
  // the instance
  static CrystalHEPlasticityFCC1Builder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CrystalHEPlasticityFCC1Builder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementation of the model.
 */
class CrystalPlasticityFCCNew3D : public CrystalPlasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  CrystalPlasticityFCCNew3D()
  : Elasticity<TensorAlgebra3D>(*(new CubicElasticPotential<TensorAlgebra3D>())),
    CrystalPlasticity<TensorAlgebra3D>(new SingleCrystalFCC(),
                                       new StdCrystalViscoPlasticity(new FCCHardeningCuitino1(),
                                                                     new FCCRateDependency())) {}
  
  // copy constructor
  CrystalPlasticityFCCNew3D(const CrystalPlasticityFCCNew3D& src) 
  : Elasticity<TensorAlgebra3D>(src), CrystalPlasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CrystalPlasticityFCCNew3D() {}
};

/**
 * The associated model builder
 */
class CrystalPlasticityFCC1Builder : public ModelBuilder {
  
 private:
  
  // constructor
  CrystalPlasticityFCC1Builder();
  
  // the instance
  static CrystalPlasticityFCC1Builder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~CrystalPlasticityFCC1Builder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementation of the model.
 */
class PolyCrystalHEPlasticityFCCNew3D : public PolyCrystalHEModel<TensorAlgebra3D> {
  
 public:
  
  // constructor
  PolyCrystalHEPlasticityFCCNew3D(EOS *eos = 0)
  : PolyCrystalHEModel<TensorAlgebra3D>(new CrystalHEPlasticityFCC3D(),eos) {}
  
  // copy constructor
  PolyCrystalHEPlasticityFCCNew3D(const PolyCrystalHEPlasticityFCCNew3D& src) 
  : PolyCrystalHEModel<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~PolyCrystalHEPlasticityFCCNew3D() {}
};

/**
 * The associated model builder
 */
class PolyCrystalHEPlasticityFCC1Builder : public ModelBuilder {
  
 private:
  
  // constructor
  PolyCrystalHEPlasticityFCC1Builder();
  
  // the instance
  static PolyCrystalHEPlasticityFCC1Builder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~PolyCrystalHEPlasticityFCC1Builder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
