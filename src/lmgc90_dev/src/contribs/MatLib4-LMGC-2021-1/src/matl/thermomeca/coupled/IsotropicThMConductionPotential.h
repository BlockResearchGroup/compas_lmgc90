/*
 *  $Id: IsotropicThMConductionPotential.h 156 2014-10-15 20:26:02Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_COUPLED_STD_ISOTROPIC_THERMO_MECHANICAL_CONDUCTION_POTENTIAL_H
#define ZORGLIB_MATL_COUPLED_STD_ISOTROPIC_THERMO_MECHANICAL_CONDUCTION_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <matl/thermomeca/coupled/CoupledStdThermoMechanics.h>
#include <matl/thermomeca/hyper/J2ThermoHEPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for standard (non-linear) isotropic thermo-mechanical conduction potentials.
 */
template <class ALG1,class ALG2>
class IsotropicThMConductionPotential
  : virtual public CoupledStdThermoMechanics<ALG1,ALG2>::ConductionPotential {
  
 public:
  
  // define new types
  typedef typename ALG1::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG1::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG1::Tensor::TYPE     TENSOR;
  typedef typename ALG1::Tensor4          TENSOR4;
  typedef typename ALG2::Vector           VECTOR;

  // default constructor
  IsotropicThMConductionPotential() {}
  
  // copy constructor
  IsotropicThMConductionPotential(const IsotropicThMConductionPotential&) {}

  // destructor
  virtual ~IsotropicThMConductionPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic (non-linear) thermo-mechanical conductivity***" << std::endl;

    // get conductivity coefficient
    double k;
    try {
      k = material.getDoubleProperty("THERMAL_CONDUCTIVITY_COEFFICIENT");
      if (k < 0.e0) {
        if (os) (*os) << "ERROR: thermal conductivity coefficient must be positive." << std::endl;
        throw InvalidPropertyException("thermal conductivity coefficient");
      }
    }
    catch (NoSuchPropertyException) {
      try {
        k = material.getDoubleProperty("CONDUCTIVITY_COEFFICIENT");
        if (k < 0.e0) {
          if (os) (*os) << "ERROR: conductivity coefficient must be positive." << std::endl;
          throw InvalidPropertyException("conductivity coefficient");
        }
        material.setProperty("THERMAL_CONDUCTIVITY_COEFFICIENT",k);
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: thermal conductivity coefficient is not defined." << std::endl;
        throw e;
      }
    }
    if (os) (*os) << "\n\tthermal conductivity coefficient = " << k << std::endl;
  }
  
  // compute 
  double diffusionEnergy(const MaterialProperties& material,const ParameterSet& extPar,
                         const SYM_TENSOR& C,double T,const VECTOR& G,
                         SYM_TENSOR& S,double& N,VECTOR& H,
                         SYM_TENSOR4& M,double& CT,MatLibMatrix& K,
                         SYM_TENSOR& ST,SYM_TENSOR SG[],VECTOR& HT,
                         bool computeFirst,bool computeSecond) {

    // get conductivity coefficient
    double k = material.getDoubleProperty("THERMAL_CONDUCTIVITY_COEFFICIENT");

    // compute "Lagrangian metric"
    double J;
    SYM_TENSOR Cinv;
    Cinv = C.inverse(J);
    if (J < 1.0e-16)
      throw UpdateFailedException("zero jacobian (det[C])");
    J = std::sqrt(J);
    MatLibMatrix K0(VECTOR::MEMSIZE);
    for (unsigned int k=0; k < VECTOR::MEMSIZE; k++)
      for (unsigned int l=0; l < VECTOR::MEMSIZE; l++)
        K0[k][l] = Cinv[SYM_TENSOR::MAP[k][l]];
    SYM_TENSOR4 M0;
    if (computeFirst || computeSecond) {
      M0 = outerProd(Cinv,Cinv);
      M0.addIJKL(-1.0e0,Cinv,Cinv);
    }

    // compute diffusion energy
    double X;
    double coef = k*J*T;
    double val = 1.0/T;
    if (computeFirst) {
      H = coef*(K0*G);
      X = 0.5*(G*H);
      N = X*val;
      SYM_TENSOR GG;
      GG = 0.0e0;
      for (unsigned int k=0; k < VECTOR::MEMSIZE; k++)
        for (unsigned int l=0; l < VECTOR::MEMSIZE; l++)
          GG[SYM_TENSOR::MAP[k][l]] += G[k]*G[l];
      S = 0.5*coef*(M0*GG);
    }
    else
      X = 0.5*coef*(G*(K0*G));
    
    if (computeSecond) {
      // effective conduction matrix
      K = coef*K0;
      // contribution to stiffness matrix
      VECTOR H0 = K0*G;
      SYM_TENSOR HH;
      HH = 0.0e0;
      for (unsigned int k=0; k < VECTOR::MEMSIZE; k++)
        for (unsigned int l=0; l < VECTOR::MEMSIZE; l++)
          HH[SYM_TENSOR::MAP[k][l]] += H0[k]*H0[l];
      M = X*M0-coef*(outerProd(Cinv,HH)+outerProd(HH,Cinv));
      M.addIJKL(coef,Cinv,HH);
      M.addIJKL(coef,HH,Cinv);
      // other terms
      for (unsigned int k=0; k < VECTOR::MEMSIZE; k++) {
        SG[k] = 0.0e0;
        for (unsigned int ij=0; ij < SYM_TENSOR::MEMSIZE; ij++)
          for (unsigned int l=0; l < VECTOR::MEMSIZE; l++)
            SG[k][ij] += M0[ij][SYM_TENSOR::MAP[k][l]]*G[l];
        SG[k] *= coef;
      }
      // temperature-dependence effects
      ST = val*S;
      HT = val*H;
      CT = 0.0e0;
    }
    
    return X;
  }
};


/**
 * Coupled thermo-hyperelasticity
 */
class CoupledIsotropicThermoHyperElasticity3D
: public CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledIsotropicThermoHyperElasticity3D()
  : CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new IsotropicThermoHyperElasticity3D(),
                new IsotropicThMConductionPotential<TensorAlgebra3D,StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledIsotropicThermoHyperElasticity3D(const CoupledIsotropicThermoHyperElasticity3D& src) 
  : CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledIsotropicThermoHyperElasticity3D() {}
};
class CoupledIsotropicThermoHyperElasticity2D 
: public CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledIsotropicThermoHyperElasticity2D()
  : CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new IsotropicThermoHyperElasticity2D(),
                new IsotropicThMConductionPotential<TensorAlgebra2D,StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledIsotropicThermoHyperElasticity2D(const CoupledIsotropicThermoHyperElasticity2D& src) 
  : CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledIsotropicThermoHyperElasticity2D() {}
};
class CoupledIsotropicThermoHyperElasticity1D 
: public CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledIsotropicThermoHyperElasticity1D()
  : CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new IsotropicThermoHyperElasticity1D(),
                new IsotropicThMConductionPotential<TensorAlgebra1D,StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledIsotropicThermoHyperElasticity1D(const CoupledIsotropicThermoHyperElasticity1D& src) 
  : CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledIsotropicThermoHyperElasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledIsotropicThermoHyperElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledIsotropicThermoHyperElasticityBuilder();
  
  // the instance
  static CoupledIsotropicThermoHyperElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CoupledIsotropicThermoHyperElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Coupled finite strains J2 thermo-plasticity (linear isotropic hardening)
 */
class CoupledLinearIsotropicJ2ThermoHEPlasticity3D
: public CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledLinearIsotropicJ2ThermoHEPlasticity3D()
  : CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new LinearIsotropicJ2ThermoHEPlasticity3D(),
                new IsotropicThMConductionPotential<TensorAlgebra3D,StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledLinearIsotropicJ2ThermoHEPlasticity3D(const CoupledLinearIsotropicJ2ThermoHEPlasticity3D& src) 
  : CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledLinearIsotropicJ2ThermoHEPlasticity3D() {}
};
class CoupledLinearIsotropicJ2ThermoHEPlasticity2D 
: public CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledLinearIsotropicJ2ThermoHEPlasticity2D()
  : CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new LinearIsotropicJ2ThermoHEPlasticity2D(),
                new IsotropicThMConductionPotential<TensorAlgebra2D,StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledLinearIsotropicJ2ThermoHEPlasticity2D(const CoupledLinearIsotropicJ2ThermoHEPlasticity2D& src) 
  : CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledLinearIsotropicJ2ThermoHEPlasticity2D() {}
};
class CoupledLinearIsotropicJ2ThermoHEPlasticity1D 
: public CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledLinearIsotropicJ2ThermoHEPlasticity1D()
  : CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new LinearIsotropicJ2ThermoHEPlasticity1D(),
                new IsotropicThMConductionPotential<TensorAlgebra1D,StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledLinearIsotropicJ2ThermoHEPlasticity1D(const CoupledLinearIsotropicJ2ThermoHEPlasticity1D& src) 
  : CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledLinearIsotropicJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledLinearIsotropicJ2ThermoHEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledLinearIsotropicJ2ThermoHEPlasticityBuilder();
  
  // the instance
  static CoupledLinearIsotropicJ2ThermoHEPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CoupledLinearIsotropicJ2ThermoHEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Coupled finite strains J2 thermo-plasticity (nonlinear isotropic hardening)
 */
class CoupledNonLinearIsotropicJ2ThermoHEPlasticity3D
: public CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledNonLinearIsotropicJ2ThermoHEPlasticity3D()
  : CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new NonLinearIsotropicJ2ThermoHEPlasticity3D(),
                new IsotropicThMConductionPotential<TensorAlgebra3D,StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledNonLinearIsotropicJ2ThermoHEPlasticity3D(const CoupledNonLinearIsotropicJ2ThermoHEPlasticity3D& src) 
  : CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledNonLinearIsotropicJ2ThermoHEPlasticity3D() {}
};
class CoupledNonLinearIsotropicJ2ThermoHEPlasticity2D 
: public CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledNonLinearIsotropicJ2ThermoHEPlasticity2D()
  : CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new NonLinearIsotropicJ2ThermoHEPlasticity2D(),
                new IsotropicThMConductionPotential<TensorAlgebra2D,StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledNonLinearIsotropicJ2ThermoHEPlasticity2D(const CoupledNonLinearIsotropicJ2ThermoHEPlasticity2D& src) 
  : CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledNonLinearIsotropicJ2ThermoHEPlasticity2D() {}
};
class CoupledNonLinearIsotropicJ2ThermoHEPlasticity1D 
: public CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledNonLinearIsotropicJ2ThermoHEPlasticity1D()
  : CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new NonLinearIsotropicJ2ThermoHEPlasticity1D(),
                new IsotropicThMConductionPotential<TensorAlgebra1D,StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledNonLinearIsotropicJ2ThermoHEPlasticity1D(const CoupledNonLinearIsotropicJ2ThermoHEPlasticity1D& src) 
  : CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledNonLinearIsotropicJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledNonLinearIsotropicJ2ThermoHEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledNonLinearIsotropicJ2ThermoHEPlasticityBuilder();
  
  // the instance
  static CoupledNonLinearIsotropicJ2ThermoHEPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CoupledNonLinearIsotropicJ2ThermoHEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Coupled finite strains J2 thermo-plasticity (nonlinear isotropic hardening + asinh rate-dependency)
 */
class CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity3D
: public CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity3D()
  : CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new NonLinearASinhIsotropicJ2ThermoHEPlasticity3D(),
                new IsotropicThMConductionPotential<TensorAlgebra3D,StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity3D(const CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity3D& src) 
  : CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity3D() {}
};
class CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity2D 
: public CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity2D()
  : CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new NonLinearASinhIsotropicJ2ThermoHEPlasticity2D(),
                new IsotropicThMConductionPotential<TensorAlgebra2D,StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity2D(const CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity2D& src) 
  : CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity2D() {}
};
class CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity1D 
: public CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity1D()
  : CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new NonLinearASinhIsotropicJ2ThermoHEPlasticity1D(),
                new IsotropicThMConductionPotential<TensorAlgebra1D,StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity1D(const CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity1D& src) 
  : CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder();
  
  // the instance
  static CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CoupledNonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};
      
      
/**
 * Coupled finite strains J2 thermo-plasticity (Norton-Hoff isotropic hardening / rate-dependency)
 */
class CoupledNortonHoffIsotropicJ2ThermoHEPlasticity3D
: public CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {

 public:

  // constructor
  CoupledNortonHoffIsotropicJ2ThermoHEPlasticity3D()
  : CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new NortonHoffIsotropicJ2ThermoHEPlasticity3D(),
                new IsotropicThMConductionPotential<TensorAlgebra3D,StdTensorAlgebra3D>()) {}

  // copy constructor
  CoupledNortonHoffIsotropicJ2ThermoHEPlasticity3D(const CoupledNortonHoffIsotropicJ2ThermoHEPlasticity3D& src)
  : CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}

  // destructor
  virtual ~CoupledNortonHoffIsotropicJ2ThermoHEPlasticity3D() {}
};
class CoupledNortonHoffIsotropicJ2ThermoHEPlasticity2D
: public CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {

 public:

  // constructor
  CoupledNortonHoffIsotropicJ2ThermoHEPlasticity2D()
  : CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new NortonHoffIsotropicJ2ThermoHEPlasticity2D(),
                new IsotropicThMConductionPotential<TensorAlgebra2D,StdTensorAlgebra2D>()) {}

  // copy constructor
  CoupledNortonHoffIsotropicJ2ThermoHEPlasticity2D(const CoupledNortonHoffIsotropicJ2ThermoHEPlasticity2D& src)
  : CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}

  // destructor
  virtual ~CoupledNortonHoffIsotropicJ2ThermoHEPlasticity2D() {}
};
class CoupledNortonHoffIsotropicJ2ThermoHEPlasticity1D
: public CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {

 public:

  // constructor
  CoupledNortonHoffIsotropicJ2ThermoHEPlasticity1D()
  : CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new NortonHoffIsotropicJ2ThermoHEPlasticity1D(),
                new IsotropicThMConductionPotential<TensorAlgebra1D,StdTensorAlgebra1D>()) {}

  // copy constructor
  CoupledNortonHoffIsotropicJ2ThermoHEPlasticity1D(const CoupledNortonHoffIsotropicJ2ThermoHEPlasticity1D& src)
  : CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}

  // destructor
  virtual ~CoupledNortonHoffIsotropicJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledNortonHoffIsotropicJ2ThermoHEPlasticityBuilder : public ModelBuilder {

 private:

  // constructor
  CoupledNortonHoffIsotropicJ2ThermoHEPlasticityBuilder();

  // the instance
  static CoupledNortonHoffIsotropicJ2ThermoHEPlasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~CoupledNortonHoffIsotropicJ2ThermoHEPlasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
