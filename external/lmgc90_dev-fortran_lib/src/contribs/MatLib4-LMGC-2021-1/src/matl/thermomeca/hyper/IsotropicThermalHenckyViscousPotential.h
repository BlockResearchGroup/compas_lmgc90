/*
 *  $Id: ThermalHenckyViscousPotential.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2020, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_THERMOMECA_HYPER_ISOTROPIC_THERMAL_HENCKY_VISCOUS_POTENTIAL_H
#define ZORGLIB_MATL_THERMOMECA_HYPER_ISOTROPIC_THERMAL_HENCKY_VISCOUS_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <matl/thermomeca/linear/IsotropicThermoElasticity.h>
#include <matl/thermomeca/hyper/GeneralThermalHenckyPotential.h>
#include <matl/thermomeca/hyper/ThermoHyperViscoElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing (isotropic) Hencky viscous potentials.
 */
template <class ALG>
class IsotropicThermalHenckyViscousPotential
: virtual public ThermoViscoHyperElasticity<ALG>::ViscousPotential {

 protected:

  // isochoric?
  bool isochoric;

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

  // constructor
  IsotropicThermalHenckyViscousPotential(bool i) {isochoric=i;}

  // copy constructor
  IsotropicThermalHenckyViscousPotential(const IsotropicThermalHenckyViscousPotential& src) {
    isochoric = src.isochoric;
  }

  // destructor
  virtual ~IsotropicThermalHenckyViscousPotential() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {

    if (os) {
      (*os) << "\n\t***Isotropic Hencky thermo-visco-hyperelastic potential" << std::endl;
    }

    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;

    double E,K=0.0e0,lambda=0.0e0,mu,nu;

    // reference temperature
    double T0;
    try {
      T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
      if (T0 <= 0.e0) {
        if (os) (*os) << "ERROR: reference temperature must be strictly positive." << std::endl;
        throw InvalidPropertyException("reference temperature");
      }
    }
    catch (NoSuchPropertyException) {
      // use initial temperature
      try {
        T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
        if (T0 <= 0.e0) {
          if (os) (*os) << "ERROR: initial temperature must be strictly positive." << std::endl;
          throw InvalidPropertyException("initial temperature");
        }
        material.setProperty("REFERENCE_TEMPERATURE",T0);
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: reference temperature cannot be set." << std::endl;
        throw e;
      }
    }

    try {
      // get viscous Young's modulus
      try {
        Function& fctE = material.getFunctionProperty("VISCOUS_YOUNG_MODULUS_EVOLUTION");
        E = fctE.value(T0);
        material.setProperty("VISCOUS_YOUNG_MODULUS",E);
        if (os) {
          (*os) << "\n\tViscous Young's modulus temperature dependence: ";
          (*os) << fctE << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        E = material.getDoubleProperty("VISCOUS_YOUNG_MODULUS");
      }
      if (E < 0.e0) {
        if (os) (*os) << "ERROR: viscous Young's modulus must be positive." << std::endl;
        throw InvalidPropertyException("viscous Young modulus");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: viscous Young's modulus is not defined." << std::endl;
      throw e;
    }

    try {
      // get viscousPoisson's coefficient
      try {
        Function& fctN = material.getFunctionProperty("VISCOUS_POISSON_COEFFICIENT_EVOLUTION");
        nu = fctN.value(T0);
        material.setProperty("VISCOUS_POISSON_COEFFICIENT",nu);
        if (os) {
          (*os) << "\n\tViscous Poisson's coefficient temperature dependence: ";
          (*os) << fctN << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        nu = material.getDoubleProperty("VISCOUS_POISSON_COEFFICIENT");
      }
      if (nu < -1.0e0 || nu > 0.5e0) {
        if (os) (*os) << "ERROR: viscous Poisson's coefficient must be in [-1.0,0.5]." << std::endl;
        throw InvalidPropertyException("Poisson's coefficient");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: viscous Poisson's coefficient is not defined." << std::endl;
      throw e;
    }

    // compute other properties
    mu = 0.5*E/(1.+nu);
    K = ONE_THIRD*E/(1.-2*nu);
    lambda = K-TWO_THIRD*mu;

    material.setProperty("VISCOUS_BULK_MODULUS",K);
    material.setProperty("VISCOUS_SHEAR_MODULUS",mu);
    material.setProperty("VISCOUS_1ST_LAME_CONSTANT",lambda);
    material.setProperty("VISCOUS_2ND_LAME_CONSTANT",mu);

    if (os) {
      (*os) << "\tviscous Young's modulus       = " << E << std::endl;
      (*os) << "\tviscous Poisson's coefficient = " << nu << std::endl;
      if (isochoric) {
        (*os) << "\tviscous shear modulus         = " << mu << std::endl;
      }
      else {
        (*os) << "\tviscous bulk modulus          = " << K << std::endl;
        (*os) << "\tviscous 1st Lame constant     = " << lambda << std::endl;
        (*os) << "\tviscous 2nd Lame constant     = " << mu << std::endl;
      }
    }
  }

  // compute dissipated energy
  double dissipatedEnergy(const MaterialProperties& material,
                          const ParameterSet& extPar,
                          const SYM_TENSOR& Dv,const SYM_TENSOR& Cvpar,double T,
                          SYM_TENSOR& sigV1,SYM_TENSOR& sigV2,double& dNv,
                          SYM_TENSOR4& hv11,SYM_TENSOR4& hv22,SYM_TENSOR4& hv12,
                          SYM_TENSOR& dSigV1,SYM_TENSOR& dSigV2,double& Cmv,
                          bool first,bool second) {

    // get viscous properties
    double E,nu,lambda,mu;
    double dE,dnu,dlambda,dmu;

    try { // get viscous Young's modulus
      Function& fctE = material.getFunctionProperty("VISCOUS_YOUNG_MODULUS_EVOLUTION");
      E = fctE.value(T,dE);
    }
    catch (NoSuchPropertyException) {
      E = material.getDoubleProperty("VISCOUS_YOUNG_MODULUS");
      dE = 0.0e0;
    }
    if (E < 0.e0) throw InvalidPropertyException("viscous Young's modulus");

    try { // get viscous Poisson's coefficient
      Function& fctN = material.getFunctionProperty("VISCOUS_POISSON_COEFFICIENT_EVOLUTION");
      nu = fctN.value(T,dnu);
    }
    catch (NoSuchPropertyException) {
      nu = material.getDoubleProperty("VISCOUS_POISSON_COEFFICIENT");
      dnu = 0.0e0;
    }
    if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException("viscous Poisson's coefficient");

    // compute other properties
    mu = 0.5*E/(1.+nu);
    lambda = 2*mu*nu/(1.-2*nu);
    dmu = 0.5*(dE-E*dnu/(1.+nu))/(1.+nu);
    dlambda = 2*(dmu*nu+mu*dnu/(1.-2*nu))/(1.-2*nu);
    
    // potential
    double tr = trace(Dv);
    double norm = innerProd2(Dv,Dv);
    double phi = 0.5*lambda*tr*tr + mu*norm;
    if (!first && !second) return phi;
    
    // stress
    static SYM_TENSOR delta = SYM_TENSOR::identity();
    double mu2 = mu+mu;
    if (first) {
      sigV1 = 0.0e0;
      sigV2 = (lambda*tr)*delta + mu2*Dv;
      dNv = 0.5*dlambda*tr*tr + dmu*norm;
    }
    
    // tangent
    if (second) {
      static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
      static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
      double dmu2 = dmu+dmu;
      hv11 = 0.0e0;
      hv12 = 0.0e0;
      hv22 = mu2*I+(3*lambda)*K;
      dSigV1 = 0.0e0;
      dSigV2 = (dlambda*tr)*delta + dmu2*Dv;
      Cmv = 0.0e0;
    }
    return phi;
  }
};

/**
 * Implementations of the model.
 */
class IsotropicThermalHenckyViscoHyperElasticity3D : public ThermoViscoHyperElasticity<TensorAlgebra3D> {

 public:

  // constructor
  IsotropicThermalHenckyViscoHyperElasticity3D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra3D>(
          new GeneralThermalHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra3D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra3D>()),
    ThermoViscoHyperElasticity<TensorAlgebra3D>(new IsotropicThermalHenckyViscousPotential<TensorAlgebra3D>(true)) {}

  // copy constructor
  IsotropicThermalHenckyViscoHyperElasticity3D(const IsotropicThermalHenckyViscoHyperElasticity3D& src)
  : ThermoHyperElasticity<TensorAlgebra3D>(src), ThermoViscoHyperElasticity<TensorAlgebra3D>(src) {}

  // destructor
  virtual ~IsotropicThermalHenckyViscoHyperElasticity3D() {}
};
class IsotropicThermalHenckyViscoHyperElasticity2D : public ThermoViscoHyperElasticity<TensorAlgebra2D> {

 public:

  // constructor
  IsotropicThermalHenckyViscoHyperElasticity2D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra2D>(
          new GeneralThermalHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra2D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra2D>()),
    ThermoViscoHyperElasticity<TensorAlgebra2D>(new IsotropicThermalHenckyViscousPotential<TensorAlgebra2D>(true)) {}

  // copy constructor
  IsotropicThermalHenckyViscoHyperElasticity2D(const IsotropicThermalHenckyViscoHyperElasticity2D& src)
  : ThermoHyperElasticity<TensorAlgebra2D>(src), ThermoViscoHyperElasticity<TensorAlgebra2D>(src) {}

  // destructor
  virtual ~IsotropicThermalHenckyViscoHyperElasticity2D() {}
};
class IsotropicThermalHenckyViscoHyperElasticity1D : public ThermoViscoHyperElasticity<TensorAlgebra1D> {

 public:

  // constructor
  IsotropicThermalHenckyViscoHyperElasticity1D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra1D>(
          new GeneralThermalHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra1D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra1D>()),
    ThermoViscoHyperElasticity<TensorAlgebra1D>(new IsotropicThermalHenckyViscousPotential<TensorAlgebra1D>(true)) {}

  // copy constructor
  IsotropicThermalHenckyViscoHyperElasticity1D(const IsotropicThermalHenckyViscoHyperElasticity1D& src)
  : ThermoHyperElasticity<TensorAlgebra1D>(src), ThermoViscoHyperElasticity<TensorAlgebra1D>(src) {}

  // destructor
  virtual ~IsotropicThermalHenckyViscoHyperElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicThermalHenckyViscoHyperElasticityBuilder : public ModelBuilder {

 private:

  // constructor
  IsotropicThermalHenckyViscoHyperElasticityBuilder();

  // the instance
  static IsotropicThermalHenckyViscoHyperElasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~IsotropicThermalHenckyViscoHyperElasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
