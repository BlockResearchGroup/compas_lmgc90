/*
 *  $Id: IsotropicHenckyViscousPotential.h 252 2018-05-18 11:58:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2018, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_ISOTROPIC_HENCKY_VISCOUS_POTENTIAL_H
#define ZORGLIB_MATL_MECA_HYPER_ISOTROPIC_HENCKY_VISCOUS_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/hyper/GeneralHenckyPotential.h>
#include <matl/meca/hyper/ViscoHyperElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing (isotropic) Hencky viscous potentials.
 */
template <class ALG>
class IsotropicHenckyViscousPotential
: virtual public SpectralHEViscousPotential<ALG> {

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

  // constructor
  IsotropicHenckyViscousPotential() {}

  // copy constructor
  IsotropicHenckyViscousPotential(const IsotropicHenckyViscousPotential&) {}

  // destructor
  virtual ~IsotropicHenckyViscousPotential() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {

    if (os) {
      (*os) << "\n\t***Isotropic Hencky visco-hyperelastic potential" << std::endl;
    }

    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;

    double E,K=0.0e0,lambda=0.0e0,mu,nu;

    try {
      // get viscous Young's modulus
      E = material.getDoubleProperty("VISCOUS_YOUNG_MODULUS");

      if (E < 0.e0) {
        if (os) (*os) << "ERROR: viscous Young's modulus must be positive." << std::endl;
        throw InvalidPropertyException("viscous Young modulus");
      }

      // get viscousPoisson's coefficient
      nu = material.getDoubleProperty("VISCOUS_POISSON_COEFFICIENT");

      if (nu < -1.0e0 || nu > 0.5e0) {
        if (os) (*os) << "ERROR: viscous Poisson's coefficient must be in [-1.0,0.5]." << std::endl;
        throw InvalidPropertyException("viscous Poisson's coefficient");
      }

      // compute other properties
      mu = 0.5*E/(1.+nu);
      K = ONE_THIRD*E/(1.-2*nu);
      lambda = K-TWO_THIRD*mu;

      material.setProperty("VISCOUS_BULK_MODULUS",K);
      material.setProperty("VISCOUS_SHEAR_MODULUS",mu);
      material.setProperty("VISCOUS_1ST_LAME_CONSTANT",lambda);
      material.setProperty("VISCOUS_2ND_LAME_CONSTANT",mu);
    }
    catch (NoSuchPropertyException) {

      // get viscous second Lame constant (a.k.a. viscous shear modulus)
      try {
        mu = material.getDoubleProperty("VISCOUS_2ND_LAME_CONSTANT");

        if (mu < 0.0e0) {
          if (os) (*os) << "ERROR: viscous Lame constants must be positive." << std::endl;
          throw InvalidPropertyException("viscous 2nd Lame constant");
        }
        material.setProperty("VISCOUS_SHEAR_MODULUS",mu);
      }
      catch (NoSuchPropertyException) {
        mu = material.getDoubleProperty("VISCOUS_SHEAR_MODULUS");
        
        if (mu < 0.0e0) {
          if (os) (*os) << "ERROR: viscous shear modulus must be positive." << std::endl;
          throw InvalidPropertyException("viscous shear modulus");
        }
        material.setProperty("VISCOUS_2ND_LAME_CONSTANT",mu);
      }

      // get viscous first Lame constant
      try {
        lambda = material.getDoubleProperty("VISCOUS_1ST_LAME_CONSTANT");

        if (lambda < 0.0e0) {
          if (os) (*os) << "ERROR: viscous Lame constants must be positive." << std::endl;
            throw InvalidPropertyException("viscous 1st Lame constant");
        }
        K = lambda+TWO_THIRD*mu;
        material.setProperty("VISCOUS_BULK_MODULUS",K);
      }
      catch (NoSuchPropertyException) {
        try {
          K = material.getDoubleProperty("VISCOUS_BULK_MODULUS");

          if (K < 0.0e0) {
            if (os) (*os) << "ERROR: viscous bulk modulus must be positive." << std::endl;
            throw InvalidPropertyException("viscous bulk modulus");
          }
        }
        catch (NoSuchPropertyException) {
          if (os) (*os) << "WARNING: viscous bulk modulus set to zero." << std::endl;
          K = 0.0e0;
          material.setProperty("VISCOUS_BULK_MODULUS",K);
        }
        lambda = K-TWO_THIRD*mu;
        material.setProperty("VISCOUS_1ST_LAME_CONSTANT",lambda);
      }

      // compute other properties
      nu = (3*K-2*mu)/(6*K+2*mu);
      E = 2*mu*(1.+nu);
      
      material.setProperty("VISCOUS_YOUNG_MODULUS",E);
      material.setProperty("VISCOUS_POISSON_COEFFICIENT",nu);
    }

    if (os) {
      (*os) << "\tviscous Young's modulus       = " << E << std::endl;
      (*os) << "\tviscous Poisson's coefficient = " << nu << std::endl;
      (*os) << "\tviscous shear modulus         = " << mu << std::endl;
      (*os) << "\tviscous bulk modulus          = " << K << std::endl;
      (*os) << "\tviscous 1st Lame constant     = " << lambda << std::endl;
      (*os) << "\tviscous 2nd Lame constant     = " << mu << std::endl;
    }
  }

  // compute dissipated energy
  double dissipatedEnergy(const MaterialProperties& material,
                          const ParameterSet& extPar,
                          const TENSOR& F0,const SYM_TENSOR& C,SYM_TENSOR& S,
                          SYM_TENSOR4& M,double dTime,bool first,bool second) {

    // get viscous properties
    double lambda = material.getDoubleProperty("VISCOUS_1ST_LAME_CONSTANT");
    double mu     = material.getDoubleProperty("VISCOUS_2ND_LAME_CONSTANT");
    double mu2 = mu+mu;

    // compute viscous strain-rate
    SYM_TENSOR dC;
    dC = covariantPush(C,F0);
    double coef = 0.5/dTime;
    SYM_TENSOR dLogC[SYM_TENSOR::MEMSIZE];
    SYM_TENSOR d2LogC[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
    SYM_TENSOR epsDot = coef*log(dC,dLogC,d2LogC,first || second,second);
    
    // potential
    double tr = trace(epsDot);
    double phi = 0.5*lambda*tr*tr + mu*innerProd2(epsDot,epsDot);
    if (!first && !second) return phi;
    
    // stress
    unsigned int sz = SYM_TENSOR::MEMSIZE;
    SYM_TENSOR sig,Sv;
    static SYM_TENSOR delta = SYM_TENSOR::identity();
    if (first || second)
      sig = (lambda*tr)*delta + mu2*epsDot;
    if (first) {
      for (unsigned int ij=0; ij < sz; ij++)
        Sv[ij] = innerProd2(sig,dLogC[ij]);
      S = contravariantPull(Sv,F0);
    }
    
    // tangent
    if (second) {
      SYM_TENSOR4 Mv;
      SYM_TENSOR *p = *d2LogC;
      for (unsigned int ij=0; ij < sz; ij++)
        for (unsigned int kl=0; kl < sz; kl++, p++)
          Mv[ij][kl] = lambda*trace(dLogC[ij])*trace(dLogC[kl])
                      +mu2*innerProd2(dLogC[ij],dLogC[kl])
                      +2*dTime*innerProd2(sig,*p);
      M = contravariantPull(Mv,F0);
    }

    return phi;
  }
       
  // compute dissipated energy from principal stretches
  double dissipatedEnergy(const MaterialProperties& material,
                          const ParameterSet& extPar,
                          const double epsDot[],double sig[],double M[][3],
                          bool first,bool second) {
    
    // get elastic modulus
    double lambda = material.getDoubleProperty("VISCOUS_1ST_LAME_CONSTANT");
    double mu     = material.getDoubleProperty("VISCOUS_2ND_LAME_CONSTANT");
    double mu2 = mu+mu;

    // compute dissipation potential
    double tr = epsDot[0]+epsDot[1]+epsDot[2];
    double phi = 0.5*lambda*tr*tr
                +mu*(epsDot[0]*epsDot[0]+epsDot[1]*epsDot[1]+epsDot[2]*epsDot[2]);

    // first derivative
    if (first) {
      double p = lambda*tr;
      for (unsigned int k=0; k < 3; k++) sig[k] = p+mu2*epsDot[k];
    }
    if (second) {
      double val = lambda+mu2;
      for (unsigned int k=0; k < 3; k++)
        for (unsigned int l=0; l < 3; l++)
          if (k == l)
            M[k][l] = val;
          else
            M[k][l] = lambda;
    }

    return phi;
  }
};

/**
 * Implementations of the model.
 */
class IsotropicHenckyKelvinViscoElasticity3D : public ViscoHyperElasticity<TensorAlgebra3D> {

 public:

  // constructor
  IsotropicHenckyKelvinViscoElasticity3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
          new GeneralHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicElasticPotential<TensorAlgebra3D>())),
          eos),
    ViscoHyperElasticity<TensorAlgebra3D>(
          new IsotropicHenckyViscousPotential<TensorAlgebra3D>()) {}

  // copy constructor
  IsotropicHenckyKelvinViscoElasticity3D(const IsotropicHenckyKelvinViscoElasticity3D& src)
  : HyperElasticity<TensorAlgebra3D>(src), ViscoHyperElasticity<TensorAlgebra3D>(src) {}

  // destructor
  virtual ~IsotropicHenckyKelvinViscoElasticity3D() {}
};
class IsotropicHenckyKelvinViscoElasticity2D : public ViscoHyperElasticity<TensorAlgebra2D> {

 public:

  // constructor
  IsotropicHenckyKelvinViscoElasticity2D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra2D>(
          new GeneralHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicElasticPotential<TensorAlgebra2D>())),
          eos),
    ViscoHyperElasticity<TensorAlgebra2D>(
          new IsotropicHenckyViscousPotential<TensorAlgebra2D>()) {}

  // copy constructor
  IsotropicHenckyKelvinViscoElasticity2D(const IsotropicHenckyKelvinViscoElasticity2D& src)
  : HyperElasticity<TensorAlgebra2D>(src), ViscoHyperElasticity<TensorAlgebra2D>(src) {}

  // destructor
  virtual ~IsotropicHenckyKelvinViscoElasticity2D() {}
};
class IsotropicHenckyKelvinViscoElasticity1D : public ViscoHyperElasticity<TensorAlgebra1D> {

 public:

  // constructor
  IsotropicHenckyKelvinViscoElasticity1D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra1D>(
          new GeneralHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicElasticPotential<TensorAlgebra1D>())),
          eos),
    ViscoHyperElasticity<TensorAlgebra1D>(
          new IsotropicHenckyViscousPotential<TensorAlgebra1D>()) {}

  // copy constructor
  IsotropicHenckyKelvinViscoElasticity1D(const IsotropicHenckyKelvinViscoElasticity1D& src)
  : HyperElasticity<TensorAlgebra1D>(src), ViscoHyperElasticity<TensorAlgebra1D>(src) {}

  // destructor
  virtual ~IsotropicHenckyKelvinViscoElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicHenckyKelvinViscoElasticityBuilder : public ModelBuilder {

 private:

  // constructor
  IsotropicHenckyKelvinViscoElasticityBuilder();

  // the instance
  static IsotropicHenckyKelvinViscoElasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~IsotropicHenckyKelvinViscoElasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif

