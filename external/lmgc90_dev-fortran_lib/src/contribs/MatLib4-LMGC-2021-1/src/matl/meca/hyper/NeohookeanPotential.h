/*
 *  $Id: NeohookeanPotential.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_NEOHOOKEAN_POTENTIAL_H
#define ZORGLIB_MATL_MECA_HYPER_NEOHOOKEAN_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/meca/hyper/HyperElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing hyperelastic neohookean potentials.
 */
template <class ALG>
class NeohookeanPotential : virtual public SpectralHEPotential<ALG> {
  
 public:

  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

 public:

  // constructor
  NeohookeanPotential() {}
  
  // copy constructor
  NeohookeanPotential(const NeohookeanPotential&) {}

  // destructor
  virtual ~NeohookeanPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
     if (os) (*os) << "\n\t***Neo-Hookean potential***" << std::endl;
     
    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;

    double E,K,lambda,mu,nu;
    try {
      // get Young's modulus
      E = material.getDoubleProperty("YOUNG_MODULUS");
      if (E < 0.e0) {
        if (os) (*os) << "ERROR: Young's modulus must be positive." << std::endl;
        throw InvalidPropertyException("Young's modulus");
      }

      // get Poisson's coefficient
      nu = material.getDoubleProperty("POISSON_COEFFICIENT");
      if (nu < -1.0e0 || nu > 0.5e0) {
        if (os) (*os) << "ERROR: Poisson's coefficient must be in [-1.0,0.5]." << std::endl;
        throw InvalidPropertyException("Poisson's coefficient");
      }

      // compute other properties
      mu = 0.5*E/(1.+nu);
      K = ONE_THIRD*E/(1.-2*nu);
      lambda = K-TWO_THIRD*mu;

      material.setProperty("BULK_MODULUS",K);
      material.setProperty("SHEAR_MODULUS",mu);
      material.setProperty("1ST_LAME_CONSTANT",lambda);
      material.setProperty("2ND_LAME_CONSTANT",mu);
    }
    catch (NoSuchPropertyException) {
      // get second Lame constant (a.k.a. shear modulus)
      try {
        mu = material.getDoubleProperty("2ND_LAME_CONSTANT");
        if (mu < 0.0e0) {
          if (os) (*os) << "ERROR: Lame constants must be positive." << std::endl;
          throw InvalidPropertyException("second Lame constant");
        }
      }
      catch (NoSuchPropertyException) {
        try {
          mu = material.getDoubleProperty("SHEAR_MODULUS");
          if (mu < 0.0e0) {
            if (os) (*os) << "ERROR: shear modulus must be positive." << std::endl;
            throw InvalidPropertyException("shear modulus");
          }
          material.setProperty("2ND_LAME_CONSTANT",mu);
        }
        catch (NoSuchPropertyException e) {
          if (os) (*os) << "ERROR: second Lame constant is not defined." << std::endl;
          throw e;
        }
      }

      // get first Lame constant
      try {
        lambda = material.getDoubleProperty("1ST_LAME_CONSTANT");
        if (lambda < 0.0e0) {
          if (os) (*os) << "ERROR: Lame constants must be positive." << std::endl;
          throw InvalidPropertyException("first Lame constant");
        }
        K = lambda+TWO_THIRD*mu;
        material.setProperty("BULK_MODULUS",K);
      }
      catch (NoSuchPropertyException) {
        try {
          K = material.getDoubleProperty("BULK_MODULUS");
          if (K < 0.0e0) {
            if (os) (*os) << "ERROR: bulk modulus must be positive." << std::endl;
            throw InvalidPropertyException("bulk modulus");
          }
        }
        catch (NoSuchPropertyException) {
          if (os) (*os) << "WARNING: bulk modulus set to zero." << std::endl;
          K = 0.0e0;
          material.setProperty("BULK_MODULUS",K);
        }
        lambda = K-TWO_THIRD*mu;
        material.setProperty("1ST_LAME_CONSTANT",lambda);
      }

      // compute other properties
      nu = (3*K-2*mu)/(6*K+2*mu);
      E = 2*mu*(1.+nu);

      material.setProperty("YOUNG_MODULUS",E);
      material.setProperty("POISSON_COEFFICIENT",nu);
    }
     
    if (os) {
      (*os) << "\tYoung's modulus       = " << E << std::endl;
      (*os) << "\tPoisson's coefficient = " << nu << std::endl;
      (*os) << "\tbulk modulus          = " << K << std::endl;
      (*os) << "\t1st Lame constant     = " << lambda << std::endl;
      (*os) << "\t2nd Lame constant     = " << mu << std::endl;
    }
    
    // compute dilatational elastic wave speed
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      double c = std::sqrt((lambda+2*mu)/rho);
      material.setProperty("CELERITY",c);
      if (os) (*os) << "\n\tcelerity              = " << c << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tcelerity is not defined" << std::endl;
    }
  }
  
  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& C,SYM_TENSOR& S,
                      SYM_TENSOR4& M,bool first,bool second) {
    
    // compute determinant and inverse
    double detC;
    SYM_TENSOR Cinv;
    if (first || second)
      Cinv = C.inverse(detC);
    else
      detC = determinant(C);
    
    // Lame constants
    double lambda = material.getDoubleProperty("1ST_LAME_CONSTANT");
    double mu = material.getDoubleProperty("2ND_LAME_CONSTANT");
    
    // potential
    double trC = trace(C);
    double logJ = 0.5*std::log(detC);
    double W = 0.5*lambda*logJ*logJ - mu*logJ + 0.5*mu*(trC-3);
    
    // stress tensor
    double muBar = mu-lambda*logJ;
    static const SYM_TENSOR I = SYM_TENSOR::identity();
    if (first) S = mu*I-muBar*Cinv;
    
    // consistent tangent
    if (second) {
      M = lambda*outerProd(Cinv,Cinv);
      M.addIJKL(muBar,Cinv);
    }
    
    return W;
  }
  
  // compute stored energy from principal stretches
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const double eps[],double sig[],
                      double M[][3],bool first,bool second) {
    
    // invariants
    double trC  = eps[0]+eps[1]+eps[2];
    double detC = eps[0]*eps[1]*eps[2];
    
    // Lame constants
    double lambda = material.getDoubleProperty("1ST_LAME_CONSTANT");
    double mu = material.getDoubleProperty("2ND_LAME_CONSTANT");
    
    // potential
    double logJ = 0.5*std::log(detC);
    double W = 0.5*lambda*logJ*logJ - mu*logJ + 0.5*mu*(trC-3);
    
    // principal stresses
    if (first) {
      double coef = lambda*logJ-mu;
      for (unsigned int k=0; k < 3; k++) sig[k] = 0.5*(coef/eps[k]+mu);
    }

    // second derivatives
    if (second) {
      double coef = 0.5*(lambda*logJ-mu);
      for (unsigned int k=0; k < 3; k++)
        for (unsigned int l=0; l < 3; l++) {
          double val = 1.0e0/(eps[k]*eps[l]);
          M[k][l] = 0.25*lambda*val;
          if (k == l) M[k][l] -= coef*val;
        }
    }
    
    return W;
  }
};


/**
 * Implementations of the model.
 */
class Neohookean3D : public HyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  Neohookean3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(new NeohookeanPotential<TensorAlgebra3D>(),
                                     eos) {}
  
  // copy constructor
  Neohookean3D(const Neohookean3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~Neohookean3D() {}
};
class Neohookean2D : public HyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  Neohookean2D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra2D>(new NeohookeanPotential<TensorAlgebra2D>(),
                                     eos) {}
  
  // copy constructor
  Neohookean2D(const Neohookean2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~Neohookean2D() {}
};
class Neohookean1D : public HyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  Neohookean1D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra1D>(new NeohookeanPotential<TensorAlgebra1D>(),
                                     eos) {}
  
  // copy constructor
  Neohookean1D(const Neohookean1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~Neohookean1D() {}
};

/**
 * The associated model builder
 */
class NeohookeanBuilder : public ModelBuilder {

 private:
  
  // constructor
  NeohookeanBuilder();

  // the instance
  static NeohookeanBuilder const* BUILDER;

 public:
  
  // destructor
  virtual ~NeohookeanBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
