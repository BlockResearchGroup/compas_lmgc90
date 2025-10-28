/*
 *  $Id: CubicElasticPotential.h 202 2016-03-31 11:51:40Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_CUBIC_ELASTIC_POTENTIAL_H
#define ZORGLIB_MATL_MECA_LINEAR_CUBIC_ELASTIC_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/meca/linear/Elasticity.h>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing elastic cubic potentials.
 */
template <class ALG>
class CubicElasticPotential : virtual public Elasticity<ALG>::Potential {
  
 public:
  
  // define new types
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
    
  // constructor
  CubicElasticPotential() {}
  
  // copy constructor
  CubicElasticPotential(const CubicElasticPotential&) {}
  
  // destructor
  virtual ~CubicElasticPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Cubic elastic potential***" << std::endl;

    static const double ONE_THIRD = 1.e0/3.e0;

    unsigned int nDim = ALG::DIMENSION;
    double C11,C12,C44,K,mu,mu1,A;
    try {
      // get elastic moduli
      C11 = material.getDoubleProperty("C11_MODULUS");
      if (C11 < 0.e0) {
        if (os) (*os) << "ERROR: C11 modulus must be positive." << std::endl;
        throw InvalidPropertyException("C11 modulus");
      }
      C12 = material.getDoubleProperty("C12_MODULUS");
      // check positive-definiteness
      if (nDim >= 2) {
        double det = C11*C11-C12*C12;
        if (det <= 0.e0) {
          if (os) (*os) << "ERROR: Elasticity tensor must be positive-definite.\n";
          throw InvalidPropertyException("C12 modulus");
        }
      }
      if (nDim >= 3) {
        double det = C11*C11*C11-3*C11*C12*C12+2*C12*C12*C12;
        if (det <= 0.e0) {
          if (os) (*os) << "ERROR: Elasticity tensor must be positive-definite.\n";
          throw InvalidPropertyException("C12 modulus");
        }
      }
      C44 = material.getDoubleProperty("C44_MODULUS");
      if (C44 < 0.e0) {
        if (os) (*os) << "ERROR: C44 modulus must be positive." << std::endl;
        throw InvalidPropertyException("C44 modulus");
      }

      // compute other properties
      K = ONE_THIRD*(C11+2.*C12);
      mu = C44;
      mu1 = 0.5*(C11-C12);
      if (mu1 < 0.e0) {
        if (os) (*os) << "ERROR: anisotropy ratio must be positive." << std::endl;
        throw InvalidPropertyException("anisotropy ratio");
      }
      A = mu/mu1;

      material.setProperty("BULK_MODULUS",K);
      material.setProperty("SHEAR_MODULUS",mu);
      material.setProperty("ANISOTROPY_RATIO",A);
    }
    catch (NoSuchPropertyException) {
      // get shear modulus
      mu = material.getDoubleProperty("SHEAR_MODULUS");
      if (mu < 0.0e0) {
        if (os) (*os) << "ERROR: shear modulus must be positive." << std::endl;
        throw InvalidPropertyException("shear modulus");
      }
      material.setProperty("2ND_LAME_CONSTANT",mu);

      // get bulk modulus
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
      
      // get anisotropy ratio
      A = material.getDoubleProperty("ANISOTROPY_RATIO");
      if (A < 0.e0) {
        if (os) (*os) << "ERROR: anisotropy ratio must be positive." << std::endl;
        throw InvalidPropertyException("anisotropy ratio");
      }
      
      // compute other properties
      mu1 = mu/A;
      C11 = K+4*ONE_THIRD*mu1;
      C12 = K-2*ONE_THIRD*mu1;
      C44 = mu;

      // check positive-definiteness
      if (nDim >= 2) {
        double det = C11*C11-C12*C12;
        if (det <= 0.e0) {
          if (os) (*os) << "ERROR: Elasticity tensor must be positive-definite.\n";
          throw InvalidPropertyException("anisotropy ratio");
        }
      }
      if (nDim >= 3) {
        double det = C11*C11*C11-3*C11*C12*C12+2*C12*C12*C12;
        if (det <= 0.e0) {
          if (os) (*os) << "ERROR: Elasticity tensor must be positive-definite.\n";
          throw InvalidPropertyException("anisotropy ratio");
        }
      }

      material.setProperty("C11_MODULUS",C11);
      material.setProperty("C12_MODULUS",C12);
      material.setProperty("C44_MODULUS",C44);
    }

    if (os) {
      (*os) << "\tC11 modulus       = " << C11 << std::endl;
      (*os) << "\tC12 modulus       = " << C12 << std::endl;
      (*os) << "\tC44 modulus       = " << C44 << std::endl;
      (*os) << "\tbulk modulus      = " << K   << std::endl;
      (*os) << "\tshear modulus     = " << mu  << std::endl;
      (*os) << "\tanisotropy ratio  = " << A   << std::endl;
    }
     
    // check for eigen-strain
    StdProperty< SYM_TENSOR > BProp;
    SYM_TENSOR& B = BProp.value();
    double B11,B12,B22,B13,B23,B33;
    try {
      B11 = material.getDoubleProperty("EIGEN_STRAIN_11");
    }
    catch (NoSuchPropertyException) {
      B11 = 0.0e0;
      material.setProperty("EIGEN_STRAIN_11",B11);
    }
    B[SYM_TENSOR::MAP[0][0]] = B11;

    if (ALG::DIMENSION >= 2) {
      try {
        B12 = material.getDoubleProperty("EIGEN_STRAIN_12");
      }
      catch (NoSuchPropertyException) {
        B12 = 0.0e0;
        material.setProperty("EIGEN_STRAIN_12",B12);
      }
      B[SYM_TENSOR::MAP[0][1]] = B12;
    }
    try {
      B22 = material.getDoubleProperty("EIGEN_STRAIN_22");
    }
    catch (NoSuchPropertyException) {
      B22 = 0.0e0;
      material.setProperty("EIGEN_STRAIN_22",B22);
    }
    B[SYM_TENSOR::MAP[1][1]] = B22;
    if (ALG::DIMENSION == 3) {
      try {
        B13 = material.getDoubleProperty("EIGEN_STRAIN_13");
      }
      catch (NoSuchPropertyException) {
        B13 = 0.0e0;
        material.setProperty("EIGEN_STRAIN_13",B13);
      }
      B[SYM_TENSOR::MAP[0][2]] = B13;
      try {
        B23 = material.getDoubleProperty("EIGEN_STRAIN_23");
      }
      catch (NoSuchPropertyException) {
        B23 = 0.0e0;
        material.setProperty("EIGEN_STRAIN_23",B23);
      }
      B[SYM_TENSOR::MAP[1][2]] = B23;
    }
    try {
      B33 = material.getDoubleProperty("EIGEN_STRAIN_33");
    }
    catch (NoSuchPropertyException) {
      B33 = 0.0e0;
      material.setProperty("EIGEN_STRAIN_33",B33);
    }
    B[SYM_TENSOR::MAP[2][2]] = B33;
    if (normL1(B) > 0.0e0) {
      if (os) {
        (*os) << "\n\teigen strain 11 = " << B11 << std::endl;
        if (ALG::DIMENSION >= 2)
          (*os) << "\teigen strain 12 = " << B12 << std::endl;
        (*os) << "\teigen strain 22 = " << B22 << std::endl;
        if (ALG::DIMENSION == 3) {
          (*os) << "\teigen strain 13 = " << B13 << std::endl;
          (*os) << "\teigen strain 23 = " << B23 << std::endl;
        }
        (*os) << "\teigen strain 33 = " << B33 << std::endl;
      }
      material.setProperty("EIGEN_STRAIN",BProp);
    }
     
    // initialize structural tensor
    StdProperty<SYM_TENSOR4> IProp;
    SYM_TENSOR4& I1 = IProp.value();
    I1 = 0.0e0;
    I1[SYM_TENSOR::MAP[0][0]][SYM_TENSOR::MAP[0][0]] = 1.0e0;
    I1[SYM_TENSOR::MAP[1][1]][SYM_TENSOR::MAP[1][1]] = 1.0e0;
    I1[SYM_TENSOR::MAP[2][2]][SYM_TENSOR::MAP[2][2]] = 1.0e0;
    material.setProperty("CUBIC_STRUCTURAL_TENSOR",IProp);

    // compute dilatational elastic wave speed
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      double c = std::sqrt(C11/rho);
      material.setProperty("CELERITY",c);
      if (os) (*os) << "\n\tcelerity           = " << c << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tcelerity is not defined" << std::endl;
    }
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    TENSOR R0;
    R.toTensor(R0);
    
    // rotate eigen-strain
    if (material.checkProperty("EIGEN_STRAIN")) {
      // get eigen-strain
      StdProperty< SYM_TENSOR >& BProp
        = dynamic_cast<StdProperty< SYM_TENSOR >&>(material.getProperty("EIGEN_STRAIN"));
      SYM_TENSOR& B = BProp.value();
      
      // rotate eigen strain (B is in "covariant" form, i.e. gamma, not epsilon)
      B = covariant(B.contravariant().contravariantPush(R0));
    }

    // get structural tensor
    StdProperty<SYM_TENSOR4>& IProp = 
      dynamic_cast< StdProperty<SYM_TENSOR4>& >(material.getProperty("CUBIC_STRUCTURAL_TENSOR"));
    SYM_TENSOR4& I1 = IProp.value();
    
    // rotate structural tensor
    I1 = contravariantPush(I1,R0);
  }
  
  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& gam,SYM_TENSOR& sig,
                      SYM_TENSOR4& M,bool first,bool second) {

    // get elastic constants
    double C11 = material.getDoubleProperty("C11_MODULUS");
    double C12 = material.getDoubleProperty("C12_MODULUS");
    double C44 = material.getDoubleProperty("C44_MODULUS");
    double h = 2*C44+C12-C11; // anisotropy factor
    
    // transform engineering strains
    SYM_TENSOR eps = contravariant(gam);

    // account for eigen-strain
    if (material.checkProperty("EIGEN_STRAIN")) {
      StdProperty< SYM_TENSOR >& BProp
        = dynamic_cast<StdProperty< SYM_TENSOR >&>(material.getProperty("EIGEN_STRAIN"));
      SYM_TENSOR& B = BProp.value();
      eps -= contravariant(B);
    }

    // compute anisotropic term
    StdProperty<SYM_TENSOR4>& I1 = 
      dynamic_cast< StdProperty<SYM_TENSOR4>& >(material.getProperty("CUBIC_STRUCTURAL_TENSOR"));
    SYM_TENSOR eps1 = innerProd2(I1.value(),eps);

    // potential
    double tr = trace(eps);
    double W = 0.5*C12*tr*tr + C44*innerProd2(eps,eps) - 0.5*h*innerProd2(eps,eps1);
    if (!first && !second) return W;
    
    // stress
    double mu2 = C44+C44;
    if (first) {
      static SYM_TENSOR delta = SYM_TENSOR::identity();
      sig = (C12*tr)*delta + mu2*eps - h*eps1;
    }
    
    // tangent
    if (second) {
      static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
      static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
      M = mu2*I+(3*C12)*K-h*I1.value();
    }

    return W;
  }
  
  // compute material stiffness (Hooke) tensor
  void computeStiffness(const MaterialProperties& material,
                        const ParameterSet& extPar,SYM_TENSOR4& M) {
    
    // get elastic constants
    double C11 = material.getDoubleProperty("C11_MODULUS");
    double C12 = material.getDoubleProperty("C12_MODULUS");
    double mu2 = 2*material.getDoubleProperty("C44_MODULUS");
    double h = mu2+C12-C11; // anisotropy factor

    // get structural tensor
    StdProperty<SYM_TENSOR4>& I1 = 
      dynamic_cast< StdProperty<SYM_TENSOR4>& >(material.getProperty("CUBIC_STRUCTURAL_TENSOR"));
    
    // stiffness
    static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
    static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
    M = mu2*I+(3*C12)*K-h*I1.value();
  }
};


/**
 * Implementations of the model.
 */
class CubicElasticity3D : public Elasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  CubicElasticity3D()
  : Elasticity<TensorAlgebra3D>(new CubicElasticPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  CubicElasticity3D(const CubicElasticity3D& src) 
  : Elasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CubicElasticity3D() {}
};
class CubicElasticity2D : public Elasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  CubicElasticity2D()
  : Elasticity<TensorAlgebra2D>(new CubicElasticPotential<TensorAlgebra2D>()) {}
  
  // copy constructor
  CubicElasticity2D(const CubicElasticity2D& src) 
  : Elasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CubicElasticity2D() {}
};
class CubicElasticity1D : public Elasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  CubicElasticity1D()
  : Elasticity<TensorAlgebra1D>(new CubicElasticPotential<TensorAlgebra1D>()) {}
  
  // copy constructor
  CubicElasticity1D(const CubicElasticity1D& src) 
  : Elasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CubicElasticity1D() {}
};

/**
 * The associated model builder
 */
class CubicElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CubicElasticityBuilder();
  
  // the instance
  static CubicElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CubicElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
