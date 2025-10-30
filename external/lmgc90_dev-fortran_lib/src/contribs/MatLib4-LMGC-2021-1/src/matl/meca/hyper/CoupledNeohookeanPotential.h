/*
 *  $Id: CoupledNeohookeanPotential.h 134 2013-07-22 19:05:07Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_COUPLED_NEOHOOKEAN_POTENTIAL_H
#define ZORGLIB_MATL_MECA_HYPER_COUPLED_NEOHOOKEAN_POTENTIAL_H

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
 * Class describing hyperelastic coupled Neohookean potentials,
 * (cf. Holzapfel eq. 6.148), but with a pre-strain.
 */
template <class ALG>
class CoupledNeohookeanPotential : virtual public HyperElasticity<ALG>::Potential {
  
 public:
  
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

 public:
  
  // constructor
  CoupledNeohookeanPotential() {}
  
  // copy constructor
  CoupledNeohookeanPotential(const CoupledNeohookeanPotential&) {}
  
  // destructor
  virtual ~CoupledNeohookeanPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Coupled neo-Hookean potential (with eigen-strain)***" << std::endl;
    
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
     
    // check eigen-strain
    StdProperty< SYM_TENSOR > BProp;
    SYM_TENSOR& B = BProp.value();
    double B11,B12,B22,B13,B23,B33;
    try {
      B11 = material.getDoubleProperty("EIGEN_STRAIN_11");
    }
    catch (NoSuchPropertyException) {
      B11 = 1.0e0;
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
      B22 = 1.0e0;
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
      B33 = 1.0e0;
      material.setProperty("EIGEN_STRAIN_33",B33);
    }
    B[SYM_TENSOR::MAP[2][2]] = B33;
    if (normL1(B-SYM_TENSOR::identity()) > 0.0e0) {
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
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    Tensor3D R0;
    R.toTensor(R0);

    // rotate eigen-strain
    if (material.checkProperty("EIGEN_STRAIN")) {
      // get eigen-strain
      StdProperty< SYM_TENSOR >& BProp
        = dynamic_cast<StdProperty< SYM_TENSOR >&>(material.getProperty("EIGEN_STRAIN"));
      SYM_TENSOR& B = BProp.value();
    
      // rotate eigen-strain
      B = B.contravariantPush(R0);
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
    
    // shear modulus and Poisson ratio
    double mu = material.getDoubleProperty("SHEAR_MODULUS");
    double nu = material.getDoubleProperty("POISSON_COEFFICIENT");
    double beta = nu/(1.0-nu-nu);
    
    // get eigenstrain
    SYM_TENSOR Binv;
    double detB;
    if (material.checkProperty("EIGEN_STRAIN")) {
      StdProperty< SYM_TENSOR >& BProp
        = dynamic_cast<StdProperty< SYM_TENSOR >&>(material.getProperty("EIGEN_STRAIN"));
      SYM_TENSOR& B = BProp.value();
      Binv = B.inverse(detB);
    }
    else {
      Binv = SYM_TENSOR::identity();
      detB = 1.0e0;
    }
    
    // potential
    double coef = std::pow(detB/detC,beta);
    double W = 0.5*mu*(innerProd2(C,Binv)-3.0+(coef-1.0)/beta);
    
    // stress tensor
    if (first) {
      S = mu*(Binv-coef*Cinv);
    }
    
    // consistent tangent
    if (second) {
      double muBar = mu*coef;
      M = (2*muBar*beta)*outerProd(Cinv,Cinv);
      M.addIJKL(muBar,Cinv);
    }
    
    return W;
  }
};


/**
 * Implementations of the model.
 */
class CoupledNeohookean3D : public HyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledNeohookean3D()
  : HyperElasticity<TensorAlgebra3D>(new CoupledNeohookeanPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledNeohookean3D(const CoupledNeohookean3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledNeohookean3D() {}
};
class CoupledNeohookean2D : public HyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledNeohookean2D()
  : HyperElasticity<TensorAlgebra2D>(new CoupledNeohookeanPotential<TensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledNeohookean2D(const CoupledNeohookean2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledNeohookean2D() {}
};
class CoupledNeohookean1D : public HyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledNeohookean1D()
  : HyperElasticity<TensorAlgebra1D>(new CoupledNeohookeanPotential<TensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledNeohookean1D(const CoupledNeohookean1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledNeohookean1D() {}
};

/**
 * The associated model builder
 */
class CoupledNeohookeanBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledNeohookeanBuilder();
  
  // the instance
  static CoupledNeohookeanBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~CoupledNeohookeanBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
