/*
 *  $Id: OrthotropicElasticPotential.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ORTHOTROPIC_ELASTIC_POTENTIAL_H
#define ZORGLIB_MATL_MECA_LINEAR_ORTHOTROPIC_ELASTIC_POTENTIAL_H

// config
#include <matlib_macros.h>

// std C++ library
#include <limits>
// local
#include <math/MathUtils.h>
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/meca/linear/Elasticity.h>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing elastic orthotropic potentials.
 */
template <class ALG>
class OrthotropicElasticPotential : virtual public Elasticity<ALG>::Potential {
  
 public:
  
  // define new types
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
    
  // constructor
  OrthotropicElasticPotential() {}
  
  // copy constructor
  OrthotropicElasticPotential(const OrthotropicElasticPotential&) {}
  
  // destructor
  virtual ~OrthotropicElasticPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Orthotropic elastic potential***" << std::endl;

    unsigned int nDim = ALG::DIMENSION;
    double E1,E2,E3,G12=0.e0,G13=0.e0,G23=0.e0,nu12,nu13,nu23;

    // get elasticity coefficients
    E1 = material.getDoubleProperty("YOUNG_MODULUS_1");
    if (E1 < 0.e0) {
      if (os) (*os) << "ERROR: Elastic moduli must be positive.\n";
      throw InvalidPropertyException("E1 modulus");
    }
    E2 = material.getDoubleProperty("YOUNG_MODULUS_2");
    if (E2 < 0.e0) {
      if (os) (*os) << "ERROR: Elastic moduli must be positive.\n";
      throw InvalidPropertyException("E2 modulus");
    }
    E3 = material.getDoubleProperty("YOUNG_MODULUS_3");
    if (E3 < 0.e0) {
      if (os) (*os) << "ERROR: Elastic moduli must be positive.\n";
      throw InvalidPropertyException("E3 modulus");
    }

    nu12 = material.getDoubleProperty("POISSON_COEFFICIENT_12");
    nu13 = material.getDoubleProperty("POISSON_COEFFICIENT_13");
    nu23 = material.getDoubleProperty("POISSON_COEFFICIENT_23");
    // no bounds on Poisson coefficients in anisotropic materials      

    // build partial compliance tensor
    double S[6],C[6];
    S[0] = 1.e0/E1;
    S[1] = -nu12/E1;
    S[2] = 1.e0/E2;
    S[3] = -nu13/E1;
    S[4] = -nu23/E2;
    S[5] = 1.e0/E3;
    // invert and check positive-definiteness
    double det = invsym3(S,C,std::numeric_limits<double>::min());
    if (det <= 0.0e0) {
      if (os) (*os) << "ERROR: Elasticity tensor must be positive-definite.\n";
      throw InvalidPropertyException("elastic moduli");
    }

    // store stiffness components
    material.setProperty("C11_MODULUS",C[0]);
    material.setProperty("C12_MODULUS",C[1]);
    material.setProperty("C22_MODULUS",C[2]);
    material.setProperty("C13_MODULUS",C[3]);
    material.setProperty("C23_MODULUS",C[4]);
    material.setProperty("C33_MODULUS",C[5]);

    if (nDim >= 2) {
      G12 = material.getDoubleProperty("SHEAR_MODULUS_12");
      if (G12 < 0.e0) {
        if (os) (*os) << "ERROR: Elastic moduli must be positive.\n";
        throw InvalidPropertyException("G12 modulus");
      }
      material.setProperty("C44_MODULUS",G12);
    }
    if (nDim >= 3) {
      G13 = material.getDoubleProperty("SHEAR_MODULUS_13");
      if (G13 < 0.e0) {
        if (os) (*os) << "ERROR: Elastic moduli must be positive.\n";
        throw InvalidPropertyException("G13 modulus");
      }
      material.setProperty("C55_MODULUS",G13);
      G23 = material.getDoubleProperty("SHEAR_MODULUS_23");
      if (G23 < 0.e0) {
        if (os) (*os) << "ERROR: Elastic moduli must be positive.\n";
        throw InvalidPropertyException("G23 modulus");
      }
      material.setProperty("C66_MODULUS",G23);
    }
    
    // compute alternate coefficients
    double lambda1,lambda2,lambda3,mu1,mu2,mu3;
    mu1 =  G12+G13-G23;
    mu2 =  G12-G13+G23;
    mu3 = -G12+G13+G23;
    lambda1 = C[0]-2*mu1;
    lambda2 = C[2]-2*mu2;
    lambda3 = C[5]-2*mu3;
    material.setProperty("1ST_LAME_CONSTANT_1",lambda1);
    material.setProperty("1ST_LAME_CONSTANT_2",lambda2);
    material.setProperty("1ST_LAME_CONSTANT_3",lambda3);
    material.setProperty("2ND_LAME_CONSTANT_1",mu1);
    material.setProperty("2ND_LAME_CONSTANT_2",mu2);
    material.setProperty("2ND_LAME_CONSTANT_3",mu3);

    // print-out
    if (os) {
      (*os) << "\tYoung's modulus 1        = " << E1 << std::endl;
      (*os) << "\tYoung's modulus 2        = " << E2 << std::endl;
      (*os) << "\tYoung's modulus 3        = " << E3 << std::endl;
      (*os) << "\tPoisson's coefficient 12 = " << nu12 << std::endl;
      (*os) << "\tPoisson's coefficient 13 = " << nu13 << std::endl;
      (*os) << "\tPoisson's coefficient 23 = " << nu23 << std::endl;
      if (nDim >= 2) {
        (*os) << "\tshear modulus         12 = " <<  G12 << std::endl;
      }
      if (nDim >= 3) {
        (*os) << "\tshear modulus         13 = " <<  G13 << std::endl;
        (*os) << "\tshear modulus         23 = " <<  G23 << std::endl;
      }
    }
    
    // initialize structural tensor
    StdProperty<SYM_TENSOR> MProp1,MProp2,MProp3;
    SYM_TENSOR& M1 = MProp1.value();
    SYM_TENSOR& M2 = MProp2.value();
    SYM_TENSOR& M3 = MProp3.value();
    M1 = 0.0e0; M1[SYM_TENSOR::MAP[0][0]] = 1.0e0;
    M2 = 0.0e0; M2[SYM_TENSOR::MAP[1][1]] = 1.0e0;
    M3 = 0.0e0; M3[SYM_TENSOR::MAP[2][2]] = 1.0e0;
    material.setProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_1",MProp1);
    material.setProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_2",MProp2);
    material.setProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_3",MProp3);

    // compute dilatational elastic wave speed
    // NOT GOOD AT ALL !!!
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      double c = std::sqrt(E1/rho);
      material.setProperty("CELERITY",c);
      if (os) (*os) << "\n\tcelerity           = " << c << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tcelerity is not defined" << std::endl;
    }
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    
    // get structural tensors
    StdProperty<SYM_TENSOR>& MProp1 = 
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_1"));
    SYM_TENSOR& M1 = MProp1.value();
    StdProperty<SYM_TENSOR>& MProp2 = 
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_2"));
    SYM_TENSOR& M2 = MProp2.value();
    StdProperty<SYM_TENSOR>& MProp3 = 
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_3"));
    SYM_TENSOR& M3 = MProp3.value();
    
    // rotate structural tensors
    TENSOR R0;
    R.toTensor(R0);
    M1 = contravariantPush(M1,R0);
    M2 = contravariantPush(M2,R0);
    M3 = contravariantPush(M3,R0);
  }
  
  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& gam,SYM_TENSOR& sig,
                      SYM_TENSOR4& M,bool first,bool second) {

    // get elastic constants
    double lambda1 = material.getDoubleProperty("1ST_LAME_CONSTANT_1");
    double lambda2 = material.getDoubleProperty("1ST_LAME_CONSTANT_2");
    double lambda3 = material.getDoubleProperty("1ST_LAME_CONSTANT_3");
    double mu1 = material.getDoubleProperty("2ND_LAME_CONSTANT_1");
    double mu2 = material.getDoubleProperty("2ND_LAME_CONSTANT_2");
    double mu3 = material.getDoubleProperty("2ND_LAME_CONSTANT_3");
    double C12 = material.getDoubleProperty("C12_MODULUS");
    double C13 = material.getDoubleProperty("C13_MODULUS");
    double C23 = material.getDoubleProperty("C23_MODULUS");

    // transform engineering strains
    SYM_TENSOR eps = contravariant(gam);

    // get structural tensors and invariants
    StdProperty<SYM_TENSOR>& M1 =
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_1"));
    double J1 = innerProd2(M1.value(),eps);
    SYM_TENSOR eps1 = symProd(M1.value(),eps);
    double J4 = innerProd2(eps,eps1);
    StdProperty<SYM_TENSOR>& M2 =
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_2"));
    double J2 = innerProd2(M2.value(),eps);
    SYM_TENSOR eps2 = symProd(M2.value(),eps);
    double J5 = innerProd2(eps,eps2);
    StdProperty<SYM_TENSOR>& M3 =
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_3"));
    double J3 = innerProd2(M3.value(),eps);
    SYM_TENSOR eps3 = symProd(M3.value(),eps);
    double J6 = innerProd2(eps,eps3);

    // potential
    double W = 0.5*(lambda1*J1*J1+lambda2*J2*J2+lambda3*J3*J3)
              +C12*J1*J2+C13*J1*J3+C23*J2*J3+mu1*J4+mu2*J5+mu3*J6;
    if (!first && !second) return W;
    
    // stress
    if (first) {
      sig = (lambda1*J1+C12*J2+C13*J3)*M1.value()
           +(C12*J1+lambda2*J2+C23*J3)*M2.value()
           +(C13*J1+C23*J2+lambda3*J3)*M3.value()
           +2*(mu1*eps1+mu2*eps2+mu3*eps3);
    }
    
    // tangent
    if (second) {
      static SYM_TENSOR delta = SYM_TENSOR::identity();
      M = lambda1*outerProd(M1.value(),M1.value())
         +lambda2*outerProd(M2.value(),M2.value())
         +lambda3*outerProd(M3.value(),M3.value())
         +C12*(outerProd(M1.value(),M2.value())+outerProd(M2.value(),M1.value()))
         +C13*(outerProd(M1.value(),M3.value())+outerProd(M3.value(),M1.value()))
         +C23*(outerProd(M2.value(),M3.value())+outerProd(M3.value(),M2.value()));
      M.addIJKL(0.5*mu1,delta,M1.value());
      M.addIJKL(0.5*mu1,M1.value(),delta);
      M.addIJKL(0.5*mu2,delta,M2.value());
      M.addIJKL(0.5*mu2,M2.value(),delta);
      M.addIJKL(0.5*mu3,delta,M3.value());
      M.addIJKL(0.5*mu3,M3.value(),delta);
    }

    return W;
  }
  
  // compute material stiffness (Hooke) tensor
  void computeStiffness(const MaterialProperties& material,
                        const ParameterSet& extPar,SYM_TENSOR4& M) {
    
    // get elastic constants
    double lambda1 = material.getDoubleProperty("1ST_LAME_CONSTANT_1");
    double lambda2 = material.getDoubleProperty("1ST_LAME_CONSTANT_2");
    double lambda3 = material.getDoubleProperty("1ST_LAME_CONSTANT_3");
    double mu1 = material.getDoubleProperty("2ND_LAME_CONSTANT_1");
    double mu2 = material.getDoubleProperty("2ND_LAME_CONSTANT_2");
    double mu3 = material.getDoubleProperty("2ND_LAME_CONSTANT_3");
    double C12 = material.getDoubleProperty("C12_MODULUS");
    double C13 = material.getDoubleProperty("C13_MODULUS");
    double C23 = material.getDoubleProperty("C23_MODULUS");
    
    // get structural tensors
    StdProperty<SYM_TENSOR>& M1 =
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_1"));
    StdProperty<SYM_TENSOR>& M2 =
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_2"));
    StdProperty<SYM_TENSOR>& M3 =
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_3"));
    
    // stiffness
    static SYM_TENSOR delta = SYM_TENSOR::identity();
    M = lambda1*outerProd(M1.value(),M1.value())
       +lambda2*outerProd(M2.value(),M2.value())
       +lambda3*outerProd(M3.value(),M3.value())
       +C12*(outerProd(M1.value(),M2.value())+outerProd(M2.value(),M1.value()))
       +C13*(outerProd(M1.value(),M3.value())+outerProd(M3.value(),M1.value()))
       +C23*(outerProd(M2.value(),M3.value())+outerProd(M3.value(),M2.value()));
    M.addIJKL(0.5*mu1,delta,M1.value());
    M.addIJKL(0.5*mu1,M1.value(),delta);
    M.addIJKL(0.5*mu2,delta,M2.value());
    M.addIJKL(0.5*mu2,M2.value(),delta);
    M.addIJKL(0.5*mu3,delta,M3.value());
    M.addIJKL(0.5*mu3,M3.value(),delta);
  }
};


/**
 * Implementations of the model.
 */
class OrthotropicElasticity3D : public Elasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  OrthotropicElasticity3D()
  : Elasticity<TensorAlgebra3D>(new OrthotropicElasticPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  OrthotropicElasticity3D(const OrthotropicElasticity3D& src) 
  : Elasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~OrthotropicElasticity3D() {}
};
class OrthotropicElasticity2D : public Elasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  OrthotropicElasticity2D()
  : Elasticity<TensorAlgebra2D>(new OrthotropicElasticPotential<TensorAlgebra2D>()) {}
  
  // copy constructor
  OrthotropicElasticity2D(const OrthotropicElasticity2D& src) 
  : Elasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~OrthotropicElasticity2D() {}
};
class OrthotropicElasticity1D : public Elasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  OrthotropicElasticity1D()
  : Elasticity<TensorAlgebra1D>(new OrthotropicElasticPotential<TensorAlgebra1D>()) {}
  
  // copy constructor
  OrthotropicElasticity1D(const OrthotropicElasticity1D& src) 
  : Elasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~OrthotropicElasticity1D() {}
};

/**
 * The associated model builder
 */
class OrthotropicElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  OrthotropicElasticityBuilder();
  
  // the instance
  static OrthotropicElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~OrthotropicElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
