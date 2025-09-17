/*
 *  $Id$
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2020, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ORTHOTROPIC_VISCOUS_POTENTIAL_H
#define ZORGLIB_MATL_MECA_LINEAR_ORTHOTROPIC_VISCOUS_POTENTIAL_H

// config
#include <matlib_macros.h>

// std C++ library
#include <limits>
// local
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/meca/linear/OrthotropicElasticPotential.h>
#include <matl/meca/linear/OrthotropicLinThermalDilatancy.h>
#include <matl/meca/linear/ViscoElasticity.h>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing orthotropic viscous potentials.
 */
template <class ALG>
class OrthotropicViscousPotential : virtual public ViscoElasticity<ALG>::ViscousPotential {
  
 public:
  
  // define new types
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
    
  // constructor
  OrthotropicViscousPotential() {}
  
  // copy constructor
  OrthotropicViscousPotential(const OrthotropicViscousPotential&) {}
  
  // destructor
  virtual ~OrthotropicViscousPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Orthotropic viscous potential***" << std::endl;

    unsigned int nDim = ALG::DIMENSION;
    double E1,E2,E3,G12=0.e0,G13=0.e0,G23=0.e0,nu12,nu13,nu23;

    // get elasticity coefficients
    E1 = material.getDoubleProperty("VISCOUS_YOUNG_MODULUS_1");
    if (E1 < 0.e0) {
      if (os) (*os) << "ERROR: viscous moduli must be positive.\n";
      throw InvalidPropertyException("Ev1 modulus");
    }
    E2 = material.getDoubleProperty("VISCOUS_YOUNG_MODULUS_2");
    if (E2 < 0.e0) {
      if (os) (*os) << "ERROR: viscous moduli must be positive.\n";
      throw InvalidPropertyException("Ev2 modulus");
    }
    E3 = material.getDoubleProperty("VISCOUS_YOUNG_MODULUS_3");
    if (E3 < 0.e0) {
      if (os) (*os) << "ERROR: viscous moduli must be positive.\n";
      throw InvalidPropertyException("Ev3 modulus");
    }

    nu12 = material.getDoubleProperty("VISCOUS_POISSON_COEFFICIENT_12");
    nu13 = material.getDoubleProperty("VISCOUS_POISSON_COEFFICIENT_13");
    nu23 = material.getDoubleProperty("VISCOUS_POISSON_COEFFICIENT_23");
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
      if (os) (*os) << "ERROR: viscosity tensor must be positive-definite.\n";
      throw InvalidPropertyException("viscous moduli");
    }

    // store stiffness components
    material.setProperty("VISCOUS_C11_MODULUS",C[0]);
    material.setProperty("VISCOUS_C12_MODULUS",C[1]);
    material.setProperty("VISCOUS_C22_MODULUS",C[2]);
    material.setProperty("VISCOUS_C13_MODULUS",C[3]);
    material.setProperty("VISCOUS_C23_MODULUS",C[4]);
    material.setProperty("VISCOUS_C33_MODULUS",C[5]);

    if (nDim >= 2) {
      G12 = material.getDoubleProperty("VISCOUS_SHEAR_MODULUS_12");
      if (G12 < 0.e0) {
        if (os) (*os) << "ERROR: viscous moduli must be positive.\n";
        throw InvalidPropertyException("viscous G12 modulus");
      }
      material.setProperty("VISCOUS_C44_MODULUS",G12);
    }
    if (nDim >= 3) {
      G13 = material.getDoubleProperty("VISCOUS_SHEAR_MODULUS_13");
      if (G13 < 0.e0) {
        if (os) (*os) << "ERROR: viscous moduli must be positive.\n";
        throw InvalidPropertyException("viscous G13 modulus");
      }
      material.setProperty("VISCOUS_C55_MODULUS",G13);
      G23 = material.getDoubleProperty("VISCOUS_SHEAR_MODULUS_23");
      if (G23 < 0.e0) {
        if (os) (*os) << "ERROR: viscous moduli must be positive.\n";
        throw InvalidPropertyException("viscous G23 modulus");
      }
      material.setProperty("VISCOUS_C66_MODULUS",G23);
    }
    
    // compute alternate coefficients
    double lambda1,lambda2,lambda3,mu1,mu2,mu3;
    mu1 =  G12+G13-G23;
    mu2 =  G12-G13+G23;
    mu3 = -G12+G13+G23;
    lambda1 = C[0]-2*mu1;
    lambda2 = C[2]-2*mu2;
    lambda3 = C[5]-2*mu3;
    material.setProperty("VISCOUS_1ST_LAME_CONSTANT_1",lambda1);
    material.setProperty("VISCOUS_1ST_LAME_CONSTANT_2",lambda2);
    material.setProperty("VISCOUS_1ST_LAME_CONSTANT_3",lambda3);
    material.setProperty("VISCOUS_2ND_LAME_CONSTANT_1",mu1);
    material.setProperty("VISCOUS_2ND_LAME_CONSTANT_2",mu2);
    material.setProperty("VISCOUS_2ND_LAME_CONSTANT_3",mu3);

    // print-out
    if (os) {
      (*os) << "\tviscous Young's modulus 1        = " << E1 << std::endl;
      (*os) << "\tviscous Young's modulus 2        = " << E2 << std::endl;
      (*os) << "\tviscous Young's modulus 3        = " << E3 << std::endl;
      (*os) << "\tviscous Poisson's coefficient 12 = " << nu12 << std::endl;
      (*os) << "\tviscous Poisson's coefficient 13 = " << nu13 << std::endl;
      (*os) << "\tviscous Poisson's coefficient 23 = " << nu23 << std::endl;
      if (nDim >= 2) {
        (*os) << "\tviscous shear modulus         12 = " <<  G12 << std::endl;
      }
      if (nDim >= 3) {
        (*os) << "\tviscous shear modulus         13 = " <<  G13 << std::endl;
        (*os) << "\tviscous shear modulus         23 = " <<  G23 << std::endl;
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
    material.setProperty("VISCOUS_ORTHOTROPIC_STRUCTURAL_TENSOR_1",MProp1);
    material.setProperty("VISCOUS_ORTHOTROPIC_STRUCTURAL_TENSOR_2",MProp2);
    material.setProperty("VISCOUS_ORTHOTROPIC_STRUCTURAL_TENSOR_3",MProp3);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    
    // get structural tensors
    StdProperty<SYM_TENSOR>& MProp1 = 
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("VISCOUS_ORTHOTROPIC_STRUCTURAL_TENSOR_1"));
    SYM_TENSOR& M1 = MProp1.value();
    StdProperty<SYM_TENSOR>& MProp2 = 
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("VISCOUS_ORTHOTROPIC_STRUCTURAL_TENSOR_2"));
    SYM_TENSOR& M2 = MProp2.value();
    StdProperty<SYM_TENSOR>& MProp3 = 
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("VISCOUS_ORTHOTROPIC_STRUCTURAL_TENSOR_3"));
    SYM_TENSOR& M3 = MProp3.value();
    
    // rotate structural tensors
    TENSOR R0;
    R.toTensor(R0);
    M1 = contravariantPush(M1,R0);
    M2 = contravariantPush(M2,R0);
    M3 = contravariantPush(M3,R0);
  }
  
  // compute dissipated energy
  double dissipatedEnergy(const MaterialProperties& material,const ParameterSet& extPar,
                          const SYM_TENSOR& gam,const SYM_TENSOR& gamDot,
                          SYM_TENSOR& sig1,SYM_TENSOR& sig2,
                          SYM_TENSOR4& M11,SYM_TENSOR4& M22,
                          SYM_TENSOR4& M12,double dTime,bool first,bool second) {

    // get elastic constants
    double lambda1 = material.getDoubleProperty("VISCOUS_1ST_LAME_CONSTANT_1");
    double lambda2 = material.getDoubleProperty("VISCOUS_1ST_LAME_CONSTANT_2");
    double lambda3 = material.getDoubleProperty("VISCOUS_1ST_LAME_CONSTANT_3");
    double mu1 = material.getDoubleProperty("VISCOUS_2ND_LAME_CONSTANT_1");
    double mu2 = material.getDoubleProperty("VISCOUS_2ND_LAME_CONSTANT_2");
    double mu3 = material.getDoubleProperty("VISCOUS_2ND_LAME_CONSTANT_3");
    double C12 = material.getDoubleProperty("VISCOUS_C12_MODULUS");
    double C13 = material.getDoubleProperty("VISCOUS_C13_MODULUS");
    double C23 = material.getDoubleProperty("VISCOUS_C23_MODULUS");

    // transform engineering strain rates
    SYM_TENSOR epsDot = contravariant(gamDot);

    // get structural tensors and invariants
    StdProperty<SYM_TENSOR>& M1 =
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("VISCOUS_ORTHOTROPIC_STRUCTURAL_TENSOR_1"));
    double J1 = innerProd2(M1.value(),epsDot);
    SYM_TENSOR epsDot1 = symProd(M1.value(),epsDot);
    double J4 = innerProd2(epsDot,epsDot1);
    StdProperty<SYM_TENSOR>& M2 =
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("VISCOUS_ORTHOTROPIC_STRUCTURAL_TENSOR_2"));
    double J2 = innerProd2(M2.value(),epsDot);
    SYM_TENSOR epsDot2 = symProd(M2.value(),epsDot);
    double J5 = innerProd2(epsDot,epsDot2);
    StdProperty<SYM_TENSOR>& M3 =
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("VISCOUS_ORTHOTROPIC_STRUCTURAL_TENSOR_3"));
    double J3 = innerProd2(M3.value(),epsDot);
    SYM_TENSOR epsDot3 = symProd(M3.value(),epsDot);
    double J6 = innerProd2(epsDot,epsDot3);

    // potential
    double Wv = 0.5*(lambda1*J1*J1+lambda2*J2*J2+lambda3*J3*J3)
               +C12*J1*J2+C13*J1*J3+C23*J2*J3+mu1*J4+mu2*J5+mu3*J6;
    if (!first && !second) return Wv;
    
    // stress
    if (first) {
      sig1 = 0.0e0;
      sig2 = (lambda1*J1+C12*J2+C13*J3)*M1.value()
            +(C12*J1+lambda2*J2+C23*J3)*M2.value()
            +(C13*J1+C23*J2+lambda3*J3)*M3.value()
            +2*(mu1*epsDot1+mu2*epsDot2+mu3*epsDot3);
    }
    
    // tangent
    if (second) {
      static SYM_TENSOR delta = SYM_TENSOR::identity();
      M11 = 0.0e0;
      M12 = 0.0e0;
      M22 = lambda1*outerProd(M1.value(),M1.value())
           +lambda2*outerProd(M2.value(),M2.value())
           +lambda3*outerProd(M3.value(),M3.value())
           +C12*(outerProd(M1.value(),M2.value())+outerProd(M2.value(),M1.value()))
           +C13*(outerProd(M1.value(),M3.value())+outerProd(M3.value(),M1.value()))
           +C23*(outerProd(M2.value(),M3.value())+outerProd(M3.value(),M2.value()));
      M22.addIJKL(0.5*mu1,delta,M1.value());
      M22.addIJKL(0.5*mu1,M1.value(),delta);
      M22.addIJKL(0.5*mu2,delta,M2.value());
      M22.addIJKL(0.5*mu2,M2.value(),delta);
      M22.addIJKL(0.5*mu3,delta,M3.value());
      M22.addIJKL(0.5*mu3,M3.value(),delta);
    }

    return Wv;
  }
};


/**
 * Implementations of the model.
 */
class OrthotropicKelvinViscoElasticity3D : public ViscoElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  OrthotropicKelvinViscoElasticity3D()
  : Elasticity<TensorAlgebra3D>(new OrthotropicElasticPotential<TensorAlgebra3D>()),
    ViscoElasticity<TensorAlgebra3D>(new OrthotropicViscousPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  OrthotropicKelvinViscoElasticity3D(const OrthotropicKelvinViscoElasticity3D& src)
  : Elasticity<TensorAlgebra3D>(src), ViscoElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~OrthotropicKelvinViscoElasticity3D() {}
};
class OrthotropicKelvinViscoElasticity2D : public ViscoElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  OrthotropicKelvinViscoElasticity2D()
  : Elasticity<TensorAlgebra2D>(new OrthotropicElasticPotential<TensorAlgebra2D>()),
    ViscoElasticity<TensorAlgebra2D>(new OrthotropicViscousPotential<TensorAlgebra2D>()){}
  
  // copy constructor
  OrthotropicKelvinViscoElasticity2D(const OrthotropicKelvinViscoElasticity2D& src)
  : Elasticity<TensorAlgebra2D>(src), ViscoElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~OrthotropicKelvinViscoElasticity2D() {}
};
class OrthotropicKelvinViscoElasticity1D : public ViscoElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  OrthotropicKelvinViscoElasticity1D()
  : Elasticity<TensorAlgebra1D>(new OrthotropicElasticPotential<TensorAlgebra1D>()),
    ViscoElasticity<TensorAlgebra1D>(new OrthotropicViscousPotential<TensorAlgebra1D>()){}
  
  // copy constructor
  OrthotropicKelvinViscoElasticity1D(const OrthotropicKelvinViscoElasticity1D& src)
  : Elasticity<TensorAlgebra1D>(src), ViscoElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~OrthotropicKelvinViscoElasticity1D() {}
};

/**
 * The associated model builder
 */
class OrthotropicKelvinViscoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  OrthotropicKelvinViscoElasticityBuilder();
  
  // the instance
  static OrthotropicKelvinViscoElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~OrthotropicKelvinViscoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};
      
      
/**
 * Implementations of the model.
 */
class OrthotropicThermoDilatantKelvinViscoElasticity3D : public ViscoElasticity<TensorAlgebra3D> {

 public:

  // constructor
  OrthotropicThermoDilatantKelvinViscoElasticity3D()
  : Elasticity<TensorAlgebra3D>(new OrthotropicElasticPotential<TensorAlgebra3D>(),
                                new OrthotropicLinThermalDilatancy<TensorAlgebra3D>()),
    ViscoElasticity<TensorAlgebra3D>(new OrthotropicViscousPotential<TensorAlgebra3D>()) {}

  // copy constructor
  OrthotropicThermoDilatantKelvinViscoElasticity3D(const OrthotropicThermoDilatantKelvinViscoElasticity3D& src)
  : Elasticity<TensorAlgebra3D>(src), ViscoElasticity<TensorAlgebra3D>(src) {}

  // destructor
  virtual ~OrthotropicThermoDilatantKelvinViscoElasticity3D() {}
};
class OrthotropicThermoDilatantKelvinViscoElasticity2D : public ViscoElasticity<TensorAlgebra2D> {

 public:

  // constructor
  OrthotropicThermoDilatantKelvinViscoElasticity2D()
  : Elasticity<TensorAlgebra2D>(new OrthotropicElasticPotential<TensorAlgebra2D>(),
                                new OrthotropicLinThermalDilatancy<TensorAlgebra2D>()),
    ViscoElasticity<TensorAlgebra2D>(new OrthotropicViscousPotential<TensorAlgebra2D>()) {}

  // copy constructor
  OrthotropicThermoDilatantKelvinViscoElasticity2D(const OrthotropicThermoDilatantKelvinViscoElasticity2D& src)
  : Elasticity<TensorAlgebra2D>(src), ViscoElasticity<TensorAlgebra2D>(src) {}

  // destructor
  virtual ~OrthotropicThermoDilatantKelvinViscoElasticity2D() {}
};
class OrthotropicThermoDilatantKelvinViscoElasticity1D : public ViscoElasticity<TensorAlgebra1D> {

 public:

  // constructor
  OrthotropicThermoDilatantKelvinViscoElasticity1D()
  : Elasticity<TensorAlgebra1D>(new OrthotropicElasticPotential<TensorAlgebra1D>(),
                                new OrthotropicLinThermalDilatancy<TensorAlgebra1D>()),
    ViscoElasticity<TensorAlgebra1D>(new OrthotropicViscousPotential<TensorAlgebra1D>()) {}

  // copy constructor
  OrthotropicThermoDilatantKelvinViscoElasticity1D(const OrthotropicThermoDilatantKelvinViscoElasticity1D& src)
  : Elasticity<TensorAlgebra1D>(src), ViscoElasticity<TensorAlgebra1D>(src) {}

  // destructor
  virtual ~OrthotropicThermoDilatantKelvinViscoElasticity1D() {}
};

/**
 * The associated model builder
 */
class OrthotropicThermoDilatantKelvinViscoElasticityBuilder : public ModelBuilder {

 private:

  // constructor
  OrthotropicThermoDilatantKelvinViscoElasticityBuilder();

  // the instance
  static OrthotropicThermoDilatantKelvinViscoElasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~OrthotropicThermoDilatantKelvinViscoElasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
