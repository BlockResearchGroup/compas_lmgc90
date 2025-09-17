/*
 *  $Id: OrthotropicLinThermalDilatancy.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ORTHOTROPIC_THERMAL_DILATANCY_H
#define ZORGLIB_MATL_MECA_LINEAR_ORTHOTROPIC_THERMAL_DILATANCY_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/linear/OrthotropicElasticPotential.h>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing orthotropic thermal dilatancy models.
 */
template <class ALG>
class OrthotropicLinThermalDilatancy : virtual public Elasticity<ALG>::Dilatancy {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  OrthotropicLinThermalDilatancy() {}
  
  // copy constructor
  OrthotropicLinThermalDilatancy(const OrthotropicLinThermalDilatancy&) {}
  
  // destructor
  virtual ~OrthotropicLinThermalDilatancy() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Orthotropic thermal dilatancy***" << std::endl;

    double alpha1,alpha2,alpha3,C11,C12,C22,C13,C23,C33,T0;
    // get dilatation coefficient
    try {
      alpha1 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_1");
      alpha2 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_2");
      alpha3 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_3");
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: one (or more) thermal dilatation coefficient is not defined." << std::endl;
      throw e;
    }

    // get elastic moduli
    try {
      C11 = material.getDoubleProperty("C11_MODULUS");
      C12 = material.getDoubleProperty("C12_MODULUS");
      C22 = material.getDoubleProperty("C22_MODULUS");
      C13 = material.getDoubleProperty("C13_MODULUS");
      C23 = material.getDoubleProperty("C23_MODULUS");
      C33 = material.getDoubleProperty("C33_MODULUS");
      // check positive-definiteness
      double det = C11*(C22*C33-C23*C23)-C12*(C12*C33-C13*C23)+C13*(C12*C23-C13*C22);
      if (det <= 0.0e0) {
        if (os) (*os) << "ERROR: Elasticity tensor must be positive-definite.\n";
        throw InvalidPropertyException("elastic moduli");
      }
    }
    catch (NoSuchPropertyException e) {
      // get elasticity coefficients
      double E1,E2,E3,nu12,nu13,nu23;
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
        throw InvalidPropertyException("Poisson's coefficient 12");
      }
      
      // store stiffness components
      material.setProperty("C11_MODULUS",C[0]);
      material.setProperty("C12_MODULUS",C[1]);
      material.setProperty("C22_MODULUS",C[2]);
      material.setProperty("C13_MODULUS",C[3]);
      material.setProperty("C23_MODULUS",C[4]);
      material.setProperty("C33_MODULUS",C[5]);
    }

    // get initial temperature
    try {
      T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: initial temperature is not defined." << std::endl;
      throw e;
    }
    
    if (os) {
      (*os) << "\tthermal dilatation coefficient 1 = " << alpha1 << std::endl;
      (*os) << "\tthermal dilatation coefficient 2 = " << alpha2 << std::endl;
      (*os) << "\tthermal dilatation coefficient 3 = " << alpha3 << std::endl;
      (*os) << "\tinitial temperature              = " << T0    << std::endl;
    }
     
     // initialize structural tensor
     if (   !material.checkProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_1")
         || !material.checkProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_2")
         || !material.checkProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_3")) {
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
     }
   }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties&,const Rotation&) {
    /* rotation of the structiral tensor is taken care of by elastic model */
  }
    
  // compute coupling energy
  double couplingEnergy(const MaterialProperties& material,
                        const ParameterSet& extPar,
                        const SYM_TENSOR& gam,SYM_TENSOR& sig,
                        SYM_TENSOR4& M,bool first,bool second) {
    
    // check for temperature
    if (!extPar.count("TEMPERATURE")) {
      sig = 0.0e0;
      M = 0.0e0;
      return 0.e0;
    }
    double T = extPar.find("TEMPERATURE")->second;
    
    // get material parameters
    double alpha1 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_1");
    double alpha2 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_2");
    double alpha3 = material.getDoubleProperty("THERMAL_DILATATION_COEFFICIENT_3");
    double C11 = material.getDoubleProperty("C11_MODULUS");
    double C12 = material.getDoubleProperty("C12_MODULUS");
    double C22 = material.getDoubleProperty("C22_MODULUS");
    double C13 = material.getDoubleProperty("C13_MODULUS");
    double C23 = material.getDoubleProperty("C23_MODULUS");
    double C33 = material.getDoubleProperty("C33_MODULUS");
    double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
    
    // transform engineering strains
    SYM_TENSOR eps = contravariant(gam);
    
    // get structural tensors and invariants
    StdProperty<SYM_TENSOR>& M1 =
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_1"));
    double J1 = innerProd2(M1.value(),eps);
    SYM_TENSOR eps1 = symProd(M1.value(),eps);
    StdProperty<SYM_TENSOR>& M2 =
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_2"));
    double J2 = innerProd2(M2.value(),eps);
    SYM_TENSOR eps2 = symProd(M2.value(),eps);
    StdProperty<SYM_TENSOR>& M3 =
      dynamic_cast< StdProperty<SYM_TENSOR>& >(material.getProperty("ORTHOTROPIC_STRUCTURAL_TENSOR_3"));
    double J3 = innerProd2(M3.value(),eps);
    SYM_TENSOR eps3 = symProd(M3.value(),eps);
    
    // compute coupling energy
    double coef0 = -(T-T0);
    double coef1 = alpha1*C11+alpha2*C12+alpha3*C13;
    double coef2 = alpha1*C12+alpha2*C22+alpha3*C23;
    double coef3 = alpha1*C13+alpha2*C23+alpha3*C33;
    double W = coef0*(coef1*J1+coef2*J2+coef3*J3);
    if (first) {
      sig = coef0*(coef1*M1.value()+coef2*M2.value()+coef3*M3.value());
    }
    if (second) M = 0.0e0;
    
    return W;
  }
};


/**
 * Implementations of the model.
 */
class OrthotropicThermoDilatantElasticity3D : public Elasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  OrthotropicThermoDilatantElasticity3D()
  : Elasticity<TensorAlgebra3D>(new OrthotropicElasticPotential<TensorAlgebra3D>(),
                                new OrthotropicLinThermalDilatancy<TensorAlgebra3D>()) {}
  
  // copy constructor
  OrthotropicThermoDilatantElasticity3D(const OrthotropicThermoDilatantElasticity3D& src) 
  : Elasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~OrthotropicThermoDilatantElasticity3D() {}
};
class OrthotropicThermoDilatantElasticity2D : public Elasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  OrthotropicThermoDilatantElasticity2D()
  : Elasticity<TensorAlgebra2D>(new OrthotropicElasticPotential<TensorAlgebra2D>(),
                                new OrthotropicLinThermalDilatancy<TensorAlgebra2D>()) {}
  
  // copy constructor
  OrthotropicThermoDilatantElasticity2D(const OrthotropicThermoDilatantElasticity2D& src) 
  : Elasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~OrthotropicThermoDilatantElasticity2D() {}
};
class OrthotropicThermoDilatantElasticity1D : public Elasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  OrthotropicThermoDilatantElasticity1D()
  : Elasticity<TensorAlgebra1D>(new OrthotropicElasticPotential<TensorAlgebra1D>(),
                                new OrthotropicLinThermalDilatancy<TensorAlgebra1D>()) {}
  
  // copy constructor
  OrthotropicThermoDilatantElasticity1D(const OrthotropicThermoDilatantElasticity1D& src) 
  : Elasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~OrthotropicThermoDilatantElasticity1D() {}
};

/**
 * The associated model builder
 */
class OrthotropicThermoDilatantElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  OrthotropicThermoDilatantElasticityBuilder();
  
  // the instance
  static OrthotropicThermoDilatantElasticityBuilder const* BUILDER;
  
public:
    
  // destructor
  virtual ~OrthotropicThermoDilatantElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
