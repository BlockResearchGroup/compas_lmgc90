/*
 *  $Id: IsotropicLinDilatancy.h 169 2015-08-10 09:34:30Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFF_MECA_ISOTROPIC_LINEAR_DILATANCY_H
#define ZORGLIB_MATL_DIFF_MECA_ISOTROPIC_LINEAR_DILATANCY_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/linear/IsotropicElasticPotential.h>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing isotropic thermal dilatancy models.
 */
template <class ALG>
class IsotropicLinDilatancy : virtual public Elasticity<ALG>::Dilatancy {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  IsotropicLinDilatancy() {}
  
  // copy constructor
  IsotropicLinDilatancy(const IsotropicLinDilatancy&) {}
  
  // destructor
  virtual ~IsotropicLinDilatancy() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic linear dilatancy***" << std::endl;

    double alpha,K,c0;
    // get dilatation coefficient
    try {
      alpha = material.getDoubleProperty("CHEMICAL_DILATATION_COEFFICIENT");
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: chemical dilatation coefficient is not defined." << std::endl;
      throw e;
    }
    // get bulk modulus
    try {
      K = material.getDoubleProperty("BULK_MODULUS");
      if (K < 0.0e0) {
        if (os) (*os) << "ERROR: bulk modulus must be positive." << std::endl;
        throw InvalidPropertyException("bulk modulus");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: bulk modulus is not defined." << std::endl;
      throw e;
    }
    // get initial temperature
    try {
      c0 = material.getDoubleProperty("INITIAL_CONCENTRATION");
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: initial concentration is not defined." << std::endl;
      throw e;
    }
    
    if (os) {
      (*os) << "\tchemical dilatation coefficient = " << alpha << std::endl;
      (*os) << "\tbulk modulus                    = " << K     << std::endl;
      (*os) << "\tinitial concentration           = " << c0    << std::endl;
    }
  }
  
  // compute coupling energy
  double couplingEnergy(const MaterialProperties& material,
                        const ParameterSet& extPar,
                        const SYM_TENSOR& eps,SYM_TENSOR& sig,
                        SYM_TENSOR4& M,bool first,bool second) {
    
    // check for temperature
    if (!extPar.count("CONCENTRATION")) {
      sig = 0.0e0;
      M = 0.0e0;
      return 0.e0;
    }
    double c = extPar.find("CONCENTRATION")->second;
    
    // get material parameters
    double alpha = material.getDoubleProperty("CHEMICAL_DILATATION_COEFFICIENT");
    double K = material.getDoubleProperty("BULK_MODULUS");
    double c0 = material.getDoubleProperty("INITIAL_CONCENTRATION");

    // compute coupling energy
    double tr = trace(eps);
    double coef = -3*K*alpha*(c-c0);
    double W = coef*tr;
    if (first) {
      static const SYM_TENSOR delta = SYM_TENSOR::identity();
      sig = coef*delta;
    }
    if (second) M = 0.0e0;
    
    return W;
  }
};


/**
 * Implementations of the model.
 */
class IsotropicDilatantElasticity3D : public Elasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicDilatantElasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>(),
                                new IsotropicLinDilatancy<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicDilatantElasticity3D(const IsotropicDilatantElasticity3D& src)
  : Elasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicDilatantElasticity3D() {}
};
class IsotropicDilatantElasticity2D : public Elasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicDilatantElasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>(),
                                new IsotropicLinDilatancy<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicDilatantElasticity2D(const IsotropicDilatantElasticity2D& src)
  : Elasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicDilatantElasticity2D() {}
};
class IsotropicDilatantElasticity1D : public Elasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicDilatantElasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>(),
                                new IsotropicLinDilatancy<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicDilatantElasticity1D(const IsotropicDilatantElasticity1D& src)
  : Elasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicDilatantElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicDilatantElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicDilatantElasticityBuilder();
  
  // the instance
  static IsotropicDilatantElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicDilatantElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
