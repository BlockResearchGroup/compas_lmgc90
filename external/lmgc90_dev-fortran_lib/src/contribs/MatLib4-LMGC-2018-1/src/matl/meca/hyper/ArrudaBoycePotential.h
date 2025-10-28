/*
 *  $Id: ArrudaBoycePotential.h 200 2016-03-10 20:55:58Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_ARRUDABOYCE_POTENTIAL_H
#define ZORGLIB_MATL_MECA_HYPER_ARRUDABOYCE_POTENTIAL_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
// local
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/meca/eos/StandardEOS.h>
#include <matl/meca/hyper/HyperElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing hyperelastic Arruda-Boyce potentials.
 */
template <class ALG>
class ArrudaBoycePotential : virtual public SpectralHEPotential<ALG> {
  
 public:

  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

 public:

  // constructor
  ArrudaBoycePotential() {}
  
  // copy constructor
  ArrudaBoycePotential(const ArrudaBoycePotential&) {}

  // destructor
  virtual ~ArrudaBoycePotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException) {
    if (os) (*os) << "\n\t***Arruda-Boyce potential***" << std::endl;

    // get shear modulus
    double mu = material.getDoubleProperty("SHEAR_MODULUS");
    if (mu < 0.e0) {
      if (os) (*os) << "ERROR: shear modulus must be positive." << std::endl;
      throw InvalidPropertyException("shear modulus");
    }

    // get number of chains
    double lambda,N;
    try {
      N = material.getDoubleProperty("NUMBER_OF_SEGMENTS");
      if (N <= 0.e0) {
        if (os) (*os) << "ERROR: number of segments must be strictly positive." << std::endl;
        throw InvalidPropertyException("number of segments");
      }
      lambda = std::sqrt(N);
    }
    catch (NoSuchPropertyException) {
      lambda = material.getDoubleProperty("LOCKING_STRETCH");
      if (N <= 0.e0) {
        if (os) (*os) << "ERROR: locking stretch must be strictly positive." << std::endl;
        throw InvalidPropertyException("locking stretch");
      }
      N = lambda*lambda;
    }
     
    if (os) {
      (*os) << "\tshear modulus      = " << mu      << std::endl;
      (*os) << "\tlocking stretch    = " << lambda  << std::endl;
      (*os) << "\tnumber of segments = " << N       << std::endl;
    }
  }

  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& C,SYM_TENSOR& S,
                      SYM_TENSOR4& M,bool first,bool second) {

    // model constants
    double mu = material.getDoubleProperty("SHEAR_MODULUS");
    double N = material.getDoubleProperty("NUMBER_OF_SEGMENTS");
    
    // series expansion coefficient
    double N2 = N*N;
    double SEC1 = 0.5;
    double SEC2 = 1./(20.*N);
    double SEC3 = 11./(1050.*N2);
    double SEC4 = 19./(7000.*N*N2);
    double SEC5 = 519./(673750.*N2*N2);
    
    // first invariant
    double I1 = trace(C);
    
    // potential
    double I1_2 = I1*I1;
    double W = mu * ( SEC1 * (I1 - 3.0) 
                    + SEC2 * (I1_2 - 9.0)
                    + SEC3 * (I1*I1_2 - 27.0)
                    + SEC4 * (I1_2*I1_2 - 81.0)
                    + SEC5 * (I1_2*I1_2*I1 - 243.0));
    
    // stress tensor
    static const SYM_TENSOR I = SYM_TENSOR::identity();
    if (first) S = 2.0*mu*(SEC1 + 2*SEC2*I1 + 3*SEC3*I1_2
                           + 4*SEC4*I1*I1_2 + 5*SEC5*I1_2*I1_2)*I;
    
    
    // consistent tangent
    if (second) {
      double val = 2*SEC2+6*SEC3*I1+12*SEC4*I1_2+20*SEC5*I1*I1_2;
      M = (4.0*mu*val)*outerProd(I,I);
    }
    
    return W;
  }
  
  // compute stored energy from principal stretches
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const double eps[],double sig[],
                      double M[][3],bool first,bool second) {
    
    // model constants
    double mu = material.getDoubleProperty("SHEAR_MODULUS");
    double N = material.getDoubleProperty("NUMBER_OF_SEGMENTS");
    
    // series expansion coefficient
    double N2 = N*N;
    double SEC1 = 0.5;
    double SEC2 = 1./(20.*N);
    double SEC3 = 11./(1050.*N2);
    double SEC4 = 19./(7000.*N*N2);
    double SEC5 = 519./(673750.*N2*N2);

    // first invariant
    double I1  = eps[0]+eps[1]+eps[2];
    
    // potential
    double I1_2 = I1*I1;
    double W = mu * ( SEC1 * (I1 - 3.0)
                     + SEC2 * (I1_2 - 9.0)
                     + SEC3 * (I1*I1_2 - 27.0)
                     + SEC4 * (I1_2*I1_2 - 81.0)
                     + SEC5 * (I1_2*I1_2*I1 - 243.0));
    
    // stress tensor
    if (first) {
      double val = mu*(SEC1 + 2*SEC2*I1 + 3*SEC3*I1_2
                      + 4*SEC4*I1*I1_2 + 5*SEC5*I1_2*I1_2);
      sig[0] = val;
      sig[1] = val;
      sig[2] = val;
    }

    // second derivatives
    if (second) {
      double val = mu*(2*SEC2+6*SEC3*I1+12*SEC4*I1_2+20*SEC5*I1*I1_2);
      M[0][0] = val; M[0][1] = val; M[0][2] = val;
      M[1][0] = val; M[1][1] = val; M[1][2] = val;
      M[2][0] = val; M[2][1] = val; M[2][2] = val;
    }
    
    return W;
  }
};


/**
 * Implementations of the model.
 */
class ArrudaBoyce3D : public HyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  ArrudaBoyce3D()
  : HyperElasticity<TensorAlgebra3D>(new ArrudaBoycePotential<TensorAlgebra3D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  ArrudaBoyce3D(const ArrudaBoyce3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~ArrudaBoyce3D() {}
};
class ArrudaBoyce2D : public HyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  ArrudaBoyce2D()
  : HyperElasticity<TensorAlgebra2D>(new ArrudaBoycePotential<TensorAlgebra2D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  ArrudaBoyce2D(const ArrudaBoyce2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~ArrudaBoyce2D() {}
};
class ArrudaBoyce1D : public HyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  ArrudaBoyce1D()
  : HyperElasticity<TensorAlgebra1D>(new ArrudaBoycePotential<TensorAlgebra1D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  ArrudaBoyce1D(const ArrudaBoyce1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~ArrudaBoyce1D() {}
};

/**
 * The associated model builder
 */
class ArrudaBoyceBuilder : public ModelBuilder {

 private:
  
  // constructor
  ArrudaBoyceBuilder();

  // the instance
  static ArrudaBoyceBuilder const* BUILDER;

 public:
  
  // destructor
  virtual ~ArrudaBoyceBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
