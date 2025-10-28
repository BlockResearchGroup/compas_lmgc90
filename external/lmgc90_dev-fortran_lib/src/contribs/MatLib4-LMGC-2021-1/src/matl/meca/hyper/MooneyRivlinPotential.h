/*
 *  $Id: MooneyRivlinPotential.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_MOONEY_RIVLIN_POTENTIAL_H
#define ZORGLIB_MATL_MECA_HYPER_MOONEY_RIVLIN_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/meca/eos/StandardEOS.h>
#include <matl/meca/hyper/HyperElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing hyperelastic Mooney-Rivlin potentials.
 */
template <class ALG>
class MooneyRivlinPotential : virtual public SpectralHEPotential<ALG> {
  
 public:

  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

 public:

  // constructor
  MooneyRivlinPotential() {}
  
  // copy constructor
  MooneyRivlinPotential(const MooneyRivlinPotential&) {}

  // destructor
  virtual ~MooneyRivlinPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Mooney-Rivlin potential***" << std::endl;
    
    double C1,C2; 
    // get C1 modulus
    C1 = material.getDoubleProperty("C10_MODULUS");
    if (C1 < 0.e0) {
      if (os) (*os) << "ERROR: C10 modulus must be positive." << std::endl;
      throw InvalidPropertyException("C10 modulus");
    }

    // get C2 modulus
    C2 = material.getDoubleProperty("C01_MODULUS");
    if (C2 < 0.e0) {
      if (os) (*os) << "ERROR: C01 modulus must be positive." << std::endl;
      throw InvalidPropertyException("C01 modulus");
    }
     
    if (os) {
      (*os) << "\tC10 modulus       = " << C1 << std::endl;
      (*os) << "\tC01 modulus       = " << C2 << std::endl;
    }
    
    // compute dilatational elastic wave speed
    if (os) (*os) << "\n\tcelerity is not defined" << std::endl;
  }
  
  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& C,SYM_TENSOR& S,
                      SYM_TENSOR4& M,bool first,bool second) {
    
    // constants
    double C1 = material.getDoubleProperty("C10_MODULUS");
    double C2 = material.getDoubleProperty("C01_MODULUS");
    
    // invariants
    double I1 = trace(C);
    double I2 = 0.5*(I1*I1-innerProd2(C,C));

    // potential
    double W = C1*(I1-3.0)+C2*(I2-3.0);
    
    // stress tensor
    static const SYM_TENSOR I = SYM_TENSOR::identity();
    if (first) S = 2*((C1+C2*I1)*I-C2*C);
    
    // consistent tangent
    if (second) M = 4*C2*(outerProd(I,I)-SYM_TENSOR4::contravariantIdentity());
    
    return W;
  }
  
  // compute stored energy from principal stretches
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const double eps[],double sig[],
                      double M[][3],bool first,bool second) {
 
    // constants
    double C1 = material.getDoubleProperty("C10_MODULUS");
    double C2 = material.getDoubleProperty("C01_MODULUS");
    
    // invariants
    double I1 = eps[0]+eps[1]+eps[2];
    double I2 = eps[0]*eps[1]+eps[0]*eps[2]+eps[1]*eps[2];

    // potential
    double W = C1*(I1-3.0)+C2*(I2-3.0);
    
    // stress tensor
    if (first) {
      sig[0] = C1+C2*(eps[1]+eps[2]);
      sig[1] = C1+C2*(eps[0]+eps[2]);
      sig[2] = C1+C2*(eps[0]+eps[1]);
    }
    
    // consistent tangent
    if (second) {
      M[0][0] = 0.0e0; M[0][1] = C2;    M[0][2] = C2;
      M[1][0] = C2;    M[1][1] = 0.0e0; M[1][2] = C2;
      M[2][0] = C2;    M[2][1] = C2;    M[2][2] = 0.0e0;
    }
    
    return W;
  }
};


/**
 * Implementations of the model.
 */
class MooneyRivlin3D : public HyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  MooneyRivlin3D()
  : HyperElasticity<TensorAlgebra3D>(new MooneyRivlinPotential<TensorAlgebra3D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  MooneyRivlin3D(const MooneyRivlin3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~MooneyRivlin3D() {}
};
class MooneyRivlin2D : public HyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  MooneyRivlin2D()
  : HyperElasticity<TensorAlgebra2D>(new MooneyRivlinPotential<TensorAlgebra2D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  MooneyRivlin2D(const MooneyRivlin2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~MooneyRivlin2D() {}
};
class MooneyRivlin1D : public HyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  MooneyRivlin1D()
  : HyperElasticity<TensorAlgebra1D>(new MooneyRivlinPotential<TensorAlgebra1D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  MooneyRivlin1D(const MooneyRivlin1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~MooneyRivlin1D() {}
};

/**
 * The associated model builder
 */
class MooneyRivlinBuilder : public ModelBuilder {

 private:
  
  // constructor
  MooneyRivlinBuilder();

  // the instance
  static MooneyRivlinBuilder const* BUILDER;

 public:
  
  // destructor
  virtual ~MooneyRivlinBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
