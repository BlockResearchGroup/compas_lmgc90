/*
 *  $Id: YeohPotential.h 199 2016-03-10 20:35:29Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_YEOH_POTENTIAL_H
#define ZORGLIB_MATL_MECA_HYPER_YEOH_POTENTIAL_H

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
 * Class describing hyperelastic Yeoh potentials.
 */
template <class ALG>
class YeohPotential : virtual public SpectralHEPotential<ALG> {
  
 public:

  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

 public:

  // constructor
  YeohPotential() {}
  
  // copy constructor
  YeohPotential(const YeohPotential&) {}

  // destructor
  virtual ~YeohPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException) {
    if (os) (*os) << "\n\t***Yeoh potential***" << std::endl;
     
    double C10,C20,C30;

    // get moduli
    C10 = material.getDoubleProperty("C10_MODULUS");
    if (C10 < 0.e0) {
      if (os) (*os) << "ERROR: C10 modulus must be positive." << std::endl;
      throw InvalidPropertyException("c10 modulus");
    }
    C20 = material.getDoubleProperty("C20_MODULUS");
    C30 = material.getDoubleProperty("C30_MODULUS");
     
    if (os) {
      (*os) << "\tC10 modulus          = " << C10 << std::endl;
      (*os) << "\tC20 modulus          = " << C20 << std::endl;
      (*os) << "\tC30 modulus          = " << C30 << std::endl;
    }
  }

  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& C,SYM_TENSOR& S,
                      SYM_TENSOR4& M,bool first,bool second) {

    // model constants
    double C10 = material.getDoubleProperty("C10_MODULUS");
    double C20 = material.getDoubleProperty("C20_MODULUS");
    double C30 = material.getDoubleProperty("C30_MODULUS");
    
    // potential
    double I1 = trace(C);
    double val = I1 - 3.0;
    double val2 = val*val;
    double W = C10*val + C20*val2 + C30*val2*val;
    
    // stress tensor
    static const SYM_TENSOR I = SYM_TENSOR::identity();
    if (first) S = 2*(C10+2*C20*val+3*C30*val2)*I;
    
    // consistent tangent
    if (second) M = 4*(2*C20+6*C30*val)*outerProd(I,I);
    
    return W;
  }
  
  // compute stored energy from principal stretches
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const double eps[],double sig[],
                      double M[][3],bool first,bool second) {
    
    // model constants
    double C10 = material.getDoubleProperty("C10_MODULUS");
    double C20 = material.getDoubleProperty("C20_MODULUS");
    double C30 = material.getDoubleProperty("C30_MODULUS");
    
    // potential
    double I1  = eps[0]+eps[1]+eps[2];
    double val = I1 - 3.0;
    double val2 = val*val;
    double W = C10*val + C20*val2 + C30*val2*val;
    
    // stress tensor
    if (first) {
      double coef = C10+2*C20*val+3*C30*val2;
      sig[0] = coef;
      sig[1] = coef;
      sig[2] = coef;
    }

    // second derivatives
    if (second) {
      double coef = 2*C20+6*C30*val;
      M[0][0] = coef; M[0][1] = coef; M[0][2] = coef;
      M[1][0] = coef; M[1][1] = coef; M[1][2] = coef;
      M[2][0] = coef; M[2][1] = coef; M[2][2] = coef;
    }
    
    return W;
  }
};


/**
 * Implementations of the model.
 */
class Yeoh3D : public HyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  Yeoh3D()
  : HyperElasticity<TensorAlgebra3D>(new YeohPotential<TensorAlgebra3D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  Yeoh3D(const Yeoh3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~Yeoh3D() {}
};
class Yeoh2D : public HyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  Yeoh2D()
  : HyperElasticity<TensorAlgebra2D>(new YeohPotential<TensorAlgebra2D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  Yeoh2D(const Yeoh2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~Yeoh2D() {}
};
class Yeoh1D : public HyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  Yeoh1D()
  : HyperElasticity<TensorAlgebra1D>(new YeohPotential<TensorAlgebra1D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  Yeoh1D(const Yeoh1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~Yeoh1D() {}
};

/**
 * The associated model builder
 */
class YeohBuilder : public ModelBuilder {

 private:
  
  // constructor
  YeohBuilder();

  // the instance
  static YeohBuilder const* BUILDER;

 public:
  
  // destructor
  virtual ~YeohBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
