/*
 *  $Id: GentPotential.h 199 2016-03-10 20:35:29Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_GENT_POTENTIAL_H
#define ZORGLIB_MATL_MECA_HYPER_GENT_POTENTIAL_H

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
 * Class describing hyperelastic Gent (1996) potentials.
 */
template <class ALG>
class GentPotential : virtual public SpectralHEPotential<ALG> {
  
 public:

  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

 public:

  // constructor
  GentPotential() {}
  
  // copy constructor
  GentPotential(const GentPotential&) {}

  // destructor
  virtual ~GentPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Gent potential***" << std::endl;
     
    double mu,Im;

    // get Mu modulus
    mu = material.getDoubleProperty("SHEAR_MODULUS");
    if (mu < 0.e0) {
      if (os) (*os) << "ERROR: shear modulus must be positive." << std::endl;
      throw InvalidPropertyException("shear modulus");
    }

    // get Im coefficient
    Im = material.getDoubleProperty("FIRST_INVARIANT_LIMIT");
    if (Im < 3.e0) {
      if (os) (*os) << "ERROR: first invariant limit must be larger than 3." << std::endl;
      throw InvalidPropertyException("first invariant limit");
    }
     
    if (os) {
      (*os) << "\tshear modulus          = " << mu << std::endl;
      (*os) << "\tfirst invariant limit  = " << Im << std::endl;
    }
      
    // compute dilatational elastic wave speed
    if (os) (*os) << "\n\tcelerity is not defined" << std::endl;
  }

  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& C,SYM_TENSOR& S,
                      SYM_TENSOR4& M,bool first,bool second) {
    
    // model constants
    double mu = material.getDoubleProperty("SHEAR_MODULUS");
    double Im = material.getDoubleProperty("FIRST_INVARIANT_LIMIT");
    double Jm = Im - 3.0;
    
    // potential
    double I1  = trace(C);
    double val = Im-I1;
    double W = -0.5*mu*Jm*std::log(val/Jm);
    
    // stress tensor
    static const SYM_TENSOR I = SYM_TENSOR::identity();
    if (first) S = (mu*Jm/val)*I;

    // consistent tangent
    if (second) M = (2*mu*Jm/(val*val))*outerProd(I,I);
    
    return W;
  }
  
  // compute stored energy from principal stretches
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const double eps[],double sig[],
                      double M[][3],bool first,bool second) {
    
    // model constants
    double mu = material.getDoubleProperty("SHEAR_MODULUS");
    double Im = material.getDoubleProperty("FIRST_INVARIANT_LIMIT");
    double Jm = Im - 3.0;
    
    // potential
    double I1  = eps[0]+eps[1]+eps[2];
    double val = Im-I1;
    double W = -0.5*mu*Jm*std::log(val/Jm);

    // stress tensor
    if (first) {
      double coef = 0.5*mu*Jm/val;
      sig[0] = coef;
      sig[1] = coef;
      sig[2] = coef;
    }

    // second derivatives
    if (second) {
      double coef = 0.5*mu*Jm/(val*val);
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
class Gent3D : public HyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  Gent3D()
  : HyperElasticity<TensorAlgebra3D>(new GentPotential<TensorAlgebra3D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  Gent3D(const Gent3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~Gent3D() {}
};
class Gent2D : public HyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  Gent2D()
  : HyperElasticity<TensorAlgebra2D>(new GentPotential<TensorAlgebra2D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  Gent2D(const Gent2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~Gent2D() {}
};
class Gent1D : public HyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  Gent1D()
  : HyperElasticity<TensorAlgebra1D>(new GentPotential<TensorAlgebra1D>(),
                                     new StandardEOS()) {}
  
  // copy constructor
  Gent1D(const Gent1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~Gent1D() {}
};

/**
 * The associated model builder
 */
class GentBuilder : public ModelBuilder {

 private:
  
  // constructor
  GentBuilder();

  // the instance
  static GentBuilder const* BUILDER;

 public:
  
  // destructor
  virtual ~GentBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
