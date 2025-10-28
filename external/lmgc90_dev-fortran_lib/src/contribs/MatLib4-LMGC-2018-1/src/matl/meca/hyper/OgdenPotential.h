/*
 *  $Id: OgdenPotential.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_OGDEN_POTENTIAL_H
#define ZORGLIB_MATL_MECA_HYPER_OGDEN_POTENTIAL_H

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
 * Class describing hyperelastic Ogden potentials.
 */
template <class ALG>
class OgdenPotential : virtual public SpectralHEPotential<ALG> {

 public:

  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

 public:

  // constructor
  OgdenPotential() {}

  // copy constructor
  OgdenPotential(const OgdenPotential&) {}

  // destructor
  virtual ~OgdenPotential() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Ogden potential***" << std::endl;

    double alpha1,alpha2,alpha3;
    double mu1,mu2,mu3;

    // get alpha exponents
    alpha1 = material.getDoubleProperty("OGDEN_ALPHA1");
    alpha2 = material.getDoubleProperty("OGDEN_ALPHA2");
    alpha3 = material.getDoubleProperty("OGDEN_ALPHA3");

    // get mu parameters
    mu1 = material.getDoubleProperty("OGDEN_MU1");
    mu2 = material.getDoubleProperty("OGDEN_MU2");
    mu3 = material.getDoubleProperty("OGDEN_MU3");

    if (os) {
      (*os) << "\talpha1       = " << alpha1 << std::endl;
      (*os) << "\talpha2       = " << alpha2 << std::endl;
      (*os) << "\talpha3       = " << alpha3 << std::endl;
      (*os) << "\tmu1          = " << mu1 << std::endl;
      (*os) << "\tmu2          = " << mu2 << std::endl;
      (*os) << "\tmu3          = " << mu3 << std::endl;
    }

    // compute dilatational elastic wave speed
    if (os) (*os) << "\n\tcelerity is not defined" << std::endl;
  }

  // compute stored energy from principal stretches
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const double lam[],double t[],
                      double h[][3],bool first,bool second) {

    // exponents
    double alpha1 = material.getDoubleProperty("OGDEN_ALPHA1");
    double alpha2 = material.getDoubleProperty("OGDEN_ALPHA2");
    double alpha3 = material.getDoubleProperty("OGDEN_ALPHA3");

    // parameters
    double mu1 = material.getDoubleProperty("OGDEN_MU1");
    double mu2 = material.getDoubleProperty("OGDEN_MU2");
    double mu3 = material.getDoubleProperty("OGDEN_MU3");

    // potential
    double W = 0.0e0;
    for (unsigned int i=0; i < 3; i++) {
      W += (mu1/alpha1)*(std::pow(lam[i],0.5*alpha1) - 1.0)
          +(mu2/alpha2)*(std::pow(lam[i],0.5*alpha2) - 1.0)
          +(mu3/alpha3)*(std::pow(lam[i],0.5*alpha3) - 1.0);
    }

    // first derivative
    if (first) {
      for (unsigned int i=0; i < 3; i++) {
        t[i] = 0.5*(mu1*std::pow(lam[i],0.5*alpha1-1)
                   +mu2*std::pow(lam[i],0.5*alpha2-1)
                   +mu3*std::pow(lam[i],0.5*alpha3-1));
      }
    }
    
    // second derivative
    if (second) {
      for (unsigned int i=0; i < 3; i++)
        for (unsigned int j=0; j < 3; j++) {
          if (i == j)
            h[i][j] = 0.5*((0.5*alpha1-1)*mu1*std::pow(lam[i],0.5*alpha1-2)
                          +(0.5*alpha2-1)*mu2*std::pow(lam[i],0.5*alpha2-2)
                          +(0.5*alpha3-1)*mu3*std::pow(lam[i],0.5*alpha3-2));
          else
            h[i][j] = 0.0e0;
        }
    }

    return W;
  }
};


/**
 * Implementations of the model.
 */
class Ogden3D : public HyperElasticity<TensorAlgebra3D> {

 public:

  // constructor
  Ogden3D()
  : HyperElasticity<TensorAlgebra3D>(new OgdenPotential<TensorAlgebra3D>(),
                                     new StandardEOS()) {}

  // copy constructor
  Ogden3D(const Ogden3D& src)
  : HyperElasticity<TensorAlgebra3D>(src) {}

  // destructor
  virtual ~Ogden3D() {}
};
class Ogden2D : public HyperElasticity<TensorAlgebra2D> {

 public:

  // constructor
  Ogden2D()
  : HyperElasticity<TensorAlgebra2D>(new OgdenPotential<TensorAlgebra2D>(),
                                     new StandardEOS()) {}

  // copy constructor
  Ogden2D(const Ogden2D& src)
  : HyperElasticity<TensorAlgebra2D>(src) {}

  // destructor
  virtual ~Ogden2D() {}
};
class Ogden1D : public HyperElasticity<TensorAlgebra1D> {

 public:

  // constructor
  Ogden1D()
  : HyperElasticity<TensorAlgebra1D>(new OgdenPotential<TensorAlgebra1D>(),
                                     new StandardEOS()) {}

  // copy constructor
  Ogden1D(const Ogden1D& src)
  : HyperElasticity<TensorAlgebra1D>(src) {}

  // destructor
  virtual ~Ogden1D() {}
};

/**
 * The associated model builder
 */
class OgdenBuilder : public ModelBuilder {

 private:

  // constructor
  OgdenBuilder();

  // the instance
  static OgdenBuilder const* BUILDER;

 public:

  // destructor
  virtual ~OgdenBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
