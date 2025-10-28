/*
 *  $Id: GeneralThermalHenckyPotential.h 142 2014-02-07 12:51:54Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_HYPER_GENERAL_HENCKY_POTENTIAL_H
#define ZORGLIB_MATL_MECA_THERMO_HYPER_GENERAL_HENCKY_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/hyper/IsotropicStdThermalDilatancy.h>
#include <matl/thermo/nonlinear/StdThermalCapacity.h>
#include <matl/thermomeca/hyper/IsotropicThermoHEDilatancy.h>
#include <matl/thermomeca/hyper/ThermoHyperElasticity.h>
#include <matl/thermomeca/linear/IsotropicThermoElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing general thermo-hyperelastic Hencky potentials.
 */
template <class ALG>
class GeneralThermalHenckyPotential 
: virtual public ThermoHyperElasticity<ALG>::Potential {
  
 public:

  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:
    
  // counter
  unsigned int *count;
  
  // linear elastic potential
  typename ThermoElasticity<ALG>::Potential *potential;
  
  // flag for linearization of strains
  bool linearize;
  
 public:

  // constructor
  GeneralThermalHenckyPotential(typename ThermoElasticity<ALG>::Potential& p,bool l = false) {
    count = new unsigned int(1);
    potential = &p;
    linearize = l;
  }
  
  // copy constructor
  GeneralThermalHenckyPotential(const GeneralThermalHenckyPotential& src) {
    count = src.count;
    ++(*count);
    potential = src.potential;
    linearize = src.linearize;
  }

  // destructor
  virtual ~GeneralThermalHenckyPotential() {
    if (--(*count) != 0) return;
    delete count;
    if (potential) delete potential;
  }
  
  // check if strains are linearized
  bool isLinearized() const {return linearize;}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***General Hencky potential (with thermal dependence)***" << std::endl;
    if (os && linearize) (*os) << "\t (linearized strains)" << std::endl;
    
    // associated linearized potential
    potential->checkProperties(material,os);
    
    // reference temperature
    try {
      double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
      if (TRef <= 0.e0) {
        if (os) (*os) << "ERROR: reference temperature must be strictly positive." << std::endl;
        throw InvalidPropertyException("reference temperature");
      }
      if (os) (*os) << "\n\treference temperature = " << TRef << std::endl;
    }
    catch (NoSuchPropertyException) {
      // use initial temperature
      try {
        double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
        if (T0 <= 0.e0) {
          if (os) (*os) << "ERROR: initial temperature must be strictly positive." << std::endl;
          throw InvalidPropertyException("initial temperature");
        }
        material.setProperty("REFERENCE_TEMPERATURE",T0);
        if (os) (*os) << "\n\treference temperature = " << T0 << std::endl;
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: reference temperature cannot be set." << std::endl;
        throw e;
      }
    }
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    potential->rotateProperties(material,R);
  }
  
  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& C,SYM_TENSOR& S,
                      SYM_TENSOR4& M,bool first,bool second) {

    // compute logarithmic strain tensor
    SYM_TENSOR eps,dLogC[SYM_TENSOR::MEMSIZE];
    SYM_TENSOR d2LogC[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
    if (!linearize)
      eps = 0.5*covariant(log(C,dLogC,d2LogC,
                              first || second,second));
    else
      eps = 0.5*(covariant(C)-SYM_TENSOR::identity());

    // compute elastic energy and stresses
    SYM_TENSOR sig;
    SYM_TENSOR4 K;
    typename Elasticity<ALG>::Potential* p 
      = static_cast<typename Elasticity<ALG>::Potential*>(potential);
    double W = p->storedEnergy(material,extPar,eps,sig,K,
                               first || (second && !linearize),second);

    if (!linearize) {
      
      // take into account geometrical terms
      unsigned int ij,kl,sz = SYM_TENSOR::MEMSIZE;
      
      if (first) {
        for (ij=0; ij < sz; ij++) S[ij] = innerProd2(sig,dLogC[ij]);
      }
      
      if (second) {
        SYM_TENSOR *p = *d2LogC;
        for (ij=0; ij < sz; ij++)
          for (kl=0; kl < sz; kl++)
            M[ij][kl] = innerProd2(dLogC[ij],innerProd2(K,dLogC[kl]))
                       +2*innerProd2(sig,*p);
      }
    }
    else {
      if (first) S = sig;
      if (second) M = K;
    }
    
    return W;
  }
  
  // compute stored energy
  double storedThMEnergy(const MaterialProperties& material,
                         const ParameterSet& extPar,
                         const SYM_TENSOR& C,double T,SYM_TENSOR& S,double& N,
                         SYM_TENSOR4& M,SYM_TENSOR& dS,double& Cm,
                         bool first,bool second) {
    
    // compute logarithmic strain tensor
    SYM_TENSOR eps,dLogC[SYM_TENSOR::MEMSIZE];
    SYM_TENSOR d2LogC[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
    if (!linearize)
      eps = 0.5*covariant(log(C,dLogC,d2LogC,
                              first || second,second));
    else
      eps = 0.5*(covariant(C)-SYM_TENSOR::identity());
    
    // compute temperature increment
    double T0 = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    double Th = T-T0;

    // compute elastic energy and stresses
    SYM_TENSOR sig,dSig;
    SYM_TENSOR4 K;
    double W = potential->storedThMEnergy(material,extPar,eps,Th,sig,N,K,dSig,Cm,
                                          first || (second && !linearize),second);
    
    if (!linearize) {
      
      // take into account geometrical terms
      unsigned int ij,kl,sz = SYM_TENSOR::MEMSIZE;
      
      if (first) {
        for (ij=0; ij < sz; ij++) S[ij] = innerProd2(sig,dLogC[ij]);
      }
      
      if (second) {
        for (ij=0; ij < sz; ij++) dS[ij] = innerProd2(dSig,dLogC[ij]);
        SYM_TENSOR *p = *d2LogC;
        for (ij=0; ij < sz; ij++)
          for (kl=0; kl < sz; kl++, p++)
            M[ij][kl] = innerProd2(dLogC[ij],innerProd2(K,dLogC[kl]))
                       +2*innerProd2(sig,*p);
      }
    }
    else {
      if (first) S = sig;
      if (second) {
        M = K;
        dS = dSig;
      }
    }
    
    return W;
  }
};


/**
 * Implementations of the model.
 */
class IsotropicThermoHyperElasticity3D : public ThermoHyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicThermoHyperElasticity3D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra3D>(
          new GeneralThermalHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra3D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicThermoHyperElasticity3D(const IsotropicThermoHyperElasticity3D& src) 
  : ThermoHyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoHyperElasticity3D() {}
};
class IsotropicThermoHyperElasticity2D : public ThermoHyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicThermoHyperElasticity2D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra2D>(
          new GeneralThermalHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra2D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicThermoHyperElasticity2D(const IsotropicThermoHyperElasticity2D& src) 
  : ThermoHyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoHyperElasticity2D() {}
};
class IsotropicThermoHyperElasticity1D : public ThermoHyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicThermoHyperElasticity1D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra1D>(
          new GeneralThermalHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra1D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicThermoHyperElasticity1D(const IsotropicThermoHyperElasticity1D& src) 
  : ThermoHyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoHyperElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicThermoHyperElasticityBuilder : public ModelBuilder {

 private:
  
  // constructor
  IsotropicThermoHyperElasticityBuilder();

  // the instance
  static IsotropicThermoHyperElasticityBuilder const* BUILDER;

 public:
  
  // destructor
  virtual ~IsotropicThermoHyperElasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
