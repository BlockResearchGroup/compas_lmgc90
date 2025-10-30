/*
 *  $Id: NewtonianViscosityPotential.h 266 2020-03-31 14:42:08Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2020, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_NEWTONIAN_VISCOSITY_POTENTIAL_H
#define ZORGLIB_MATL_MECA_HYPER_NEWTONIAN_VISCOSITY_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/hyper/NeohookeanPotential.h>
#include <matl/meca/hyper/HyperViscoElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing (isotropic) Newtonian viscosity potentials.
 */
template <class ALG>
class NewtonianViscosityPotential 
: virtual public ViscoHyperElasticity<ALG>::ViscousPotential {

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  NewtonianViscosityPotential() {}
  
  // copy constructor
  NewtonianViscosityPotential(const NewtonianViscosityPotential&) {}
  
  // destructor
  virtual ~NewtonianViscosityPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Newtonian viscosity potential***" << std::endl;
     
    // newtonian viscosity
    double eta;
    try {
      eta = material.getDoubleProperty("VISCOUS_SHEAR_MODULUS");
      if (eta < 0.0e0) {
        if (os) (*os) << "ERROR: viscous shear modulus must be positive." << std::endl;
        throw InvalidPropertyException("viscous shear modulus");
      }
    }
    catch (NoSuchPropertyException) {
      eta = material.getDoubleProperty("NEWTONIAN_VISCOSITY");
      if (eta < 0.0e0) {
        if (os) (*os) << "ERROR: newtonian viscosity must be positive." << std::endl;
        throw InvalidPropertyException("newtonian viscosity");
      }
      material.setProperty("VISCOUS_SHEAR_MODULUS",eta);
    }
    
    // printout
    if (os) {
      (*os) << "\tviscosity coefficient = " << eta << std::endl;
    }
  }
  
  // compute stored energy
  double dissipatedEnergy(const MaterialProperties& material,
                          const ParameterSet& extPar,
                          const TENSOR& F0,const SYM_TENSOR& C,SYM_TENSOR& S,
                          SYM_TENSOR4& M,double dTime,bool stress,bool tangent) {

    static const double ONE_THIRD = 1.e0/3.e0;

    // viscosity coefficient
    double eta = material.getDoubleProperty("VISCOUS_SHEAR_MODULUS");
    double eta2 = eta+eta;
    
    // initial jacobian
    double J0 = determinant(F0);
    
    // compute strain rate
    double coef = 1.e0/dTime;
    SYM_TENSOR dC,dCbar;
    dC = covariantPush(C,F0);
    double detC = determinant(dC);
    double fact = std::pow(detC,-ONE_THIRD);
    dCbar = fact*dC;
    SYM_TENSOR dLogC[SYM_TENSOR::MEMSIZE];
    SYM_TENSOR d2LogC[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
    SYM_TENSOR epsDot = 0.5*coef*log(dCbar,dLogC,d2LogC,
                                     stress || tangent,tangent);
    
    // compute dissipation potential
    double phi;
    SYM_TENSOR sig;
    if (stress || tangent) {
      sig = eta2*J0*epsDot;
      phi = 0.5*innerProd2(sig,epsDot);
    }
    else
      phi = eta*J0*innerProd2(epsDot,epsDot);
    
    // stresses
    double tr;
    SYM_TENSOR sigBar,Cinv;
    if (stress || tangent) {
      for (unsigned int ij=0; ij < SYM_TENSOR::MEMSIZE; ij++) 
        sigBar[ij] = innerProd2(sig,dLogC[ij]);
      S = contravariantPull(sigBar,F0);
      S *= fact;
      // deviatoric part
      double detC;
      Cinv = C.inverse(detC);
      tr = innerProd2(S,C);
      S -= (ONE_THIRD*tr)*Cinv;
    }
    
    // tangents
    if (tangent) {
      SYM_TENSOR4 Mbar;
      SYM_TENSOR *p = *d2LogC;
      for (unsigned int ij=0; ij < SYM_TENSOR::MEMSIZE; ij++)
        for (unsigned int kl=0; kl < SYM_TENSOR::MEMSIZE; kl++, p++)
          Mbar[ij][kl] = coef*eta*J0*innerProd2(dLogC[ij],dLogC[kl])
                        +innerProd2(sig,*p);
      M = contravariantPull(Mbar,F0);
      M *= (fact*fact);
      // deviatoric part
      SYM_TENSOR Sbar;
      Sbar = innerProd2(M,C)+S;
      M -= ONE_THIRD*(outerProd(Sbar,Cinv)+outerProd(Cinv,Sbar))
          -(ONE_THIRD*ONE_THIRD*(innerProd2(C,innerProd2(M,C))-tr))*outerProd(Cinv,Cinv)
          -ONE_THIRD*tr*SYM_TENSOR4::identity();
      M *= (2*dTime);
    }
    
    return phi;
  }
};


/**
 * Implementations of the model.
 */
class NewtonianViscoHyperElasticity3D : public ViscoHyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NewtonianViscoHyperElasticity3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(new NeohookeanPotential<TensorAlgebra3D>(),eos),
    ViscoHyperElasticity<TensorAlgebra3D>(new NewtonianViscosityPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  NewtonianViscoHyperElasticity3D(const NewtonianViscoHyperElasticity3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src), ViscoHyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NewtonianViscoHyperElasticity3D() {}
};
class NewtonianViscoHyperElasticity2D : public ViscoHyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  NewtonianViscoHyperElasticity2D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra2D>(new NeohookeanPotential<TensorAlgebra2D>(),eos),
    ViscoHyperElasticity<TensorAlgebra2D>(new NewtonianViscosityPotential<TensorAlgebra2D>()) {}
  
  // copy constructor
  NewtonianViscoHyperElasticity2D(const NewtonianViscoHyperElasticity2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src), ViscoHyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~NewtonianViscoHyperElasticity2D() {}
};
class NewtonianViscoHyperElasticity1D : public ViscoHyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  NewtonianViscoHyperElasticity1D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra1D>(new NeohookeanPotential<TensorAlgebra1D>(),eos),
    ViscoHyperElasticity<TensorAlgebra1D>(new NewtonianViscosityPotential<TensorAlgebra1D>()) {}
  
  // copy constructor
  NewtonianViscoHyperElasticity1D(const NewtonianViscoHyperElasticity1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src), ViscoHyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~NewtonianViscoHyperElasticity1D() {}
};

/**
 * The associated model builder
 */
class NewtonianViscoHyperElasticityBuilder : public ModelBuilder {
  
 private:

  // constructor
  NewtonianViscoHyperElasticityBuilder();
  
  // the instance
  static NewtonianViscoHyperElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~NewtonianViscoHyperElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
