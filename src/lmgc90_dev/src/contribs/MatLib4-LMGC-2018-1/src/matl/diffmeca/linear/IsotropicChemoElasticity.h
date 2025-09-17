/*
 *  $Id: IsotropicChemoElasticity.h 236 2017-06-06 09:11:19Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFF_MECA_LINEAR_CHEMO_ELASTICITY_ISOTROPIC_H
#define ZORGLIB_MATL_DIFF_MECA_LINEAR_CHEMO_ELASTICITY_ISOTROPIC_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/linear/IsotropicElasticPotential.h>
#include <matl/diff/linear/StdLinChemicalCapacity.h>
#include <matl/diffmeca/linear/ChemoElasticity.h>
#include <matl/diffmeca/linear/IsotropicLinDilatancy.h>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing chemoelastic isotropic potentials.
 */
template <class ALG>
class IsotropicChemoElasticPotential
: virtual public ChemoElasticity<ALG>::Potential,
  virtual public IsotropicElasticPotential<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  IsotropicChemoElasticPotential() {}
  
  // copy constructor
  IsotropicChemoElasticPotential(const IsotropicChemoElasticPotential&) {}
  
  // destructor
  virtual ~IsotropicChemoElasticPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic chemoelastic potential***" << std::endl;
      
    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;
    
    // reference concentration
    double c0;
    try {
      c0 = material.getDoubleProperty("REFERENCE_CONCENTRATION");
      if (c0 < 0.e0) {
        if (os) (*os) << "ERROR: reference concentration must be positive." << std::endl;
        throw InvalidPropertyException("reference concentration");
      }
    }
    catch (NoSuchPropertyException) {
      // use initial concentration
      try {
        c0 = material.getDoubleProperty("INITIAL_CONCENTRATION");
        if (c0 < 0.e0) {
          if (os) (*os) << "ERROR: initial concentration must be positive." << std::endl;
          throw InvalidPropertyException("initial concentration");
        }
        material.setProperty("REFERENCE_CONCENTRATION",c0);
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: reference concentration cannot be set." << std::endl;
        throw e;
      }
    }
    
    double G,K,lambda,E,nu;
    try {
      // get shear modulus
      try {
        Function& fctG = material.getFunctionProperty("SHEAR_MODULUS_EVOLUTION");
        G = fctG.value(c0);
        material.setProperty("SHEAR_MODULUS",G);
        if (os) {
          (*os) << "\n\tshear modulus concentration dependence: ";
          (*os) << fctG << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        G = material.getDoubleProperty("SHEAR_MODULUS");
      }
      if (G < 0.e0) {
        if (os) (*os) << "ERROR: shear modulus must be positive." << std::endl;
        throw InvalidPropertyException("shear modulus");
      }
      
      // get bulk modulus
      try {
        Function& fctK = material.getFunctionProperty("BULK_MODULUS_EVOLUTION");
        K = fctK.value(c0);
        material.setProperty("BULK_MODULUS",K);
        if (os) {
          (*os) << "\n\tbulk modulus concentration dependence: ";
          (*os) << fctK << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        K = material.getDoubleProperty("BULK_MODULUS");
      }
      if (K < 0.e0) {
        if (os) (*os) << "ERROR: bulk modulus must be positive." << std::endl;
        throw InvalidPropertyException("bulk modulus");
      }
          
      // compute other properties
      lambda = K-TWO_THIRD*G;
      nu = (3*K-2*G)/(6*K+2*G);
      E = 2*G*(1.+nu);

      material.setProperty("1ST_LAME_CONSTANT",lambda);
      material.setProperty("2ND_LAME_CONSTANT",G);
      material.setProperty("YOUNG_MODULUS",E);
      material.setProperty("POISSON_COEFFICIENT",nu);
    }
    catch (NoSuchPropertyException) {
      try {
        // get Young's modulus
        E = material.getDoubleProperty("YOUNG_MODULUS");
        if (E < 0.e0) {
          if (os) (*os) << "ERROR: Young's modulus must be positive." << std::endl;
          throw InvalidPropertyException("Young's modulus");
        }
        
        // get Poisson's coefficient
        nu = material.getDoubleProperty("POISSON_COEFFICIENT");
        if (nu < -1.0e0 || nu > 0.5e0) {
          if (os) (*os) << "ERROR: Poisson's coefficient must be in [-1.0,0.5]." << std::endl;
          throw InvalidPropertyException("Poisson's coefficient");
        }
        
        // compute other properties
        G = 0.5*E/(1.+nu);
        K = ONE_THIRD*E/(1.-2*nu);
        lambda = K-TWO_THIRD*G;
        
        material.setProperty("BULK_MODULUS",K);
        material.setProperty("SHEAR_MODULUS",G);
        material.setProperty("1ST_LAME_CONSTANT",lambda);
        material.setProperty("2ND_LAME_CONSTANT",G);
      }
      catch (NoSuchPropertyException) {
        // get second Lame constant (a.k.a. shear modulus)
        try {
          G = material.getDoubleProperty("2ND_LAME_CONSTANT");
          if (G < 0.0e0) {
            if (os) (*os) << "ERROR: second Lame constant must be positive." << std::endl;
            throw InvalidPropertyException("second Lame constant");
          }
          material.setProperty("SHEAR_MODULUS",G);
        }
        catch (NoSuchPropertyException e) {
          if (os) (*os) << "ERROR: shear modulus cannot be defined." << std::endl;
          throw e;
        }
        
        // get first Lame constant
        try {
          lambda = material.getDoubleProperty("1ST_LAME_CONSTANT");
          K = lambda+TWO_THIRD*G;
          if (K < 0.0e0) {
            if (os) (*os) << "ERROR: bulk modulus must be positive." << std::endl;
            throw InvalidPropertyException("first Lame constant");
          }
          material.setProperty("BULK_MODULUS",K);
        }
        catch (NoSuchPropertyException) {
          if (os) (*os) << "WARNING: bulk modulus set to zero." << std::endl;
          K = 0.0e0;
          material.setProperty("BULK_MODULUS",K);
          lambda = K-TWO_THIRD*G;
          material.setProperty("1ST_LAME_CONSTANT",lambda);
        }
        
        // compute other properties
        nu = (3*K-2*G)/(6*K+2*G);
        E = 2*G*(1.+nu);
        
        material.setProperty("YOUNG_MODULUS",E);
        material.setProperty("POISSON_COEFFICIENT",nu);
      }
    }
      
    if (os) {
      (*os) << "\n\tAt reference concentration (c = " << c0 << "):" << std::endl;
      (*os) << "\tshear modulus         = " << G << std::endl;
      (*os) << "\tbulk modulus          = " << K << std::endl;
      (*os) << "\tYoung's modulus       = " << E << std::endl;
      (*os) << "\tPoisson's coefficient = " << nu << std::endl;
      (*os) << "\t1st Lame constant     = " << lambda << std::endl;
      (*os) << "\t2nd Lame constant     = " << G << std::endl;
    }
    
    // compute dilatational elastic wave speed
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      double cel = std::sqrt((lambda+2*G)/rho);
      material.setProperty("CELERITY",cel);
      if (os) (*os) << "\n\tcelerity              = " << cel << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tcelerity is not defined" << std::endl;
    }
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& material,const ParameterSet& extPar) {
    
    static const double TWO_THIRD  = 2.e0/3.e0;

    if (!extPar.count("CONCENTRATION")) return;
    double c = extPar.find("CONCENTRATION")->second;
    
    double G,K,lambda,E,nu;
    // get shear modulus
    try {
      Function& fctG = material.getFunctionProperty("SHEAR_MODULUS_EVOLUTION");
      G = fctG.value(c);
      material.setProperty("SHEAR_MODULUS",G);
    }
    catch (NoSuchPropertyException) {
      G = material.getDoubleProperty("SHEAR_MODULUS");
    }
    if (G < 0.e0) throw InvalidPropertyException("shear modulus");
    
    // get bulk modulus
    try {
      Function& fctK = material.getFunctionProperty("BULK_MODULUS_EVOLUTION");
      K = fctK.value(c);
      material.setProperty("BULK_MODULUS",K);
    }
    catch (NoSuchPropertyException) {
      K = material.getDoubleProperty("BULK_MODULUS");
    }
    if (K < 0.e0) throw InvalidPropertyException("bulk modulus");
    
    // compute other properties
    lambda = K-TWO_THIRD*G;
    nu = (3*K-2*G)/(6*K+2*G);
    E = 2*G*(1.+nu);
    
    material.setProperty("1ST_LAME_CONSTANT",lambda);
    material.setProperty("2ND_LAME_CONSTANT",G);
    material.setProperty("YOUNG_MODULUS",E);
    material.setProperty("POISSON_COEFFICIENT",nu);
    
    // compute dilatational elastic wave speed
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      double cel = std::sqrt((lambda+2*G)/rho);
      material.setProperty("CELERITY",cel);
    }
    catch (NoSuchPropertyException) {
      // nothing to do!
    }
  }
  
  // compute stored energy
  double storedChMEnergy(const MaterialProperties& material,
                         const ParameterSet& extPar,
                         const SYM_TENSOR& gam,double dc,SYM_TENSOR& sig,double& muC,
                         SYM_TENSOR4& M,SYM_TENSOR& dSig,double& C,
                         bool first,bool second) {
    
    static const double TWO_THIRD  = 2.e0/3.e0;

    // concentration
    double c0 = material.getDoubleProperty("REFERENCE_CONCENTRATION");
    double c = c0+dc;
    
    // get elastic constants
    double G,K,lambda;
    double dG,dK,dlambda;

    try { // get shear modulus
      Function& fctG = material.getFunctionProperty("SHEAR_MODULUS_EVOLUTION");
      G = fctG.value(c,dG);
    }
    catch (NoSuchPropertyException) {
      G = material.getDoubleProperty("SHEAR_MODULUS");
      dG = 0.0e0;
    }
    if (G < 0.e0) throw InvalidPropertyException("shear modulus");

    try { // get bulk modulus
      Function& fctK = material.getFunctionProperty("BULK_MODULUS_EVOLUTION");
      K = fctK.value(c,dK);
    }
    catch (NoSuchPropertyException) {
      K = material.getDoubleProperty("BULK_MODULUS");
      dK = 0.0e0;
    }
    if (K < 0.0e0) throw InvalidPropertyException("bulk modulus");
    
    // compute other properties
    lambda = K-TWO_THIRD*G;
    dlambda = dK-TWO_THIRD*dG;
    
    // transform engineering strains
    SYM_TENSOR eps = contravariant(gam);
    
    // potential
    double tr = trace(eps);
    double norm = innerProd2(eps,eps);
    double W = 0.5*lambda*tr*tr + G*norm;
    if (!first && !second) return W;
    
    // stress
    static SYM_TENSOR delta = SYM_TENSOR::identity();
    double G2 = G+G;
    if (first) {
      sig = (lambda*tr)*delta + G2*eps;
      muC = 0.5*dlambda*tr*tr + dG*norm;
    }
    
    // tangent
    if (second) {
      static const SYM_TENSOR4 II = SYM_TENSOR4::contravariantIdentity();
      static const SYM_TENSOR4 KK = SYM_TENSOR4::baseK();
      double dG2 = dG+dG;
      M = G2*II+(3*lambda)*KK;
      dSig = (dlambda*tr)*delta + dG2*eps;
      C = 0.0e0;
    }
    
    return W;
  }
          
  // compute stored energy (deviatoric part: eps = dev)
  double storedChMEnergyDev(const MaterialProperties& material,
                            const ParameterSet& extPar,
                            const SYM_TENSOR& gam,double dc,SYM_TENSOR& sig,double& muC,
                            SYM_TENSOR4& M,SYM_TENSOR& dSig,double& C,
                            bool first,bool second) {
    
    // concentration
    double c0 = material.getDoubleProperty("REFERENCE_CONCENTRATION");
    double c = c0+dc;

    // get shear modulus
    double G,dG;
    try {
      Function& fctG = material.getFunctionProperty("SHEAR_MODULUS_EVOLUTION");
      G = fctG.value(c,dG);
    }
    catch (NoSuchPropertyException) {
      G = material.getDoubleProperty("SHEAR_MODULUS");
      dG = 0.0e0;
    }
    if (G < 0.e0) throw InvalidPropertyException("shear modulus");
    
    // transform engineering strains (assumed to be deviatoric)
    SYM_TENSOR eps = contravariant(gam);
            
    // potential
    double norm = innerProd2(eps,eps);
    double W = G*norm;
    if (!first && !second) return W;

    // stress
    double G2 = G+G;
    if (first) {
      sig = G2*eps;
      muC = dG*norm;
    }
            
    // tangent
    if (second) {
      static const SYM_TENSOR4 II = SYM_TENSOR4::contravariantIdentity();
      double dG2 = dG+dG;
      M = G2*II;
      dSig = dG2*eps;
      C = 0.0e0;
    }
            
    return W;
  }
          
  // compute stored energy (volumic part: eps = trace)
  double storedChMEnergyVol(const MaterialProperties& material,
                            const ParameterSet& extPar,
                            double eps,double dc,double& sig,double& muC,
                            double& M,double& dSig,double& C,
                            bool first,bool second) {
    
    // concentration
    double c0 = material.getDoubleProperty("REFERENCE_CONCENTRATION");
    double c = c0+dc;

    // get bulk modulus
    double K,dK;
    try {
      Function& fctK = material.getFunctionProperty("BULK_MODULUS_EVOLUTION");
      K = fctK.value(c,dK);
    }
    catch (NoSuchPropertyException) {
      K = material.getDoubleProperty("BULK_MODULUS");
      dK = 0.0e0;
    }
    if (K < 0.0e0) throw InvalidPropertyException("bulk modulus");
    
    // potential
    double W = 0.5*K*eps*eps;
    if (!first && !second) return W;
            
    // stress
    if (first) {
      sig = K*eps;
      muC = 0.5*dK*eps*eps;
    }

    // tangent
    if (second) {
      M = K;
      dSig = dK*eps;
      C = 0.0e0;
    }

    return W;
  }
};

/**
 * Class describing isotropic chemoelastic dilatancy models.
 */
template <class ALG>
class IsotropicChemoElasticDilatancy
: virtual public ChemoElasticity<ALG>::Dilatancy,
  virtual public IsotropicLinDilatancy<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  IsotropicChemoElasticDilatancy() {}
  
  // copy constructor
  IsotropicChemoElasticDilatancy(const IsotropicChemoElasticDilatancy&) {}
  
  // destructor
  virtual ~IsotropicChemoElasticDilatancy() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Isotropic chemoelastic dilatancy***" << std::endl;

    double alpha,K,c0,cRef;

    // get dilatation coefficient
    try {
      alpha = material.getDoubleProperty("CHEMICAL_DILATATION_COEFFICIENT");
      if (alpha < 0.e0) {
        if (os) (*os) << "ERROR: chemical dilatation coefficient must be positive." << std::endl;
        throw InvalidPropertyException("chemical dilatation coefficient");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: chemical dilatation coefficient is not defined." << std::endl;
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
    
    // get reference temperature
    try {
      cRef = material.getDoubleProperty("REFERENCE_CONCENTRATION");
    }
    catch (NoSuchPropertyException) {
      // use initial temperature
      cRef = c0;
      material.setProperty("REFERENCE_CONCENTRATION",cRef);
    }
    

    // get bulk modulus
    try {
      try {
        Function& fctK = material.getFunctionProperty("BULK_MODULUS_EVOLUTION");
        K = fctK.value(cRef);
        material.setProperty("BULK_MODULUS",K);
        if (os) {
          (*os) << "\n\tbulk modulus concentration dependence: ";
          (*os) << fctK << std::endl;
        }
      }
      catch (NoSuchPropertyException) {
        K = material.getDoubleProperty("BULK_MODULUS");
      }
      if (K < 0.0e0) {
        if (os) (*os) << "ERROR: bulk modulus must be positive." << std::endl;
        throw InvalidPropertyException("bulk modulus");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: bulk modulus is not defined." << std::endl;
      throw e;
    }

    if (os) {
      (*os) << "\tchemical dilatation coefficient = " << alpha << std::endl;
      (*os) << "\tinitial concentration           = " << c0    << std::endl;
      (*os) << "\treference concentration         = " << cRef  << std::endl;
      (*os) << "\tbulk modulus                    = " << K     << std::endl;
    }
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& material,const ParameterSet& extPar) {
    
    if (!extPar.count("CONCENTRATION")) return;
    double c = extPar.find("CONCENTRATION")->second;
    
    // get bulk modulus
    double K;
    try {
      Function& fctK = material.getFunctionProperty("BULK_MODULUS_EVOLUTION");
      K = fctK.value(c);
      material.setProperty("BULK_MODULUS",K);
    }
    catch (NoSuchPropertyException) {
      K = material.getDoubleProperty("BULK_MODULUS");
    }
    if (K < 0.e0) throw InvalidPropertyException("bulk modulus");
  }
    
  // compute coupling energy
  double couplingChMEnergy(const MaterialProperties& material,
                           const ParameterSet& extPar,
                           const SYM_TENSOR& eps,double dc,SYM_TENSOR& sig,double& muC,
                           SYM_TENSOR4& M,SYM_TENSOR& dSig,double& C,
                           bool first,bool second) {
    
    // temperature
    double c0 = material.getDoubleProperty("INITIAL_CONCENTRATION");
    double cRef = material.getDoubleProperty("REFERENCE_CONCENTRATION");
    double c = cRef+dc;
    
    // get material parameters
    double alpha = material.getDoubleProperty("CHEMICAL_DILATATION_COEFFICIENT");
    double K,dK;
    try {
      Function& fctK = material.getFunctionProperty("BULK_MODULUS_EVOLUTION");
      K = fctK.value(c,dK);
    }
    catch (NoSuchPropertyException) {
      K = material.getDoubleProperty("BULK_MODULUS");
      dK = 0.0e0;
    }
    if (K < 0.0e0) throw InvalidPropertyException("bulk modulus");
    
    // compute coupling energy
    static const SYM_TENSOR delta = SYM_TENSOR::identity();
    double tr = trace(eps);
    double val = -3*alpha;
    double dC = c-c0;
    double coef = val*K*dC;
    double W = coef*tr;
    if (first) {
      sig = coef*delta;
      muC = val*tr*(K+dK*dC);
    }
    if (second) {
      M = 0.0e0;
      dSig = val*(K+dK*dC)*delta;
      C = val*tr*dK;
    }
    
    return W;
  }
    
  // compute coupling energy (volumic part)
  double couplingChMEnergyVol(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const double eps,double dc,double& sig,double& muC,
                              double& M,double& dSig,double& C,
                              bool first,bool second) {
      
    // temperature
    double c0 = material.getDoubleProperty("INITIAL_CONCENTRATION");
    double cRef = material.getDoubleProperty("REFERENCE_CONCENTRATION");
    double c = cRef+dc;

    // get material parameters
    double alpha = material.getDoubleProperty("CHEMICAL_DILATATION_COEFFICIENT");
    double K,dK;
    try {
      Function& fctK = material.getFunctionProperty("BULK_MODULUS_EVOLUTION");
      K = fctK.value(c,dK);
    }
    catch (NoSuchPropertyException) {
      K = material.getDoubleProperty("BULK_MODULUS");
      dK = 0.0e0;
    }
    if (K < 0.0e0) throw InvalidPropertyException("bulk modulus");

    // compute coupling energy
    double val = -3*alpha;
    double dC = c-c0;
    double coef = val*K*dC;
    double W = coef*eps;
    if (first) {
      sig = coef;
      muC = val*eps*(K+dK*dC);
    }
    if (second) {
      M = 0.0e0;
      dSig = val*(K+dK*dC);
      C = val*eps*dK;
    }

    return W;
  }
};


/**
 * Implementations of the model.
 */
class IsotropicChemoElasticity3D : public ChemoElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicChemoElasticity3D()
  : ChemoElasticity<TensorAlgebra3D>(new IsotropicChemoElasticPotential<TensorAlgebra3D>(),
                                     new StdLinChemicalCapacity(),
                                     new IsotropicChemoElasticDilatancy<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicChemoElasticity3D(const IsotropicChemoElasticity3D& src)
  : ChemoElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicChemoElasticity3D() {}
};
class IsotropicChemoElasticity2D : public ChemoElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicChemoElasticity2D()
  : ChemoElasticity<TensorAlgebra2D>(new IsotropicChemoElasticPotential<TensorAlgebra2D>(),
                                     new StdLinChemicalCapacity(),
                                     new IsotropicChemoElasticDilatancy<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicChemoElasticity2D(const IsotropicChemoElasticity2D& src)
  : ChemoElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicChemoElasticity2D() {}
};
class IsotropicChemoElasticity1D : public ChemoElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicChemoElasticity1D()
  : ChemoElasticity<TensorAlgebra1D>(new IsotropicChemoElasticPotential<TensorAlgebra1D>(),
                                     new StdLinChemicalCapacity(),
                                     new IsotropicChemoElasticDilatancy<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicChemoElasticity1D(const IsotropicChemoElasticity1D& src)
  : ChemoElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicChemoElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicChemoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicChemoElasticityBuilder();
  
  // the instance
  static IsotropicChemoElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicChemoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
