/*
 *  $Id: IsotropicViscoElasticMultiPotential.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_VISCO_ELASTIC_MULTI_POTENTIAL_H
#define ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_VISCO_ELASTIC_MULTI_POTENTIAL_H

// std C library
#include <cstdio>
// local
#include <matl/meca/linear/IsotropicViscousPotential.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing isotropic visco-elasticity models:
 * elastic part.
 */
template <class ALG>
class IsotropicElasticMultiPotential : virtual public Elasticity<ALG>::Potential {
  
 protected:
  
  // rank of viscoelastic module
  unsigned int rank;

  // isochoric?
  bool isochoric;
  
 public:
    
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  IsotropicElasticMultiPotential(unsigned int r,bool i) {
    rank = r;
    isochoric = i;
  }
  
  // copy constructor
  IsotropicElasticMultiPotential(const IsotropicElasticMultiPotential& src) {
    rank = src.rank;
    isochoric = src.isochoric;
  }
  
  // destructor
  virtual ~IsotropicElasticMultiPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) {
      (*os) << "\n\t***Isotropic viscoelastic branch #";
      (*os) << rank << " (elastic part)***" << std::endl;
    }
      
    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;

    char str[64];
    double E,K=0.0e0,lambda=0.0e0,mu,nu;
    try {
      // get Young's modulus
      std::sprintf(str,"YOUNG_MODULUS_%u",rank);
      E = material.getDoubleProperty(str);
      if (E < 0.e0) {
        if (os) (*os) << "ERROR: Young's modulus must be positive." << std::endl;
        throw InvalidPropertyException(str);
      }

      // get Poisson's coefficient
      std::sprintf(str,"POISSON_COEFFICIENT_%u",rank);
      nu = material.getDoubleProperty(str);
      if (nu < -1.0e0 || nu > 0.5e0) {
        if (os) (*os) << "ERROR: Poisson's coefficient must be in [-1.0,0.5]." << std::endl;
        throw InvalidPropertyException(str);
      }

      // compute other properties
      mu = 0.5*E/(1.+nu);
      K = ONE_THIRD*E/(1.-2*nu);
      lambda = K-TWO_THIRD*mu;

      std::sprintf(str,"BULK_MODULUS_%u",rank);
      material.setProperty(str,K);
      std::sprintf(str,"SHEAR_MODULUS_%u",rank);
      material.setProperty(str,mu);
      std::sprintf(str,"1ST_LAME_CONSTANT_%u",rank);
      material.setProperty(str,lambda);
      std::sprintf(str,"2ND_LAME_CONSTANT_%u",rank);
      material.setProperty(str,mu);
    }
    catch (NoSuchPropertyException) {
      // get second Lame constant (a.k.a. shear modulus)
      try {
        std::sprintf(str,"2ND_LAME_CONSTANT_%u",rank);
        mu = material.getDoubleProperty(str);
        if (mu < 0.0e0) {
          if (os) (*os) << "ERROR: Lame constants must be positive." << std::endl;
          throw InvalidPropertyException(str);
        }
      }
      catch (NoSuchPropertyException) {
        try {
          std::sprintf(str,"SHEAR_MODULUS_%u",rank);
          mu = material.getDoubleProperty(str);
          if (mu < 0.0e0) {
            if (os) (*os) << "ERROR: shear modulus must be positive." << std::endl;
            throw InvalidPropertyException(str);
          }
          std::sprintf(str,"2ND_LAME_CONSTANT_%u",rank);
          material.setProperty(str,mu);
        }
        catch (NoSuchPropertyException e) {
          if (os) (*os) << "ERROR: second Lame constant is not defined." << std::endl;
          throw e;
        }
      }

      // get first Lame constant
      if (!isochoric) {
        try {
          std::sprintf(str,"1ST_LAME_CONSTANT_%u",rank);
          lambda = material.getDoubleProperty(str);
          if (lambda < 0.0e0) {
            if (os) (*os) << "ERROR: Lame constants must be positive." << std::endl;
            throw InvalidPropertyException(str);
          }
          K = lambda+TWO_THIRD*mu;
          std::sprintf(str,"BULK_MODULUS_%u",rank);
          material.setProperty(str,K);
        }
        catch (NoSuchPropertyException) {
          try {
            std::sprintf(str,"BULK_MODULUS_%u",rank);
            K = material.getDoubleProperty(str);
            if (K < 0.0e0) {
              if (os) (*os) << "ERROR: bulk modulus must be positive." << std::endl;
              throw InvalidPropertyException(str);
            }
          }
          catch (NoSuchPropertyException) {
            if (os) (*os) << "WARNING: bulk modulus set to zero." << std::endl;
            K = 0.0e0;
            material.setProperty(str,K);
          }
          lambda = K-TWO_THIRD*mu;
          std::sprintf(str,"1ST_LAME_CONSTANT_%u",rank);
          material.setProperty(str,lambda);
        }
        
        // compute other properties
        nu = (3*K-2*mu)/(6*K+2*mu);
        E = 2*mu*(1.+nu);
      }
      else {
        nu = 0.5;
        E = 3*mu;
      }

      std::sprintf(str,"YOUNG_MODULUS_%u",rank);
      material.setProperty(str,E);
      std::sprintf(str,"POISSON_COEFFICIENT_%u",rank);
      material.setProperty(str,nu);
    }
      
    if (os) {
      (*os) << "\tYoung's modulus       = " << E << std::endl;
      (*os) << "\tPoisson's coefficient = " << nu << std::endl;
      if (isochoric) {
        (*os) << "\tshear modulus         = " << mu << std::endl;
      }
      else {
        (*os) << "\tbulk modulus          = " << K << std::endl;
        (*os) << "\t1st Lame constant     = " << lambda << std::endl;
        (*os) << "\t2nd Lame constant     = " << mu << std::endl;
      }
    }
  }
  
  // compute stored energy
  double storedEnergy(const MaterialProperties& material,const ParameterSet& extPar,
                      const SYM_TENSOR& gam,SYM_TENSOR& sig,SYM_TENSOR4& M,
                      bool first,bool second) {
    
    // get elastic modulus
    char str[64];
    std::sprintf(str,"SHEAR_MODULUS_%u",rank);
    double mu = material.getDoubleProperty(str);
    double mu2 = mu+mu;
    
    // transform engineering strains
    SYM_TENSOR eps = contravariant(gam);
    
    // compute deviatoric part of energy
    double W = mu*innerProd2(eps,eps);
    if (first) sig = mu2*eps;
    if (second) {
      static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
      M = mu2*I;
    }
    
    if (!isochoric) {
      // get 1st Lame constant
      std::sprintf(str,"1ST_LAME_CONSTANT_%u",rank);
      double lambda = material.getDoubleProperty(str);
      
      // compute volumic part of energy
      double tr = trace(eps);
      W += 0.5*lambda*tr*tr;
      if (first) {
        static SYM_TENSOR delta = SYM_TENSOR::identity();
        sig += (lambda*tr)*delta;
      }
      if (second) {
        static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
        M += (3*lambda)*K;
      }
    }
    
    return W;
  }
  
  // compute material stiffness (Hooke) tensor
  void computeStiffness(const MaterialProperties& material,
                        const ParameterSet& extPar,SYM_TENSOR4& M) {
    
    // get elastic constants
    char str[64];
    std::sprintf(str,"SHEAR_MODULUS_%u",rank);
    double mu2 = 2*material.getDoubleProperty(str);

    // deviatoric stiffness
    static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
    M = mu2*I;
    
    if (!isochoric) {
      // get 1st Lame constant
      std::sprintf(str,"1ST_LAME_CONSTANT_%u",rank);
      double lambda = material.getDoubleProperty(str);
    
      // volumic stiffness
      static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
      M += (3*lambda)*K;
    }
  }
};


/**
 * Class describing isotropic visco-elasticity models:
 * viscous part.
 */
template <class ALG>
class IsotropicViscousMultiPotential
: virtual public ViscoElasticity<ALG>::ViscousPotential {
  
 protected:
  
  // rank of viscoelastic module
  unsigned int rank;

  // isochoric?
  bool isochoric;

 public:
    
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // constructor
  IsotropicViscousMultiPotential(unsigned int r,bool i) {
    rank = r;
    isochoric = i;
  }
  
  // copy constructor
  IsotropicViscousMultiPotential(const IsotropicViscousMultiPotential& src) {
    rank = src.rank;
    isochoric = src.isochoric;
  }
  
  // destructor
  virtual ~IsotropicViscousMultiPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) {
      (*os) << "\n\t***Isotropic viscoelastic branch #";
      (*os) << rank << " (viscous part)***" << std::endl;
    }
      
    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;

    char str[64];
    double E,K=0.0e0,lambda=0.0e0,mu,nu;
    try {
      // get viscous Young's modulus
      std::sprintf(str,"VISCOUS_YOUNG_MODULUS_%u",rank);
      E = material.getDoubleProperty(str);
      if (E < 0.e0) {
        if (os) (*os) << "ERROR: viscous Young's modulus must be positive." << std::endl;
        throw InvalidPropertyException(str);
      }
      
      // get viscous Poisson's coefficient
      std::sprintf(str,"VISCOUS_POISSON_COEFFICIENT_%u",rank);
      nu = material.getDoubleProperty(str);
      if (nu < -1.0e0 || nu > 0.5e0) {
        if (os) (*os) << "ERROR: viscous Poisson's coefficient must be in [-1.0,0.5]." << std::endl;
        throw InvalidPropertyException(str);
      }
      
      // compute other properties
      mu = 0.5*E/(1.+nu);
      K = ONE_THIRD*E/(1.-2*nu);
      lambda = K-TWO_THIRD*mu;
      
      std::sprintf(str,"VISCOUS_BULK_MODULUS_%u",rank);
      material.setProperty(str,K);
      std::sprintf(str,"VISCOUS_SHEAR_MODULUS_%u",rank);
      material.setProperty(str,mu);
      std::sprintf(str,"VISCOUS_1ST_LAME_CONSTANT_%u",rank);
      material.setProperty(str,lambda);
      std::sprintf(str,"VISCOUS_2ND_LAME_CONSTANT_%u",rank);
      material.setProperty(str,mu);
    }
    catch (NoSuchPropertyException) {
      // get viscous second Lame constant (a.k.a. viscous shear modulus)
      try {
        std::sprintf(str,"VISCOUS_2ND_LAME_CONSTANT_%u",rank);
        mu = material.getDoubleProperty(str);
        if (mu < 0.0e0) {
          if (os) (*os) << "ERROR: viscous Lame constants must be positive." << std::endl;
          throw InvalidPropertyException(str);
        }
      }
      catch (NoSuchPropertyException) {
        try {
          std::sprintf(str,"VISCOUS_SHEAR_MODULUS_%u",rank);
          mu = material.getDoubleProperty(str);
          if (mu < 0.0e0) {
            if (os) (*os) << "ERROR: viscous shear modulus must be positive." << std::endl;
            throw InvalidPropertyException(str);
          }
          std::sprintf(str,"VISCOUS_2ND_LAME_CONSTANT_%u",rank);
          material.setProperty(str,mu);
        }
        catch (NoSuchPropertyException e) {
          if (os) (*os) << "ERROR: viscous second Lame constant is not defined." << std::endl;
          throw e;
        }
      }
      
      // get viscous first Lame constant
      if (!isochoric) {
        try {
          std::sprintf(str,"VISCOUS_1ST_LAME_CONSTANT_%u",rank);
          lambda = material.getDoubleProperty(str);
          if (lambda < 0.0e0) {
            if (os) (*os) << "ERROR: viscous Lame constants must be positive." << std::endl;
            throw InvalidPropertyException(str);
          }
          K = lambda+TWO_THIRD*mu;
          std::sprintf(str,"VISCOUS_BULK_MODULUS_%u",rank);
          material.setProperty(str,K);
        }
        catch (NoSuchPropertyException) {
          try {
            std::sprintf(str,"VISCOUS_BULK_MODULUS_%u",rank);
            K = material.getDoubleProperty(str);
            if (K < 0.0e0) {
              if (os) (*os) << "ERROR: viscous bulk modulus must be positive." << std::endl;
              throw InvalidPropertyException(str);
            }
          }
          catch (NoSuchPropertyException) {
            if (os) (*os) << "WARNING: viscous bulk modulus set to zero." << std::endl;
            K = 0.0e0;
            material.setProperty(str,K);
          }
          lambda = K-TWO_THIRD*mu;
          std::sprintf(str,"VISCOUS_1ST_LAME_CONSTANT_%u",rank);
          material.setProperty(str,lambda);
        }
        
        // compute other properties
        nu = (3*K-2*mu)/(6*K+2*mu);
        E = 2*mu*(1.+nu);
      }
      else {
        nu = 0.5;
        E = 3*mu;
      }
      
      std::sprintf(str,"VISCOUS_YOUNG_MODULUS_%u",rank);
      material.setProperty(str,E);
      std::sprintf(str,"VISCOUS_POISSON_COEFFICIENT_%u",rank);
      material.setProperty(str,nu);
    }
    
    if (os) {
      (*os) << "\tviscous Young's modulus       = " << E << std::endl;
      (*os) << "\tviscous Poisson's coefficient = " << nu << std::endl;
      if (isochoric) {
        (*os) << "\tviscous shear modulus         = " << mu << std::endl;
      }
      else {
        (*os) << "\tviscous bulk modulus          = " << K << std::endl;
        (*os) << "\tviscous 1st Lame constant     = " << lambda << std::endl;
        (*os) << "\tviscous 2nd Lame constant     = " << mu << std::endl;
      }
    }
  }

  // compute dissipated energy
  double dissipatedEnergy(const MaterialProperties& material,const ParameterSet& extPar,
                          const SYM_TENSOR& gam,const SYM_TENSOR& gamDot,
                          SYM_TENSOR& sig1,SYM_TENSOR& sig2,
                          SYM_TENSOR4& M11,SYM_TENSOR4& M22,
                          SYM_TENSOR4& M12,double dTime,bool first,bool second) {
    
    // get viscous modulus
    char str[64];
    std::sprintf(str,"VISCOUS_SHEAR_MODULUS_%u",rank);
    double mu = material.getDoubleProperty(str);
    double mu2 = mu+mu;
      
    // transform engineering strains
    SYM_TENSOR epsDot = contravariant(gamDot);
      
    // compute deviatoric part of energy
    double Wv = mu*innerProd2(epsDot,epsDot);
    if (first) {
      sig1 = 0.0e0;
      sig2 = mu2*epsDot;
    }
    if (second) {
      static const SYM_TENSOR4 I = SYM_TENSOR4::contravariantIdentity();
      M11 = 0.0e0;
      M12 = 0.0e0;
      M22 = mu2*I;
    }
    
    if (!isochoric) {
      // get viscous 1st Lame constant
      std::sprintf(str,"VISCOUS_1ST_LAME_CONSTANT_%u",rank);
      double lambda = material.getDoubleProperty(str);
      
      // compute volumic part of energy
      double tr = trace(epsDot);
      Wv += 0.5*lambda*tr*tr; 
      if (first) {
        static SYM_TENSOR delta = SYM_TENSOR::identity();
        sig2 += (lambda*tr)*delta;
      }
      if (second) {
        static const SYM_TENSOR4 K = SYM_TENSOR4::baseK();
        M22 += (3*lambda)*K;
      }
    }

    return Wv;
  }
};


/**
 * Class for standard (linear) isotropic viscoelastic model (Maxwell branch).
 */
template <class ALG>
class IsotropicMaxwellViscoElasticity 
: virtual public StdMaxwellViscoElasticity<ALG> {
  
 public:
    
  // constructor
  IsotropicMaxwellViscoElasticity(unsigned int r,bool i = false)
  : StdMaxwellViscoElasticity<ALG>(*(new IsotropicElasticMultiPotential<ALG>(r,i)),
                                   *(new IsotropicViscousMultiPotential<ALG>(r,i)),
                                   i) {}
  
  // copy constructor
  IsotropicMaxwellViscoElasticity(const IsotropicMaxwellViscoElasticity& src)
  : StdMaxwellViscoElasticity<ALG>(src) {}
};


/**
 * Implementations of the model : Maxwell.
 */
class IsotropicMaxwellViscoElasticity3D : public ViscoElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicMaxwellViscoElasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicMaxwellViscoElasticity3D(const IsotropicMaxwellViscoElasticity3D& src) 
  : Elasticity<TensorAlgebra3D>(src), ViscoElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicMaxwellViscoElasticity3D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {

    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellViscoElasticity<TensorAlgebra3D>(i+1)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellViscoElasticity<TensorAlgebra3D>(1)));
    }

    // check properties
    ViscoElasticity<TensorAlgebra3D>::checkProperties(material,os);
  }
};
class IsotropicMaxwellViscoElasticity2D : public ViscoElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicMaxwellViscoElasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicMaxwellViscoElasticity2D(const IsotropicMaxwellViscoElasticity2D& src) 
  : Elasticity<TensorAlgebra2D>(src), ViscoElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicMaxwellViscoElasticity2D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellViscoElasticity<TensorAlgebra2D>(i+1)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellViscoElasticity<TensorAlgebra2D>(1)));
    }
    
    // check properties
    ViscoElasticity<TensorAlgebra2D>::checkProperties(material,os);
  }
};
class IsotropicMaxwellViscoElasticity1D : public ViscoElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicMaxwellViscoElasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicMaxwellViscoElasticity1D(const IsotropicMaxwellViscoElasticity1D& src) 
  : Elasticity<TensorAlgebra1D>(src), ViscoElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicMaxwellViscoElasticity1D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellViscoElasticity<TensorAlgebra1D>(i+1)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellViscoElasticity<TensorAlgebra1D>(1)));
    }
    
    // check properties
    ViscoElasticity<TensorAlgebra1D>::checkProperties(material,os);
  }
};

/**
 * The associated model builder
 */
class IsotropicMaxwellViscoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicMaxwellViscoElasticityBuilder();
  
  // the instance
  static IsotropicMaxwellViscoElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicMaxwellViscoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementations of the model : general viscoelasticity model (Kelvin+Maxwell).
 */
class IsotropicViscoElasticity3D : public ViscoElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicViscoElasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>()),
    ViscoElasticity<TensorAlgebra3D>(new IsotropicViscousPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicViscoElasticity3D(const IsotropicViscoElasticity3D& src) 
  : Elasticity<TensorAlgebra3D>(src), ViscoElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicViscoElasticity3D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellViscoElasticity<TensorAlgebra3D>(i+1)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellViscoElasticity<TensorAlgebra3D>(1)));
    }
    
    // check properties
    ViscoElasticity<TensorAlgebra3D>::checkProperties(material,os);
  }
};
class IsotropicViscoElasticity2D : public ViscoElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicViscoElasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>()),
    ViscoElasticity<TensorAlgebra2D>(new IsotropicViscousPotential<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicViscoElasticity2D(const IsotropicViscoElasticity2D& src) 
  : Elasticity<TensorAlgebra2D>(src), ViscoElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicViscoElasticity2D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellViscoElasticity<TensorAlgebra2D>(i+1)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellViscoElasticity<TensorAlgebra2D>(1)));
    }
    
    // check properties
    ViscoElasticity<TensorAlgebra2D>::checkProperties(material,os);
  }
};
class IsotropicViscoElasticity1D : public ViscoElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicViscoElasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>()),
    ViscoElasticity<TensorAlgebra1D>(new IsotropicViscousPotential<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicViscoElasticity1D(const IsotropicViscoElasticity1D& src) 
  : Elasticity<TensorAlgebra1D>(src), ViscoElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicViscoElasticity1D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellViscoElasticity<TensorAlgebra1D>(i+1)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellViscoElasticity<TensorAlgebra1D>(1)));
    }
    
    // check properties
    ViscoElasticity<TensorAlgebra1D>::checkProperties(material,os);
  }
};

/**
 * The associated model builder
 */
class IsotropicViscoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicViscoElasticityBuilder();
  
  // the instance
  static IsotropicViscoElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicViscoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
