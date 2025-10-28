/*
 *  $Id: IsotropicHenckyViscoHEMultiPotential.h 252 2018-05-18 11:58:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2018, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_ISOTROPIC_HENCKY_VISCO_ELASTIC_MULTI_POTENTIAL_H
#define ZORGLIB_MATL_MECA_HYPER_ISOTROPIC_HENCKY_VISCO_ELASTIC_MULTI_POTENTIAL_H

// std C library
#include <cstdio>
// local
#include <matl/meca/hyper/GeneralHenckyPotential.h>
#include <matl/meca/hyper/IsotropicHenckyViscousPotential.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing isotropic finite visco-elasticity models:
 * Hencky model, elastic part.
 */
template <class ALG>
class IsotropicHenckyHEMultiPotential : virtual public SpectralHEPotential<ALG> {
  
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
  IsotropicHenckyHEMultiPotential(unsigned int r,bool i) {
    rank = r;
    isochoric = i;
  }
  
  // copy constructor
  IsotropicHenckyHEMultiPotential(const IsotropicHenckyHEMultiPotential& src) {
    rank = src.rank;
    isochoric = src.isochoric;
  }
  
  // destructor
  virtual ~IsotropicHenckyHEMultiPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) {
      (*os) << "\n\t***Isotropic viscoelastic branch #";
      (*os) << rank << " (Hencky type - elastic part)***" << std::endl;
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
      else if (isochoric && nu < 0.5e0) {
        if (os) (*os) << "ERROR: Poisson's coefficient must set to 0.5 for isochoric potentials." << std::endl;
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

      material.setProperty("YOUNG_MODULUS",E);
      material.setProperty("POISSON_COEFFICIENT",nu);
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
                      const SYM_TENSOR& C,SYM_TENSOR& S,SYM_TENSOR4& M,
                      bool first,bool second) {
    
    // get elastic modulus
    char str[64];
    std::sprintf(str,"SHEAR_MODULUS_%u",rank);
    double mu = material.getDoubleProperty(str);
    double mu2 = mu+mu;
    double mu4 = mu2+mu2;
    
    // compute logarithmic strains
    SYM_TENSOR eps,sig,dLogC[SYM_TENSOR::MEMSIZE];
    SYM_TENSOR d2LogC[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
    eps = 0.5*log(C,dLogC,d2LogC,first || second,second);
    
    // compute deviatoric part of energy
    double W = mu*innerProd2(eps,eps);
    unsigned int sz = SYM_TENSOR::MEMSIZE;
    if (first) {
      sig = mu2*eps;
      for (unsigned int ij=0; ij < sz; ij++)
        S[ij] = innerProd2(sig,dLogC[ij]);
    }
    if (second) {
      SYM_TENSOR *p = *d2LogC;
      for (unsigned int ij=0; ij < sz; ij++)
        for (unsigned int kl=0; kl < sz; kl++, p++)
          M[ij][kl] = mu2*innerProd2(dLogC[ij],dLogC[kl]) 
                     +mu4*innerProd2(eps,*p);
    }
    
    if (!isochoric) {
      // get 1st Lame constant
      std::sprintf(str,"1ST_LAME_CONSTANT_%u",rank);
      double lambda = material.getDoubleProperty(str);
      
      // compute volumic part of energy
      double detC,tr;
      SYM_TENSOR Cinv;
      if (first || second) {
        Cinv = C.inverse(detC);
        tr = 0.5*std::log(detC);
      }
      else
        tr = trace(eps);
      W += 0.5*lambda*tr*tr;
      if (first) {
        S += (lambda*tr)*Cinv;
      }
      if (second) {
        M += lambda*outerProd(Cinv,Cinv);
        M.addIJKL(-lambda*tr,Cinv);
      }
    }
    
    return W;
  }
  
  // compute stored energy from principal stretches
  double storedEnergy(const MaterialProperties& material,const ParameterSet& extPar,
                      const double lam[],double sig[],double M[][3],
                      bool first,bool second) {
    
    // get elastic modulus
    char str[64];
    std::sprintf(str,"SHEAR_MODULUS_%u",rank);
    double mu = material.getDoubleProperty(str);
    
    // compute logarithmic strains
    double eps[3];
    for (unsigned int k=0; k < 3; k++) eps[k] = 0.5*std::log(lam[k]);
    
    // compute deviatoric part of energy
    double W = mu*(eps[0]*eps[0]+eps[1]*eps[1]+eps[2]*eps[2]);
    if (first) {
      for (unsigned int k=0; k < 3; k++) sig[k] = mu*eps[k]/lam[k];
    }
    if (second) {
      for (unsigned int k=0; k < 3; k++)
        for (unsigned int l=0; l < 3; l++)
          if (k == l)
            M[k][l] = mu*(0.5-eps[k])/(lam[k]*lam[k]);
          else
            M[k][l] = 0.0e0;
    }
    
    if (!isochoric) {
      // get 1st Lame constant
      std::sprintf(str,"1ST_LAME_CONSTANT_%u",rank);
      double lambda = material.getDoubleProperty(str);
      
      // compute volumic part of energy
      double tr = eps[0]+eps[1]+eps[2];
      W += 0.5*lambda*tr*tr;
      if (first) {
        double coef = 0.5*lambda*tr;
        for (unsigned int k=0; k < 3; k++) sig[k] += coef/lam[k];
      }
      if (second) {
        double coef2 = 0.25*lambda;
        double coef1 = coef2-0.5*lambda*tr;
        for (unsigned int k=0; k < 3; k++)
          for (unsigned int l=0; l < 3; l++)
            if (k == l)
              M[k][l] += coef1/(lam[k]*lam[k]);
            else
              M[k][l] += coef2/(lam[k]*lam[l]);
      }
    }

    return W;
  }
};


/**
 * Class describing isotropic finite visco-elasticity models:
 * Hencky model, viscous part.
 */
template <class ALG>
class IsotropicHenckyHEViscousMultiPotential 
: virtual public SpectralHEViscousPotential<ALG> {
  
 protected:
  
  // rank of viscoelastic module
  unsigned int rank;
  
  // isochoric?
  bool isochoric;
  
 public:
    
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::SymTensor::TYPE  TENSOR;

  // constructor
  IsotropicHenckyHEViscousMultiPotential(unsigned int r,bool i) {
    rank = r;
    isochoric = i;
  }
  
  // copy constructor
  IsotropicHenckyHEViscousMultiPotential(const IsotropicHenckyHEViscousMultiPotential& src) {
    rank = src.rank;
    isochoric = src.isochoric;
  }
  
  // destructor
  virtual ~IsotropicHenckyHEViscousMultiPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) {
      (*os) << "\n\t***Isotropic viscoelastic branch #";
      (*os) << rank << " (Hencky type - viscous part)***" << std::endl;
    }

    static const double ONE_THIRD = 1.e0/3.e0;
    static const double TWO_THIRD = 2.e0/3.e0;

    char str[64];
    double E,K=0.0e0,lambda=0.0e0,mu,nu;
    try {
      // get Young's modulus
      std::sprintf(str,"VISCOUS_YOUNG_MODULUS_%u",rank);
      E = material.getDoubleProperty(str);
      if (E < 0.e0) {
        if (os) (*os) << "ERROR: viscous Young's modulus must be positive." << std::endl;
        throw InvalidPropertyException(str);
      }

      // get Poisson's coefficient
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
      // get second Lame constant (a.k.a. shear modulus)
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

      // get first Lame constant
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

      material.setProperty("VISCOUS_YOUNG_MODULUS",E);
      material.setProperty("VISCOUS_POISSON_COEFFICIENT",nu);
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
  using SpectralHEViscousPotential<ALG>::dissipatedEnergy;
  double dissipatedEnergy(const MaterialProperties& material,
                          const ParameterSet& extPar,
                          const TENSOR& Fv0,const SYM_TENSOR& Cv,SYM_TENSOR& S,
                          SYM_TENSOR4& M,double dTime,bool first,bool second) {
    
    // get elastic modulus
    char str[64];
    std::sprintf(str,"SHEAR_MODULUS_%u",rank);
    double mu = material.getDoubleProperty(str);
    double mu2 = mu+mu;
    double mu4 = mu2+mu2;
    
    // compute viscous strain-rate
    SYM_TENSOR dCv;
    dCv = covariantPush(Cv,Fv0);
    double coef = 0.5/dTime;
    SYM_TENSOR eps,sig,dLogC[SYM_TENSOR::MEMSIZE];
    SYM_TENSOR d2LogC[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
    SYM_TENSOR Dv = coef*log(dCv,dLogC,d2LogC,first || second,second);
    
    // compute deviatoric part of dissipation potential
    double phi = mu*innerProd2(Dv,Dv);
    unsigned int sz = SYM_TENSOR::MEMSIZE;
    SYM_TENSOR Sv;
    SYM_TENSOR4 Mv;
    if (first) {
      sig = mu2*Dv;
      for (unsigned int ij=0; ij < sz; ij++)
        Sv[ij] = innerProd2(sig,dLogC[ij]);
    }
    if (second) {
      SYM_TENSOR *p = *d2LogC;
      for (unsigned int ij=0; ij < sz; ij++)
        for (unsigned int kl=0; kl < sz; kl++, p++)
          Mv[ij][kl] = mu2*innerProd2(dLogC[ij],dLogC[kl]) 
                      +(mu4*dTime)*innerProd2(Dv,*p);
    }
    
    if (!isochoric) {
      // get 1st Lame constant
      std::sprintf(str,"VISCOUS_1ST_LAME_CONSTANT_%u",rank);
      double lambda = material.getDoubleProperty(str);
      
      // compute volumic part of energy
      double detC,tr;
      SYM_TENSOR dCinv;
      if (first || second) {
        dCinv = dCv.inverse(detC);
        tr = coef*std::log(detC);
      }
      else
        tr = trace(Dv);
      phi += 0.5*lambda*tr*tr;
      if (first) {
        Sv += (lambda*tr)*dCinv;
      }
      if (second) {
        Mv += lambda*outerProd(dCinv,dCinv);
        Mv.addIJKL(-lambda*dTime*tr,dCinv);
      }
    }
    
    if (first) {
      S = contravariantPull(Sv,Fv0);
    }
    if (second) {
      M = contravariantPull(Mv,Fv0);
    }

    return phi;
  }
  
  // compute dissipated energy from principal stretches
  double dissipatedEnergy(const MaterialProperties& material,
                          const ParameterSet& extPar,
                          const double epsDot[],double sig[],double M[][3],
                          bool first,bool second) {
    
    // get elastic modulus
    char str[64];
    std::sprintf(str,"VISCOUS_SHEAR_MODULUS_%u",rank);
    double mu = material.getDoubleProperty(str);
    double mu2 = mu+mu;
    
    // compute deviatoric part of energy
    double W = mu*(epsDot[0]*epsDot[0]+epsDot[1]*epsDot[1]+epsDot[2]*epsDot[2]);
    if (first) {
      for (unsigned int k=0; k < 3; k++) sig[k] = mu2*epsDot[k];
    }
    if (second) {
      for (unsigned int k=0; k < 3; k++)
        for (unsigned int l=0; l < 3; l++)
          if (k == l)
            M[k][l] = mu2;
          else
            M[k][l] = 0.0e0;
    }

    if (!isochoric) {
      // get 1st Lame constant
      std::sprintf(str,"VISCOUS_1ST_LAME_CONSTANT_%u",rank);
      double lambda = material.getDoubleProperty(str);
      
      // compute volumic part of energy
      double tr = epsDot[0]+epsDot[1]+epsDot[2];
      W += 0.5*lambda*tr*tr;
      if (first) {
        double val = lambda*tr;
        for (unsigned int k=0; k < 3; k++) sig[k] += val;
      }
      if (second) {
        for (unsigned int k=0; k < 3; k++)
          for (unsigned int l=0; l < 3; l++)
            M[k][l] += lambda;
      }
    }

    return W;
  }
};


/**
 * Class for isotropic Hencky viscohyperelastic model (Maxwell branch).
 */
template <class ALG>
class IsotropicHenckyMaxwellViscoElasticity 
: virtual public SpectralMaxwellViscoElasticity<ALG> {
  
 public:
  
  // constructor
  IsotropicHenckyMaxwellViscoElasticity(unsigned int r,bool i = false)
  : SpectralMaxwellViscoElasticity<ALG>(*(new IsotropicHenckyHEMultiPotential<ALG>(r,i)),
                                        *(new IsotropicHenckyHEViscousMultiPotential<ALG>(r,i)),
                                        i) {}
  
  // copy constructor
  IsotropicHenckyMaxwellViscoElasticity(const IsotropicHenckyMaxwellViscoElasticity& src)
  : SpectralMaxwellViscoElasticity<ALG>(src) {}
};


/**
 * Implementations of the model : Maxwell.
 */
class IsotropicHenckyMaxwellViscoElasticity3D : public ViscoHyperElasticity<TensorAlgebra3D> {

 protected:

  bool isochoric;

 public:
  
  // constructor
  IsotropicHenckyMaxwellViscoElasticity3D(EOS *eos = 0,bool i = false)
  : HyperElasticity<TensorAlgebra3D>(
          new GeneralHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicElasticPotential<TensorAlgebra3D>())),
          eos) {isochoric = i;}
  
  // copy constructor
  IsotropicHenckyMaxwellViscoElasticity3D(const IsotropicHenckyMaxwellViscoElasticity3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src),
    ViscoHyperElasticity<TensorAlgebra3D>(src) {isochoric = src.isochoric;}
  
  // destructor
  virtual ~IsotropicHenckyMaxwellViscoElasticity3D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicHenckyMaxwellViscoElasticity<TensorAlgebra3D>(i+1,isochoric)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicHenckyMaxwellViscoElasticity<TensorAlgebra3D>(1,isochoric)));
    }
    
    // check properties
    ViscoHyperElasticity<TensorAlgebra3D>::checkProperties(material,os);
  }
};
class IsotropicHenckyMaxwellViscoElasticity2D : public ViscoHyperElasticity<TensorAlgebra2D> {

 protected:

  bool isochoric;

 public:
  
  // constructor
  IsotropicHenckyMaxwellViscoElasticity2D(EOS *eos = 0,bool i = false)
  : HyperElasticity<TensorAlgebra2D>(
          new GeneralHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicElasticPotential<TensorAlgebra2D>())),
          eos) {isochoric = i;}
  
  // copy constructor
  IsotropicHenckyMaxwellViscoElasticity2D(const IsotropicHenckyMaxwellViscoElasticity2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src),
    ViscoHyperElasticity<TensorAlgebra2D>(src) {isochoric = src.isochoric;}
  
  // destructor
  virtual ~IsotropicHenckyMaxwellViscoElasticity2D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicHenckyMaxwellViscoElasticity<TensorAlgebra2D>(i+1,isochoric)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicHenckyMaxwellViscoElasticity<TensorAlgebra2D>(1,isochoric)));
    }
    
    // check properties
    ViscoHyperElasticity<TensorAlgebra2D>::checkProperties(material,os);
  }
};
class IsotropicHenckyMaxwellViscoElasticity1D : public ViscoHyperElasticity<TensorAlgebra1D> {

 protected:

  bool isochoric;

 public:
  
  // constructor
  IsotropicHenckyMaxwellViscoElasticity1D(EOS *eos = 0,bool i = false)
  : HyperElasticity<TensorAlgebra1D>(
          new GeneralHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicElasticPotential<TensorAlgebra1D>())),
          eos) {isochoric = i;}
  
  // copy constructor
  IsotropicHenckyMaxwellViscoElasticity1D(const IsotropicHenckyMaxwellViscoElasticity1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src),
    ViscoHyperElasticity<TensorAlgebra1D>(src) {isochoric = src.isochoric;}
  
  // destructor
  virtual ~IsotropicHenckyMaxwellViscoElasticity1D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicHenckyMaxwellViscoElasticity<TensorAlgebra1D>(i+1,isochoric)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicHenckyMaxwellViscoElasticity<TensorAlgebra1D>(1,isochoric)));
    }
    
    // check properties
    ViscoHyperElasticity<TensorAlgebra1D>::checkProperties(material,os);
  }
};

/**
 * The associated model builder
 */
class IsotropicHenckyMaxwellViscoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicHenckyMaxwellViscoElasticityBuilder();
  
  // the instance
  static IsotropicHenckyMaxwellViscoElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicHenckyMaxwellViscoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};
      
      
/**
 * Implementations of the model : general viscoelasticity model (Kelvin+Maxwell).
 */
class IsotropicHenckyViscoElasticity3D : public ViscoHyperElasticity<TensorAlgebra3D> {

 protected:

  bool isochoric;

 public:

  // constructor
  IsotropicHenckyViscoElasticity3D(EOS *eos = 0,bool i = false)
  : HyperElasticity<TensorAlgebra3D>(
          new GeneralHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicElasticPotential<TensorAlgebra3D>())),
          eos),
    ViscoHyperElasticity<TensorAlgebra3D>(
          new IsotropicHenckyViscousPotential<TensorAlgebra3D>()) {isochoric = i;}

  // copy constructor
  IsotropicHenckyViscoElasticity3D(const IsotropicHenckyViscoElasticity3D& src)
  : HyperElasticity<TensorAlgebra3D>(src),
    ViscoHyperElasticity<TensorAlgebra3D>(src) {isochoric = src.isochoric;}

  // destructor
  virtual ~IsotropicHenckyViscoElasticity3D() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {

    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicHenckyMaxwellViscoElasticity<TensorAlgebra3D>(i+1,isochoric)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicHenckyMaxwellViscoElasticity<TensorAlgebra3D>(1,isochoric)));
    }

    // check properties
    ViscoHyperElasticity<TensorAlgebra3D>::checkProperties(material,os);
  }
};
class IsotropicHenckyViscoElasticity2D : public ViscoHyperElasticity<TensorAlgebra2D> {

 protected:

  bool isochoric;

 public:

  // constructor
  IsotropicHenckyViscoElasticity2D(EOS *eos = 0,bool i = false)
  : HyperElasticity<TensorAlgebra2D>(
          new GeneralHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicElasticPotential<TensorAlgebra2D>())),
          eos),
    ViscoHyperElasticity<TensorAlgebra2D>(
          new IsotropicHenckyViscousPotential<TensorAlgebra2D>()) {isochoric = i;}

  // copy constructor
  IsotropicHenckyViscoElasticity2D(const IsotropicHenckyViscoElasticity2D& src)
  : HyperElasticity<TensorAlgebra2D>(src),
    ViscoHyperElasticity<TensorAlgebra2D>(src) {isochoric = src.isochoric;}

  // destructor
  virtual ~IsotropicHenckyViscoElasticity2D() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {

    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicHenckyMaxwellViscoElasticity<TensorAlgebra2D>(i+1,isochoric)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicHenckyMaxwellViscoElasticity<TensorAlgebra2D>(1,isochoric)));
    }

    // check properties
    ViscoHyperElasticity<TensorAlgebra2D>::checkProperties(material,os);
  }
};
class IsotropicHenckyViscoElasticity1D : public ViscoHyperElasticity<TensorAlgebra1D> {

 protected:

  bool isochoric;

 public:

  // constructor
  IsotropicHenckyViscoElasticity1D(EOS *eos = 0,bool i = false)
  : HyperElasticity<TensorAlgebra1D>(
          new GeneralHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicElasticPotential<TensorAlgebra1D>())),
          eos),
    ViscoHyperElasticity<TensorAlgebra1D>(
          new IsotropicHenckyViscousPotential<TensorAlgebra1D>()) {isochoric = i;}

  // copy constructor
  IsotropicHenckyViscoElasticity1D(const IsotropicHenckyViscoElasticity1D& src)
  : HyperElasticity<TensorAlgebra1D>(src),
    ViscoHyperElasticity<TensorAlgebra1D>(src) {isochoric = src.isochoric;}

  // destructor
  virtual ~IsotropicHenckyViscoElasticity1D() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {

    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicHenckyMaxwellViscoElasticity<TensorAlgebra1D>(i+1,isochoric)));
    }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicHenckyMaxwellViscoElasticity<TensorAlgebra1D>(1,isochoric)));
    }

    // check properties
    ViscoHyperElasticity<TensorAlgebra1D>::checkProperties(material,os);
  }
};

/**
 * The associated model builder
 */
class IsotropicHenckyViscoElasticityBuilder : public ModelBuilder {

 private:

  // constructor
  IsotropicHenckyViscoElasticityBuilder();

  // the instance
  static IsotropicHenckyViscoElasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~IsotropicHenckyViscoElasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
