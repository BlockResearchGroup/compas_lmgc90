/*
 *  $Id: J2PlasticityMixed.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_J2_PLASTICITY_MIXED_H
#define ZORGLIB_MATL_MECA_LINEAR_J2_PLASTICITY_MIXED_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
// std C++ library
#include <limits>
// local
#include <matl/meca/linear/ElastoPlasticity.h>
#include <matl/meca/linear/HardeningModels.h>
#include <matl/meca/linear/IsotropicElasticPotential.h>
#include <matl/meca/linear/RateDependencyModels.h>
#include <matl/meca/linear/ViscoPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * J2 plasticity with mixed hardening (isotropic + linear kinematic).
 */
template <class ALG>
class J2PlasticityMixed : virtual public ElastoPlasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:

  // associated visco-plasticity model
  ViscoPlasticitySimple *viscoPlasticity;

  // empty constructor
  J2PlasticityMixed(ViscoPlasticitySimple* vp = 0) {
    viscoPlasticity = vp;
  }

 public:

  // constructor
  J2PlasticityMixed(ViscoPlasticitySimple& vp)
    : Elasticity<ALG>(new IsotropicElasticPotential<ALG>()) {viscoPlasticity = &vp;}
  
  // copy constructor
  J2PlasticityMixed(const J2PlasticityMixed& src)
    : Elasticity<ALG>(src), ElastoPlasticity<ALG>(src) {viscoPlasticity = src.viscoPlasticity;}

  // destructor
  virtual ~J2PlasticityMixed() {
    if (*(this->count) > 1) return;
    if (viscoPlasticity) delete viscoPlasticity;
  }

  // check consistency of properties
  void checkProperties(MaterialProperties& material,std::ostream *os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {
     if (os) (*os) << "\nJ2 plasticity model with mixed hardening (small strains):" << std::endl;
    
    // density
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      if (os) (*os) << "\n\tmass density = " << rho << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tmass density is not defined" << std::endl;
    }
    
    // elastic potential
    this->potential->checkProperties(material,os);
    
    // dilatancy model
    if (this->dilatancy) this->dilatancy->checkProperties(material,os);
    
    // kinematic hardening modulus
    double Hk;
    try {
      Hk = material.getDoubleProperty("KINEMATIC_HARDENING_MODULUS");
      if (Hk < 0.0e0) {
        if (os) (*os) << "ERROR: kinematic hardening modulus must be positive." << std::endl;
        throw InvalidPropertyException("kinematic hardening modulus");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: kinematic hardening modulus is not defined." << std::endl;
      throw e;
    }
    
    // viscoplastic model
    try {
      viscoPlasticity->checkProperties(material,os);
    }
    catch (InvalidPropertyException e) {
      if (e.mesg() == "hardening modulus") {
        if (os) (*os) << "       Check that condition is verified including kinematic hardening." << std::endl;
      }
      else
        throw e;
    }
    
    if (os) {
      (*os) << "\n\t***Linear kinematic hardening model***" << std::endl;
      (*os) << "\tkinematic hardening modulus = " << Hk << std::endl;
    }
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    Elasticity<ALG>::updateProperties(mater,extPar);
    viscoPlasticity->updateProperties(mater,extPar);
  }
  
  // number of internal variables
  unsigned int nIntVar() const {
    return SYM_TENSOR::MEMSIZE+2+viscoPlasticity->nIntPar();
  }
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 4;}
  unsigned int getIntVar(const std::string& str) const {
    if (str == "PSTN")
      return 0;
    else if (str == "EPLS")
      return 1;
    else if (str == "ENRG")
      return 2;
    else if (str == "PNRG")
      return 3;
    else
      return 4;
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_SYM_TENSOR;
        break;
      case 1:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      case 2:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      case 3:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      default:
        return ConstitutiveModel::TYPE_NONE;
        break;
    }
  }
  unsigned int indexIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return 0;
        break;
      case 1:
        return SYM_TENSOR::MEMSIZE;
        break;
      case 2:
        return SYM_TENSOR::MEMSIZE+1;
        break;
      case 3:
        return SYM_TENSOR::MEMSIZE+2;
        break;
      default:
        return SYM_TENSOR::MEMSIZE+3;
        break;
    }
  }
  std::string labelIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "plastic strain";
        break;
      case 1:
        return "equivalent plastic strain";
        break;
      case 2:
        return "elastically stored energy";
        break;
      case 3:
        return "plastically stored energy";
        break;
      default:
        return "";
        break;
    }
  }

 protected:

  // compute the plastic update
  double plasticUpdate(const MaterialProperties& material,const ParameterSet& extPar,
                       const SYM_TENSOR& eps,SYM_TENSOR& sig,
                       const SYM_TENSOR& epsPl0,SYM_TENSOR& epsPl,
                       const MatLibArray& intV0,MatLibArray& intV,double dTime,
                       SYM_TENSOR4& M,bool update,bool computeTangent) 
   throw (UpdateFailedException) {
    
    static const double ONE_THIRD = 1./3.;
    static const double TWO_THIRD = 2./3.;
    
    SYM_TENSOR epsEl,sigDev,dSig,Mp,backStress;
    
    // extract equivalent plastic strain
    double ePl0 = intV0[0];
    double ePl  = intV[0];
    
    // extract internal parameters
    unsigned int nIntPar = intV.size()-2;
    const MatLibArray intPar0(intV0,nIntPar,2);
    MatLibArray intPar(intV,nIntPar,2);
    
    // get shear modulus
    double mu = material.getDoubleProperty("SHEAR_MODULUS");
    double mu2=2*mu,mu3=3*mu;
    
    // get kinematic hardening modulus
    double Hk = material.getDoubleProperty("KINEMATIC_HARDENING_MODULUS");
    
    // compute elastic predictor
    double norm0=0.0e0,coef=0.0e0;
    if (update || computeTangent) {
      
      epsEl = eps-epsPl0;
      this->storedEnergy(material,extPar,epsEl,sig,M,true,false);
      
      // compute stress deviator
      static const SYM_TENSOR I = SYM_TENSOR::identity();
      double p = trace(sig);
      sigDev = sig-(ONE_THIRD*p)*I;
      
      // compute initial backstress
      backStress = (TWO_THIRD*Hk)*contravariant(epsPl0);
      
      // compute radial return direction
      dSig = sigDev-backStress;
      norm0 = innerProd2(dSig,dSig);
      if (norm0 >= 1.e-16) coef = std::sqrt(1.5/norm0);
      Mp = coef*dSig;
    }
    
    // update
    viscoPlasticity->initialize = true;
    viscoPlasticity->finalize = false;
    double dEPl=0.0e0;
    if (update) {
      // perform update (radial return)
      radialReturn(material,extPar,*viscoPlasticity,
                   intPar0,intPar,dSig,ePl0,ePl,Mp,dTime);
      
      // update internal variables
      intV[0] = ePl;
      
      // update plastic strain
      dEPl = ePl-ePl0;
      epsPl = epsPl0+dEPl*covariant(Mp);
      
      viscoPlasticity->finalize = true;
    }
    
    // elastic deformation
    epsEl = eps-epsPl;
    
    // elastic free energy
    double We = this->storedEnergy(material,extPar,epsEl,sig,M,
                                   update || computeTangent,
                                   computeTangent);
    if (update) intV[1] = We;
    
    // plastic free energy increment + dissipated energy
    double dummy,Hp;
    double Wp = viscoPlasticity->irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl,
                                                    dummy,Hp,dTime,false,computeTangent);
    // compute backstress and contribution to free energy
    backStress = (TWO_THIRD*Hk)*contravariant(epsPl);
    double WpKin = 0.5*innerProd(backStress,epsPl);
    intPar[0] += WpKin;
    
    // tangents
    dEPl = ePl-ePl0;
    if (computeTangent && dEPl > 0.0e0) {
      // (visco)plastic correction
      static const SYM_TENSOR4 II = SYM_TENSOR4::contravariantIdentity();
      static const SYM_TENSOR4 KK = SYM_TENSOR4::baseK();
      double coef1 = mu2*dEPl*coef;
      double coef2 = 4*ONE_THIRD*mu*(1.0e0/(1.0e0+(Hp+Hk)/mu3)-coef1);
      M -= ((coef1*mu2)*(II-KK)+coef2*outerProd(Mp,Mp));
    }
    
    return We-intV0[1]+Wp+WpKin;
  }

 public:

  // radial return algorithm
  static unsigned int radialReturn(const MaterialProperties& material,const ParameterSet& extPar,
                                   ViscoPlasticitySimple& viscoPlasticity,
                                   const MatLibArray& intPar0,MatLibArray& intPar,
                                   const SYM_TENSOR& sigDev,double ePl0,double& ePl,
                                   const SYM_TENSOR& Mp,double dTime) 
   throw (UpdateFailedException) {

    static const unsigned int ITMAX = 20;
    static const double MULT = 0.9;
    static const double PREC = 1.e-14;
    static const double TOLE = 1.e-7;
    static const double THRSHLD = std::numeric_limits<double>::max();
    
    // compute test function
    double sigEq = innerProd2(sigDev,Mp);
    ePl = ePl0;
    double sigPl,Hp;
    viscoPlasticity.irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl,
                                       sigPl,Hp,dTime,true,true);
    double fct = sigEq-sigPl;
    if (fct <= 0.0e0) return 0;
    
    // apply plastic corrector
    double mu = material.getDoubleProperty("SHEAR_MODULUS");
    double Hk = material.getDoubleProperty("KINEMATIC_HARDENING_MODULUS");
    double mu3h=3*mu+Hk;
    double dEPl = 0.0e0;
    double test = TOLE*(fct+TOLE);
    unsigned int iter=0;
    for (; iter < ITMAX; iter++) {
      double coef = mu3h+Hp;
      if (coef < THRSHLD)
        dEPl = fct/coef;
      else
        dEPl = fct/mu3h;
      if ((ePl+dEPl) < (ePl0+PREC)) dEPl = -MULT*(ePl-ePl0);
      if (std::fabs(dEPl) < PREC) break;
      sigEq -= dEPl*mu3h;
      ePl += dEPl;
      viscoPlasticity.irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl,
                                         sigPl,Hp,dTime,true,true);
      fct = sigEq-sigPl;
      if (std::fabs(fct) < test) break;
    }
    // check convergence
    if (iter == ITMAX) {
      throw UpdateFailedException("no convergence in radial return");
    }
    
    return iter;
  }
};


/**
 * Implementations of the model.
 */
class LinearMixedJ2Plasticity3D : public J2PlasticityMixed<TensorAlgebra3D> {
  
 public:
  
  // constructor
  LinearMixedJ2Plasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>()),
    J2PlasticityMixed<TensorAlgebra3D>(new StdViscoPlasticitySimple(new LinearIsotropicHardeningModel(),
                                                                    new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearMixedJ2Plasticity3D(const LinearMixedJ2Plasticity3D& src) 
  : Elasticity<TensorAlgebra3D>(src), ElastoPlasticity<TensorAlgebra3D>(src),
    J2PlasticityMixed<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~LinearMixedJ2Plasticity3D() {}
};
class LinearMixedJ2Plasticity2D : public J2PlasticityMixed<TensorAlgebra2D> {
  
 public:
  
  // constructor
  LinearMixedJ2Plasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>()),
    J2PlasticityMixed<TensorAlgebra2D>(new StdViscoPlasticitySimple(new LinearIsotropicHardeningModel(),
                                                                    new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearMixedJ2Plasticity2D(const LinearMixedJ2Plasticity2D& src) 
  : Elasticity<TensorAlgebra2D>(src), ElastoPlasticity<TensorAlgebra2D>(src),
    J2PlasticityMixed<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~LinearMixedJ2Plasticity2D() {}
};
class LinearMixedJ2Plasticity1D : public J2PlasticityMixed<TensorAlgebra1D> {
  
 public:
  
  // constructor
  LinearMixedJ2Plasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>()),
    J2PlasticityMixed<TensorAlgebra1D>(new StdViscoPlasticitySimple(new LinearIsotropicHardeningModel(),
                                                                    new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearMixedJ2Plasticity1D(const LinearMixedJ2Plasticity1D& src) 
  : Elasticity<TensorAlgebra1D>(src), ElastoPlasticity<TensorAlgebra1D>(src),
    J2PlasticityMixed<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~LinearMixedJ2Plasticity1D() {}
};

/**
 * The associated model builder
 */
class LinearMixedJ2PlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  LinearMixedJ2PlasticityBuilder();
  
  // the instance
  static LinearMixedJ2PlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~LinearMixedJ2PlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
