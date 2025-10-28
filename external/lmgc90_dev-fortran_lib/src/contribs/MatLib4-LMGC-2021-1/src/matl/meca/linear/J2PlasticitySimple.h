/*
 *  $Id: J2PlasticitySimple.h 207 2016-08-19 16:52:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_J2_PLASTICITY_SIMPLE_H
#define ZORGLIB_MATL_MECA_LINEAR_J2_PLASTICITY_SIMPLE_H

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
 * J2 plasticity with pure isotropic hardening.
 */
template <class ALG>
class J2PlasticitySimple : virtual public ElastoPlasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:

  // associated visco-plasticity model
  ViscoPlasticitySimple *viscoPlasticity;

  // empty constructor
  J2PlasticitySimple(ViscoPlasticitySimple* vp = 0) {
    viscoPlasticity = vp;
  }

 public:

  // constructor
  J2PlasticitySimple(ViscoPlasticitySimple& vp)
    : Elasticity<ALG>(new IsotropicElasticPotential<ALG>()) {viscoPlasticity = &vp;}
  
  // copy constructor
  J2PlasticitySimple(const J2PlasticitySimple& src)
    : Elasticity<ALG>(src), ElastoPlasticity<ALG>(src) {viscoPlasticity = src.viscoPlasticity;}

  // destructor
  virtual ~J2PlasticitySimple() {
    if (*(this->count) > 1) return;
    if (viscoPlasticity) delete viscoPlasticity;
  }

  // check consistency of properties
  void checkProperties(MaterialProperties& material,std::ostream *os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {
     if (os) (*os) << "\nJ2 plasticity model with isotropic hardening (small strains):" << std::endl;
    
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

    // viscoplastic model
    viscoPlasticity->checkProperties(material,os);
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
    
    SYM_TENSOR epsEl,sigDev,Mp;
    
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
    
    // compute elastic predictor
    double norm0=0.0e0,coef=0.0e0;
    if (update || computeTangent) {
      
      epsEl = eps-epsPl0;
      this->storedEnergy(material,extPar,epsEl,sig,M,true,false);
      
      // compute stress deviator
      static const SYM_TENSOR I = SYM_TENSOR::identity();
      double p = trace(sig);
      sigDev = sig-ONE_THIRD*p*I;
      
      // compute radial return direction
      norm0 = innerProd2(sigDev,sigDev);
      if (norm0 >= 1.e-16) coef = std::sqrt(1.5/norm0);
      Mp = coef*sigDev;
    }
    
    // update
    viscoPlasticity->initialize = true;
    viscoPlasticity->finalize = false;
    double dEPl=0.0e0;
    if (update) {
      // perform update (radial return)
      radialReturn(material,extPar,*viscoPlasticity,
                   intPar0,intPar,sigDev,ePl0,ePl,Mp,dTime);
      
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
    
    // tangents
    dEPl = ePl-ePl0;
    if (computeTangent && dEPl > 0.0e0) {
      // (visco)plastic correction
      static const SYM_TENSOR4 II = SYM_TENSOR4::contravariantIdentity();
      static const SYM_TENSOR4 KK = SYM_TENSOR4::baseK();
      double coef1 = mu2*dEPl*coef;
      double coef2 = 4*ONE_THIRD*mu*(1.0e0/(1.0e0+Hp/mu3)-coef1);
      M -= ((coef1*mu2)*(II-KK)+coef2*outerProd(Mp,Mp));
    }
    
    return We+Wp-intV0[1];
  }
  
 public:

  // radial return algorithm
  static unsigned int radialReturn(const MaterialProperties& material,const ParameterSet& extPar,
                                   ViscoPlasticitySimple& viscoPlasticity,
                                   const MatLibArray& intPar0,MatLibArray& intPar,
                                   const SYM_TENSOR& sigDev,double ePl0,double& ePl,
                                   const SYM_TENSOR& Mp,double dTime) 
   throw (UpdateFailedException) {

    static const unsigned int ITMAX = 30;
    static const double MULT = 0.9;
    static const double PREC = 1.e-14;
    static const double TOLE = 1.e-7;
    static const double THRSHLD = 0.1*std::numeric_limits<double>::max();
     
    // get algorithmic parameter
    unsigned int maxIt;
    if (material.checkProperty("RR_MAX_ITER_PARAMETER"))
      maxIt = material.getIntegerProperty("RR_MAX_ITER_PARAMETER");
    else
      maxIt = ITMAX;

    // compute test function
    double sigEq = innerProd2(sigDev,Mp);
    ePl = ePl0;
    double sigPl,Hp;
    viscoPlasticity.irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl,
                                       sigPl,Hp,dTime,true,true);
    double fct0 = sigEq-sigPl;
    if (fct0 <= 0.0e0) return 0;
    //std::cout << 0 << "," << 0.0e0 << "," << fct0 << std::endl;
    
    // apply plastic corrector
    double mu = material.getDoubleProperty("SHEAR_MODULUS");
    double mu3=3*mu;
    /*for (int k=0; k < 10; k++) {
      viscoPlasticity.irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl0+(k+1)*2.e-5,
                                        sigPl,Hp,dTime,true,false);
      std::cout << 0 << "," << (k+1)*2.e-5 << "," << sigEq-(k+1)*2.e-5*mu3-sigPl << std::endl;
    }*/
    double dEPl = 0.0e0;
    double ePl00 = ePl0;
    double fct = fct0;
    double fct00 = fct0;
    double test = TOLE*(fct+TOLE);
    unsigned int iter=0;
    for (; iter < maxIt; iter++) {
      double coef = 1.0e0/(mu3+Hp);
      if (std::fabs(Hp) < THRSHLD && !std::isnan(coef))
        dEPl = fct*coef;
      else
        dEPl = fct/mu3;
      if (std::fabs(dEPl) < PREC) break;
      if ((ePl+dEPl) < (ePl00+PREC)) { /* use secant method */
        //std::cout << "secant" << std::endl;
        double mult = fct/(fct00-fct);
        if (mult < -MULT) mult=-MULT;
        dEPl = mult*(ePl-ePl00);
      }
      if (std::fabs(dEPl) < PREC) break;
      sigEq -= dEPl*mu3;
      ePl += dEPl;
      viscoPlasticity.irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl,
                                         sigPl,Hp,dTime,true,true);
      fct = sigEq-sigPl;
      //std::cout << iter << "," << ePl-ePl0 << "," << fct << std::endl;
      if (std::fabs(fct) < test) break;
      if (fct > 0.0e0 && dEPl < 0.0e0) {
        fct00 = fct;
        ePl00 = ePl;
      }
    }
    // check convergence
    if (iter == maxIt) {
      //std::cerr << "no convergence in radial return (after " << iter << " iterations)" << std::endl;
      throw UpdateFailedException("no convergence in radial return");
    }
    
    return iter;
  }
};


/*
 * Implementations of the model.
 */

/**
 * J2 plasticity with linear isotropic hardening.
 */
class LinearIsotropicJ2Plasticity3D : public J2PlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2Plasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>()),
    J2PlasticitySimple<TensorAlgebra3D>(new StdViscoPlasticitySimple(new LinearIsotropicHardeningModel(),
                                                                     new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicJ2Plasticity3D(const LinearIsotropicJ2Plasticity3D& src) 
  : Elasticity<TensorAlgebra3D>(src), ElastoPlasticity<TensorAlgebra3D>(src),
    J2PlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2Plasticity3D() {}
};
class LinearIsotropicJ2Plasticity2D : public J2PlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2Plasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>()),
    J2PlasticitySimple<TensorAlgebra2D>(new StdViscoPlasticitySimple(new LinearIsotropicHardeningModel(),
                                                                     new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicJ2Plasticity2D(const LinearIsotropicJ2Plasticity2D& src) 
  : Elasticity<TensorAlgebra2D>(src), ElastoPlasticity<TensorAlgebra2D>(src),
    J2PlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2Plasticity2D() {}
};
class LinearIsotropicJ2Plasticity1D : public J2PlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2Plasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>()),
    J2PlasticitySimple<TensorAlgebra1D>(new StdViscoPlasticitySimple(new LinearIsotropicHardeningModel(),
                                                                     new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicJ2Plasticity1D(const LinearIsotropicJ2Plasticity1D& src) 
  : Elasticity<TensorAlgebra1D>(src), ElastoPlasticity<TensorAlgebra1D>(src),
    J2PlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2Plasticity1D() {}
};

/**
 * The associated model builder
 */
class LinearIsotropicJ2PlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  LinearIsotropicJ2PlasticityBuilder();
  
  // the instance
  static LinearIsotropicJ2PlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~LinearIsotropicJ2PlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * J2 plasticity with nonlinear isotropic hardening.
 */
class NonLinearIsotropicJ2Plasticity3D : public J2PlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NonLinearIsotropicJ2Plasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>()),
    J2PlasticitySimple<TensorAlgebra3D>(new StdViscoPlasticitySimple(new NonLinearIsotropicHardeningModel(),
                                                                     new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  NonLinearIsotropicJ2Plasticity3D(const NonLinearIsotropicJ2Plasticity3D& src) 
  : Elasticity<TensorAlgebra3D>(src), ElastoPlasticity<TensorAlgebra3D>(src),
    J2PlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NonLinearIsotropicJ2Plasticity3D() {}
};
class NonLinearIsotropicJ2Plasticity2D : public J2PlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  NonLinearIsotropicJ2Plasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>()),
    J2PlasticitySimple<TensorAlgebra2D>(new StdViscoPlasticitySimple(new NonLinearIsotropicHardeningModel(),
                                                                     new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  NonLinearIsotropicJ2Plasticity2D(const NonLinearIsotropicJ2Plasticity2D& src) 
  : Elasticity<TensorAlgebra2D>(src), ElastoPlasticity<TensorAlgebra2D>(src),
    J2PlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~NonLinearIsotropicJ2Plasticity2D() {}
};
class NonLinearIsotropicJ2Plasticity1D : public J2PlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  NonLinearIsotropicJ2Plasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>()),
    J2PlasticitySimple<TensorAlgebra1D>(new StdViscoPlasticitySimple(new NonLinearIsotropicHardeningModel(),
                                                                     new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  NonLinearIsotropicJ2Plasticity1D(const NonLinearIsotropicJ2Plasticity1D& src) 
  : Elasticity<TensorAlgebra1D>(src), ElastoPlasticity<TensorAlgebra1D>(src),
    J2PlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~NonLinearIsotropicJ2Plasticity1D() {}
};

/**
 * The associated model builder
 */
class NonLinearIsotropicJ2PlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  NonLinearIsotropicJ2PlasticityBuilder();
  
  // the instance
  static NonLinearIsotropicJ2PlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~NonLinearIsotropicJ2PlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};
        
        
/**
 * J2 plasticity with nonlinear isotropic hardening + asinh rate-dependency.
 */
class NonLinearASinhIsotropicJ2Plasticity3D : public J2PlasticitySimple<TensorAlgebra3D> {

 public:

  // constructor
  NonLinearASinhIsotropicJ2Plasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>()),
    J2PlasticitySimple<TensorAlgebra3D>(new StdViscoPlasticitySimple(new NonLinearIsotropicHardeningModel(),
                                                                     new ASinhRateDependencyModel())) {}

  // copy constructor
  NonLinearASinhIsotropicJ2Plasticity3D(const NonLinearASinhIsotropicJ2Plasticity3D& src)
  : Elasticity<TensorAlgebra3D>(src), ElastoPlasticity<TensorAlgebra3D>(src),
    J2PlasticitySimple<TensorAlgebra3D>(src) {}

  // destructor
  virtual ~NonLinearASinhIsotropicJ2Plasticity3D() {}
};
class NonLinearASinhIsotropicJ2Plasticity2D : public J2PlasticitySimple<TensorAlgebra2D> {

 public:

  // constructor
  NonLinearASinhIsotropicJ2Plasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>()),
    J2PlasticitySimple<TensorAlgebra2D>(new StdViscoPlasticitySimple(new NonLinearIsotropicHardeningModel(),
                                                                     new ASinhRateDependencyModel())) {}

  // copy constructor
  NonLinearASinhIsotropicJ2Plasticity2D(const NonLinearASinhIsotropicJ2Plasticity2D& src)
  : Elasticity<TensorAlgebra2D>(src), ElastoPlasticity<TensorAlgebra2D>(src),
    J2PlasticitySimple<TensorAlgebra2D>(src) {}

  // destructor
  virtual ~NonLinearASinhIsotropicJ2Plasticity2D() {}
};
class NonLinearASinhIsotropicJ2Plasticity1D : public J2PlasticitySimple<TensorAlgebra1D> {

 public:

  // constructor
  NonLinearASinhIsotropicJ2Plasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>()),
    J2PlasticitySimple<TensorAlgebra1D>(new StdViscoPlasticitySimple(new NonLinearIsotropicHardeningModel(),
                                                                     new ASinhRateDependencyModel())) {}

  // copy constructor
  NonLinearASinhIsotropicJ2Plasticity1D(const NonLinearASinhIsotropicJ2Plasticity1D& src)
  : Elasticity<TensorAlgebra1D>(src), ElastoPlasticity<TensorAlgebra1D>(src),
    J2PlasticitySimple<TensorAlgebra1D>(src) {}

  // destructor
  virtual ~NonLinearASinhIsotropicJ2Plasticity1D() {}
};

/**
 * The associated model builder
 */
class NonLinearASinhIsotropicJ2PlasticityBuilder : public ModelBuilder {

 private:

  // constructor
  NonLinearASinhIsotropicJ2PlasticityBuilder();

  // the instance
  static NonLinearASinhIsotropicJ2PlasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~NonLinearASinhIsotropicJ2PlasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * J2 plasticity with Norton-Hoff isotropic hardening.
 */
class NortonHoffIsotropicJ2Plasticity3D : public J2PlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NortonHoffIsotropicJ2Plasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>()),
    J2PlasticitySimple<TensorAlgebra3D>(new StdViscoPlasticitySimple(0,new NortonHoffRateDependencyModel())) {}
  
  // copy constructor
  NortonHoffIsotropicJ2Plasticity3D(const NortonHoffIsotropicJ2Plasticity3D& src) 
  : Elasticity<TensorAlgebra3D>(src), ElastoPlasticity<TensorAlgebra3D>(src),
    J2PlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NortonHoffIsotropicJ2Plasticity3D() {}
};
class NortonHoffIsotropicJ2Plasticity2D : public J2PlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  NortonHoffIsotropicJ2Plasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>()),
    J2PlasticitySimple<TensorAlgebra2D>(new StdViscoPlasticitySimple(0,new NortonHoffRateDependencyModel())) {}
  
  // copy constructor
  NortonHoffIsotropicJ2Plasticity2D(const NortonHoffIsotropicJ2Plasticity2D& src) 
  : Elasticity<TensorAlgebra2D>(src), ElastoPlasticity<TensorAlgebra2D>(src),
    J2PlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~NortonHoffIsotropicJ2Plasticity2D() {}
};
class NortonHoffIsotropicJ2Plasticity1D : public J2PlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  NortonHoffIsotropicJ2Plasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>()),
    J2PlasticitySimple<TensorAlgebra1D>(new StdViscoPlasticitySimple(0,new NortonHoffRateDependencyModel())) {}
  
  // copy constructor
  NortonHoffIsotropicJ2Plasticity1D(const NortonHoffIsotropicJ2Plasticity1D& src) 
  : Elasticity<TensorAlgebra1D>(src), ElastoPlasticity<TensorAlgebra1D>(src),
    J2PlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~NortonHoffIsotropicJ2Plasticity1D() {}
};

/**
 * The associated model builder
 */
class NortonHoffIsotropicJ2PlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  NortonHoffIsotropicJ2PlasticityBuilder();
  
  // the instance
  static NortonHoffIsotropicJ2PlasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~NortonHoffIsotropicJ2PlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
