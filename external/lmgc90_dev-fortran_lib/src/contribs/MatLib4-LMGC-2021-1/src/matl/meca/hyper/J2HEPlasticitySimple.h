/*
 *  $Id: J2HEPlasticitySimple.h 141 2014-01-27 20:57:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_J2_PLASTICITY_SIMPLE_H
#define ZORGLIB_MATL_MECA_HYPER_J2_PLASTICITY_SIMPLE_H

// config
#include <matlib_macros.h>

// local
#include "matl/meca/hyper/GeneralHenckyPotential.h"
#include "matl/meca/hyper/HyperElastoPlasticity.h"
#include "matl/meca/linear/J2PlasticitySimple.h"
#include <matl/meca/linear/ViscoPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * J2 plasticity with pure isotropic hardening.
 * (shortcuts the generic J2HEPlasticity for efficiency)
 */
template <class ALG>
class J2HEPlasticitySimple : virtual public HyperElastoPlasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  
 protected:
    
  ViscoPlasticitySimple *viscoPlasticity;
  
  // empty constructor
  J2HEPlasticitySimple(ViscoPlasticitySimple *vp = 0) {
    viscoPlasticity = vp;
  }
  
 public:
    
  // constructors
  J2HEPlasticitySimple(ViscoPlasticitySimple& vp)
  : HyperElasticity<ALG>(new GeneralHenckyPotential<ALG>( // the model MUST be elastically isotropic
				    new IsotropicElasticPotential<ALG>())) {
    viscoPlasticity = &vp;
  }
  J2HEPlasticitySimple(ViscoPlasticitySimple& vp,EOS& e)
  : HyperElasticity<ALG>(new GeneralHenckyPotential<ALG>(
                                    new IsotropicElasticPotential<ALG>()),
                                    // the model MUST be elastically isotropic
                         e) {viscoPlasticity = &vp;}
  
  // copy constructor
  J2HEPlasticitySimple(const J2HEPlasticitySimple& src)
  : HyperElasticity<ALG>(src),HyperElastoPlasticity<ALG>(src) {
    viscoPlasticity = src.viscoPlasticity;
  }
  
  // destructor
  virtual ~J2HEPlasticitySimple() {
    if (*(this->count) > 1) return;
    if (viscoPlasticity) delete viscoPlasticity;
  }
  
  // check consistency of properties
  void checkProperties(MaterialProperties& material,std::ostream *os)
   throw (InvalidPropertyException, NoSuchPropertyException) {
     if (os) (*os) << "\nJ2 plasticity model with isotropic hardening (hyperelastic base):" << std::endl;
      
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
    
    // equation-of-state
    if (this->eos) this->eos->checkProperties(material,os);
    
    // dilatancy model
    if (this->dilatancy) this->dilatancy->checkProperties(material,os);
    
    // viscoplastic model
    viscoPlasticity->checkProperties(material,os);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    HyperElasticity<ALG>::updateProperties(mater,extPar);
    viscoPlasticity->updateProperties(mater,extPar);
  }
  
  // number of internal variables
  unsigned int nIntVar() const {
    return TENSOR::MEMSIZE+2+viscoPlasticity->nIntPar();
  }
  
  // self-documenting utilities
  virtual unsigned int nIntVarBundled() const {return 4;}
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
        return ConstitutiveModel::TYPE_TENSOR;
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
        return TENSOR::MEMSIZE;
        break;
      case 2:
        return TENSOR::MEMSIZE+1;
        break;
      case 3:
        return TENSOR::MEMSIZE+2;
        break;
      default:
        return TENSOR::MEMSIZE+3;
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

  // compute the plastic update
  double plasticUpdate(const MaterialProperties& material,const ParameterSet& extPar,
                       const SYM_TENSOR& C,SYM_TENSOR& S,const TENSOR& Fp0,TENSOR& Fp,
                       const MatLibArray& intV0,MatLibArray& intV,double dTime,
                       SYM_TENSOR4& M,bool update,bool computeTangent)
   throw (UpdateFailedException) {

    static const double ONE_THIRD = 1./3.;
    static const SYM_TENSOR I = SYM_TENSOR::identity();

    SYM_TENSOR Ce,epsDev,sigDev,Mp;
    bool linearized = false;
    
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
      
      linearized = (dynamic_cast<GeneralHenckyPotential<ALG>*>(this->potential))->isLinearized();
      
      Ce = covariantPush(C,Fp0);
      SYM_TENSOR epsEl;
      if (!linearized)
        epsEl = 0.5*log(Ce);
      else
        epsEl = 0.5*(Ce-I);
      
      // compute strain deviator
      double dVol = trace(epsEl);
      epsDev = epsEl-ONE_THIRD*dVol*I;

      // compute stress deviator
      sigDev = mu2*epsDev;
      
      // compute radial return direction
      norm0 = innerProd2(sigDev,sigDev);
      if (norm0 >= 1.e-16) coef = sqrt(1.5/norm0);
      Mp = coef*sigDev;
    }
    
    // update
    viscoPlasticity->initialize = true;
    viscoPlasticity->finalize = false;
    double dEPl=0.0e0;
    if (update) {
      
      // perform update (radial return)
      J2PlasticitySimple<ALG>::radialReturn(material,extPar,*viscoPlasticity,
                                            intPar0,intPar,sigDev,ePl0,ePl,Mp,
                                            dTime);
      
      // update internal variables
      intV[0] = ePl;
      
      // update plastic strain
      dEPl = ePl-ePl0;
      SYM_TENSOR dFp = dEPl*Mp;
      Fp = exp(dFp)*Fp0;
      
      viscoPlasticity->finalize = true;
    }
    
    // elastic deformation
    Ce = covariantPush(C,Fp);
    
    // elastic free energy
    SYM_TENSOR Sbar;
    SYM_TENSOR4 Mbar;
    double We = this->storedEnergy(material,extPar,Ce,Sbar,Mbar,
                                   update,computeTangent);
    if (update) intV[1] = We;

    // plastic free energy + dissipated energy
    double dummy,Hp;
    double Wp = viscoPlasticity->irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl,
                                                    dummy,Hp,dTime,false,computeTangent);

    // stresses
    if (update) {
      S = contravariantPull(Sbar,Fp);
    }
    
    // tangents
    if (computeTangent) {
      
      dEPl = ePl-ePl0;
      if (dEPl > 0.0e0) {
        
        // (visco)plastic correction
        static const SYM_TENSOR4 II = SYM_TENSOR4::contravariantIdentity();
        static const SYM_TENSOR4 KK = SYM_TENSOR4::baseK();
        double coef1 = mu2*dEPl*coef;
        double coef2 = 4*ONE_THIRD*mu*(1.0e0/(1.0e0+Hp/mu3)-coef1);
        SYM_TENSOR4 Mcor;
        Mcor = (coef1*mu2)*(II-KK)+coef2*outerProd(Mp,Mp);
        
        if (!linearized) {
          SYM_TENSOR dLogC[6];
          Ce.log(dLogC,0,true,false);
          // apply plastic correction in nonlinear kinematics
          unsigned int ij,kl,sz=SYM_TENSOR::MEMSIZE;
          for (ij=0; ij < sz; ij++)
            for (kl=0; kl < sz; kl++) 
              Mbar[ij][kl] -= innerProd2(dLogC[ij],innerProd2(Mcor,dLogC[kl]));
        }
        else
          // in linearized kinematics (elastic strains)
          Mbar -= Mcor;
      }
        
      M = contravariantPull(Mbar,Fp);
    }
      
    return We+Wp-intV0[1];
  }
};


/*
 * Implementations of the model.
 */

/**
 * J2 finite plasticity with linear isotropic hardening.
 */
class LinearIsotropicJ2HEPlasticity3D : public J2HEPlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2HEPlasticity3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
          new GeneralHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicElasticPotential<TensorAlgebra3D>())),
          eos),
    J2HEPlasticitySimple<TensorAlgebra3D>(new StdViscoPlasticitySimple(new LinearIsotropicHardeningModel(),
                                                                       new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicJ2HEPlasticity3D(const LinearIsotropicJ2HEPlasticity3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src), HyperElastoPlasticity<TensorAlgebra3D>(src),
    J2HEPlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2HEPlasticity3D() {}
};
class LinearIsotropicJ2HEPlasticity2D : public J2HEPlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2HEPlasticity2D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra2D>(
          new GeneralHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicElasticPotential<TensorAlgebra2D>())),
          eos),
    J2HEPlasticitySimple<TensorAlgebra2D>(new StdViscoPlasticitySimple(new LinearIsotropicHardeningModel(),
                                                                       new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicJ2HEPlasticity2D(const LinearIsotropicJ2HEPlasticity2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src), HyperElastoPlasticity<TensorAlgebra2D>(src),
    J2HEPlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2HEPlasticity2D() {}
};
class LinearIsotropicJ2HEPlasticity1D : public J2HEPlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2HEPlasticity1D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra1D>(
          new GeneralHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicElasticPotential<TensorAlgebra1D>())),
          eos),
    J2HEPlasticitySimple<TensorAlgebra1D>(new StdViscoPlasticitySimple(new LinearIsotropicHardeningModel(),
                                                                       new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicJ2HEPlasticity1D(const LinearIsotropicJ2HEPlasticity1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src), HyperElastoPlasticity<TensorAlgebra1D>(src),
    J2HEPlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2HEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class LinearIsotropicJ2HEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  LinearIsotropicJ2HEPlasticityBuilder();
  
  // the instance
  static LinearIsotropicJ2HEPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~LinearIsotropicJ2HEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * J2 plasticity with nonlinear isotropic hardening.
 */
class NonLinearIsotropicJ2HEPlasticity3D : public J2HEPlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NonLinearIsotropicJ2HEPlasticity3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
          new GeneralHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicElasticPotential<TensorAlgebra3D>())),
          eos),
    J2HEPlasticitySimple<TensorAlgebra3D>(new StdViscoPlasticitySimple(new NonLinearIsotropicHardeningModel(),
                                                                       new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  NonLinearIsotropicJ2HEPlasticity3D(const NonLinearIsotropicJ2HEPlasticity3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src), HyperElastoPlasticity<TensorAlgebra3D>(src),
    J2HEPlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NonLinearIsotropicJ2HEPlasticity3D() {}
};
class NonLinearIsotropicJ2HEPlasticity2D : public J2HEPlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  NonLinearIsotropicJ2HEPlasticity2D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra2D>(
          new GeneralHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicElasticPotential<TensorAlgebra2D>())),
          eos),
    J2HEPlasticitySimple<TensorAlgebra2D>(new StdViscoPlasticitySimple(new NonLinearIsotropicHardeningModel(),
                                                                       new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  NonLinearIsotropicJ2HEPlasticity2D(const NonLinearIsotropicJ2HEPlasticity2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src), HyperElastoPlasticity<TensorAlgebra2D>(src),
    J2HEPlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~NonLinearIsotropicJ2HEPlasticity2D() {}
};
class NonLinearIsotropicJ2HEPlasticity1D : public J2HEPlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  NonLinearIsotropicJ2HEPlasticity1D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra1D>(
          new GeneralHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicElasticPotential<TensorAlgebra1D>())),
          eos),
    J2HEPlasticitySimple<TensorAlgebra1D>(new StdViscoPlasticitySimple(new NonLinearIsotropicHardeningModel(),
                                                                       new PowerLawRateDependencyModel())) {}
  
  // copy constructor
  NonLinearIsotropicJ2HEPlasticity1D(const NonLinearIsotropicJ2HEPlasticity1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src), HyperElastoPlasticity<TensorAlgebra1D>(src),
    J2HEPlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~NonLinearIsotropicJ2HEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class NonLinearIsotropicJ2HEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  NonLinearIsotropicJ2HEPlasticityBuilder();
  
  // the instance
  static NonLinearIsotropicJ2HEPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~NonLinearIsotropicJ2HEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * J2 plasticity with nonlinear isotropic hardening + asinh rate-dependency.
 */
class NonLinearASinhIsotropicJ2HEPlasticity3D : public J2HEPlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NonLinearASinhIsotropicJ2HEPlasticity3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
          new GeneralHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicElasticPotential<TensorAlgebra3D>())),
          eos),
    J2HEPlasticitySimple<TensorAlgebra3D>(new StdViscoPlasticitySimple(new NonLinearIsotropicHardeningModel(),
                                                                       new ASinhRateDependencyModel())) {}
  
  // copy constructor
  NonLinearASinhIsotropicJ2HEPlasticity3D(const NonLinearASinhIsotropicJ2HEPlasticity3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src), HyperElastoPlasticity<TensorAlgebra3D>(src),
    J2HEPlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NonLinearASinhIsotropicJ2HEPlasticity3D() {}
};
class NonLinearASinhIsotropicJ2HEPlasticity2D : public J2HEPlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  NonLinearASinhIsotropicJ2HEPlasticity2D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra2D>(
          new GeneralHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicElasticPotential<TensorAlgebra2D>())),
          eos),
    J2HEPlasticitySimple<TensorAlgebra2D>(new StdViscoPlasticitySimple(new NonLinearIsotropicHardeningModel(),
                                                                       new ASinhRateDependencyModel())) {}
  
  // copy constructor
  NonLinearASinhIsotropicJ2HEPlasticity2D(const NonLinearASinhIsotropicJ2HEPlasticity2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src), HyperElastoPlasticity<TensorAlgebra2D>(src),
    J2HEPlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~NonLinearASinhIsotropicJ2HEPlasticity2D() {}
};
class NonLinearASinhIsotropicJ2HEPlasticity1D : public J2HEPlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  NonLinearASinhIsotropicJ2HEPlasticity1D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra1D>(
          new GeneralHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicElasticPotential<TensorAlgebra1D>())),
          eos),
    J2HEPlasticitySimple<TensorAlgebra1D>(new StdViscoPlasticitySimple(new NonLinearIsotropicHardeningModel(),
                                                                       new ASinhRateDependencyModel())) {}
  
  // copy constructor
  NonLinearASinhIsotropicJ2HEPlasticity1D(const NonLinearASinhIsotropicJ2HEPlasticity1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src), HyperElastoPlasticity<TensorAlgebra1D>(src),
    J2HEPlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~NonLinearASinhIsotropicJ2HEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class NonLinearASinhIsotropicJ2HEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  NonLinearASinhIsotropicJ2HEPlasticityBuilder();
  
  // the instance
  static NonLinearASinhIsotropicJ2HEPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~NonLinearASinhIsotropicJ2HEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * J2 plasticity with Norton-Hoff isotropic hardening + rate-dependency.
 */
class NortonHoffIsotropicJ2HEPlasticity3D : public J2HEPlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NortonHoffIsotropicJ2HEPlasticity3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
          new GeneralHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicElasticPotential<TensorAlgebra3D>())),
          eos),
    J2HEPlasticitySimple<TensorAlgebra3D>(new StdViscoPlasticitySimple(0,new NortonHoffRateDependencyModel())) {}
  
  // copy constructor
  NortonHoffIsotropicJ2HEPlasticity3D(const NortonHoffIsotropicJ2HEPlasticity3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src), HyperElastoPlasticity<TensorAlgebra3D>(src),
    J2HEPlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NortonHoffIsotropicJ2HEPlasticity3D() {}
};
class NortonHoffIsotropicJ2HEPlasticity2D : public J2HEPlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  NortonHoffIsotropicJ2HEPlasticity2D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra2D>(
          new GeneralHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicElasticPotential<TensorAlgebra2D>())),
          eos),
    J2HEPlasticitySimple<TensorAlgebra2D>(new StdViscoPlasticitySimple(0,new NortonHoffRateDependencyModel())) {}
  
  // copy constructor
  NortonHoffIsotropicJ2HEPlasticity2D(const NortonHoffIsotropicJ2HEPlasticity2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src), HyperElastoPlasticity<TensorAlgebra2D>(src),
    J2HEPlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~NortonHoffIsotropicJ2HEPlasticity2D() {}
};
class NortonHoffIsotropicJ2HEPlasticity1D : public J2HEPlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  NortonHoffIsotropicJ2HEPlasticity1D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra1D>(
          new GeneralHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicElasticPotential<TensorAlgebra1D>())),
          eos),
    J2HEPlasticitySimple<TensorAlgebra1D>(new StdViscoPlasticitySimple(0,new NortonHoffRateDependencyModel())) {}
  
  // copy constructor
  NortonHoffIsotropicJ2HEPlasticity1D(const NortonHoffIsotropicJ2HEPlasticity1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src), HyperElastoPlasticity<TensorAlgebra1D>(src),
    J2HEPlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~NortonHoffIsotropicJ2HEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class NortonHoffIsotropicJ2HEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  NortonHoffIsotropicJ2HEPlasticityBuilder();
  
  // the instance
  static NortonHoffIsotropicJ2HEPlasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~NortonHoffIsotropicJ2HEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
