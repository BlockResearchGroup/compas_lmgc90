/*
 *  $Id: J2ThermoHEPlasticitySimple.h 153 2014-10-03 09:12:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_HYPER_J2_PLASTICITY_SIMPLE_H
#define ZORGLIB_MATL_MECA_THERMO_HYPER_J2_PLASTICITY_SIMPLE_H

// config
#include <matlib_macros.h>

// local
#include "matl/thermomeca/hyper/GeneralThermalHenckyPotential.h"
#include "matl/thermomeca/hyper/ThermoHyperElastoPlasticity.h"
#include "matl/thermomeca/linear/J2ThermoPlasticitySimple.h"
#include <matl/thermomeca/linear/ThermoViscoPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Finite J2 plasticity with pure isotropic hardening.
 */
template <class ALG>
class J2ThermoHEPlasticitySimple : virtual public ThermoHyperElastoPlasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  
 protected:
    
  ThermoViscoPlasticitySimple *viscoPlasticity;
  
  // empty constructor
  J2ThermoHEPlasticitySimple(ThermoViscoPlasticitySimple *vp = 0) {
    viscoPlasticity = vp;
  }
  
 public:
    
  // constructor
  J2ThermoHEPlasticitySimple(ThermoViscoPlasticitySimple& vp)
  : ThermoHyperElasticity<ALG>(new GeneralThermalHenckyPotential<ALG>(
                                        new IsotropicElasticPotential<ALG>())) {
    viscoPlasticity = &vp;              // the model MUST be elastically isotropic
  }
  
  // copy constructor
  J2ThermoHEPlasticitySimple(const J2ThermoHEPlasticitySimple& src)
  : ThermoHyperElasticity<ALG>(src),ThermoHyperElastoPlasticity<ALG>(src) {
    viscoPlasticity = src.viscoPlasticity;
  }
  
  // destructor
  virtual ~J2ThermoHEPlasticitySimple() {
    if (*(this->count) > 1) return;
    if (viscoPlasticity) delete viscoPlasticity;
  }
  
  // check consistency of properties
  void checkProperties(MaterialProperties& material,std::ostream *os)
   throw (InvalidPropertyException, NoSuchPropertyException) {
     if (os) (*os) << "\nJ2 thermo-plasticity model with isotropic hardening (hyperelastic base):" << std::endl;
      
    // density
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      if (os) (*os) << "\n\tmass density = " << rho << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tmass density is not defined" << std::endl;
    }

    // elastic potential and equation-of-state
    this->potential->checkProperties(material,os);
    
    // eos
    if (this->eos) this->eos->checkProperties(material,os);
    
    // check capacity
    this->capacity->checkProperties(material,os);
    
    // dilatancy model
    if (this->dilatancy) this->dilatancy->checkProperties(material,os);
    
    // viscoplastic model
    viscoPlasticity->checkProperties(material,os);

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
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    this->potential->updateProperties(mater,extPar);
    if (this->eos) this->eos->updateProperties(mater,extPar);
    this->capacity->updateProperties(mater,extPar);
    if (this->dilatancy) this->dilatancy->updateProperties(mater,extPar);
    viscoPlasticity->updateProperties(mater,extPar);
  }
  
  // number of internal variables
  unsigned int nIntVar() const {
    return TENSOR::MEMSIZE+4+viscoPlasticity->nIntPar();
  }
  
  // self-documenting utilities
  virtual unsigned int nIntVarBundled() const {return 6;}
  unsigned int getIntVar(const std::string& str) const {
    if (str == "PSTN")
      return 0;
    else if (str == "EPLS")
      return 1;
    else if (str == "ENTP")
      return 2;
    else if (str == "ENRG")
      return 3;
    else if (str == "TNRG")
      return 4;
    else if (str == "PNRG")
      return 5;
    else
      return 6;
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
      case 4:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      case 5:
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
      case 4:
        return TENSOR::MEMSIZE+3;
        break;
      case 5:
        return TENSOR::MEMSIZE+4;
        break;
      default:
        return TENSOR::MEMSIZE+5;
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
        return "entropy";
        break;
      case 3:
        return "elastically stored energy";
        break;
      case 4:
        return "thermally stored energy";
        break;
      case 5:
        return "plastically stored energy";
        break;
      default:
        return "";
        break;
    }
  }

  // compute the plastic update
  double plasticUpdate(const MaterialProperties& material,const ParameterSet& extPar,
                       const SYM_TENSOR& C,SYM_TENSOR& S,double T0,double T1,
                       double& dN,const TENSOR& Fp0,TENSOR& Fp,
                       const MatLibArray& intV0,MatLibArray& intV,double dTime,
                       SYM_TENSOR4& M,SYM_TENSOR& dS,double& Cm,
                       bool update,bool computeTangent)
   throw (UpdateFailedException) {

    static const double ONE_THIRD = 1./3.;
    static const SYM_TENSOR I = SYM_TENSOR::identity();

    double N;
    SYM_TENSOR Ce,epsDev,sigDev,Mp;
    bool linearized = false;
    
    // temperature increments
    double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    double Th0 = T0-TRef;
    double Th1 = T1-TRef;

    // extract equivalent plastic strain
    double ePl0 = intV0[0];
    double ePl  = intV[0];
    
    // extract internal parameters
    unsigned int nIntPar = intV.size()-4;
    const MatLibArray intPar0(intV0,nIntPar,4);
    MatLibArray intPar(intV,nIntPar,4);

    // get shear modulus
    double mu,dmu;
    try {
      double E,dE,nu,dnu;
      try { // get Young's modulus
        Function& fctE = material.getFunctionProperty("YOUNG_MODULUS_EVOLUTION");
        E = fctE.value(T1,dE);
      }
      catch (NoSuchPropertyException) {
        E = material.getDoubleProperty("YOUNG_MODULUS");
        dE = 0.0e0;
      }
      if (E < 0.e0) throw InvalidPropertyException("Young's modulus");

      try { // get Poisson's coefficient
        Function& fctN = material.getFunctionProperty("POISSON_COEFFICIENT_EVOLUTION");
        nu = fctN.value(T1,dnu);
      }
      catch (NoSuchPropertyException) {
        nu = material.getDoubleProperty("POISSON_COEFFICIENT");
        dnu = 0.0e0;
      }
      if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException("Poisson's coefficient");
      mu = 0.5*E/(1.+nu);
      dmu = 0.5*(dE-E*dnu/(1.+nu))/(1.+nu);
    }
    catch (NoSuchPropertyException) {
      mu = material.getDoubleProperty("SHEAR_MODULUS");
      dmu = 0.0e0;
    }
    double mu2=2*mu,mu3=3*mu;
    
    // compute elastic predictor
    double norm0=0.0e0,coef=0.0e0;
    if (update || computeTangent) {
      
      linearized = (dynamic_cast<GeneralThermalHenckyPotential<ALG>*>(this->potential))->isLinearized();
      
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
      J2ThermoPlasticitySimple<ALG>::radialReturn(material,extPar,*viscoPlasticity,
                                                  intPar0,intPar,sigDev,ePl0,ePl,Mp,
                                                  Th0,Th1,T0,T1,dTime);
      
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
    SYM_TENSOR Sbar,dSbar;
    SYM_TENSOR4 Mbar;
    double W = this->storedEnergy(material,extPar,Ce,T1,Sbar,N,Mbar,dSbar,Cm,
				  update,computeTangent);
    if (update) intV[2] = W;
    
    // thermal capacity
    if (this->capacity) {
      double NT,CT;
      double WT = this->capacity->internalEnergy(material,extPar,T1,NT,
                                                 CT,update,computeTangent);
      W += WT;
      if (update) {
        intV[3] = WT;
        N += NT;
      }
      if (computeTangent) Cm += CT;
    }
    
    // plastic free energy + dissipated energy
    double dummy,Np,dNp,Hp,dSigPl,Cp;
    double Wp = viscoPlasticity->irreversibleEnergy(material,extPar,
                                                    intPar0,intPar,ePl0,ePl,Th0,Th1,
                                                    T0,T1,dummy,Np,dNp,Hp,dSigPl,Cp,
                                                    dTime,update,computeTangent);

    // update
    if (update) {
      dN = N+dNp+intV0[1];
      intV[1] = -N-Np;
    }
    
    // stresses
    if (update) {
      S = contravariantPull(Sbar,Fp);
    }
    
    // tangents
    if (computeTangent) {
      
      // capacity
      Cm += Cp;
      
      // (visco)plastic correction
      dEPl = ePl-ePl0;
      if (dEPl > 0.0e0) {
        
        // (visco)plastic correction
        static const SYM_TENSOR4 II = SYM_TENSOR4::contravariantIdentity();
        static const SYM_TENSOR4 KK = SYM_TENSOR4::baseK();
        double coef1 = mu2*dEPl*coef;
        double coef2 = mu2/(mu3+Hp);
        double coef3 = 4*ONE_THIRD*mu*(1.5e0*coef2-coef1);
        SYM_TENSOR4 Mcor;
        Mcor = (coef1*mu2)*(II-KK)+coef3*outerProd(Mp,Mp);

        SYM_TENSOR dScor;
        //double dEPldT = dSigPl-3*dmu*dEPl*(1.0e0/coef1-1.0e0);
        double dEPldT = dSigPl-3*dmu*(1.0e0/(mu2*coef)-dEPl);
        dScor = coef2*dEPldT*Mp;
        
        // capacity correction
        Cm -= dEPldT*dEPldT/(mu3+Hp);
        
        // geometrical effects
        if (!linearized) {
          SYM_TENSOR dLogC[6];
          Ce.log(dLogC,0,true,false);
          // apply plastic correction in nonlinear kinematics
          unsigned int ij,kl,sz=SYM_TENSOR::MEMSIZE;
          for (ij=0; ij < sz; ij++)
            for (kl=0; kl < sz; kl++) 
              Mbar[ij][kl] -= innerProd2(dLogC[ij],innerProd2(Mcor,dLogC[kl]));
          
          // thermoelastic terms
          for (ij=0; ij < sz; ij++) dSbar[ij] += innerProd2(dScor,dLogC[ij]);
        }
        else {
          // in linearized kinematics (elastic strains)
          Mbar -= Mcor;
          dSbar += dScor;
        }
      }
        
      M = contravariantPull(Mbar,Fp);
      dS = contravariantPull(dSbar,Fp);
    }

    return W+Wp-intV0[2]-intV0[3]+intV0[1]*(T1-T0);
  }
};


/*
 * Implementations of the model.
 */

/**
 * J2 finite thermoplasticity with linear isotropic hardening.
 */
class LinearIsotropicJ2ThermoHEPlasticity3D : public J2ThermoHEPlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2ThermoHEPlasticity3D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra3D>(
          new GeneralThermalHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra3D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra3D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra3D>(
          new StdThermoViscoPlasticitySimple(new ThermalLinearIsotropicHardeningModel(),
                                             new ThermalPowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicJ2ThermoHEPlasticity3D(const LinearIsotropicJ2ThermoHEPlasticity3D& src) 
  : ThermoHyperElasticity<TensorAlgebra3D>(src), ThermoHyperElastoPlasticity<TensorAlgebra3D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2ThermoHEPlasticity3D() {}
};
class LinearIsotropicJ2ThermoHEPlasticity2D : public J2ThermoHEPlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2ThermoHEPlasticity2D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra2D>(
          new GeneralThermalHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra2D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra2D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra2D>(
          new StdThermoViscoPlasticitySimple(new ThermalLinearIsotropicHardeningModel(),
                                             new ThermalPowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicJ2ThermoHEPlasticity2D(const LinearIsotropicJ2ThermoHEPlasticity2D& src) 
  : ThermoHyperElasticity<TensorAlgebra2D>(src), ThermoHyperElastoPlasticity<TensorAlgebra2D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2ThermoHEPlasticity2D() {}
};
class LinearIsotropicJ2ThermoHEPlasticity1D : public J2ThermoHEPlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2ThermoHEPlasticity1D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra1D>(
          new GeneralThermalHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra1D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra1D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra1D>(
          new StdThermoViscoPlasticitySimple(new ThermalLinearIsotropicHardeningModel(),
                                             new ThermalPowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicJ2ThermoHEPlasticity1D(const LinearIsotropicJ2ThermoHEPlasticity1D& src) 
  : ThermoHyperElasticity<TensorAlgebra1D>(src), ThermoHyperElastoPlasticity<TensorAlgebra1D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class LinearIsotropicJ2ThermoHEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  LinearIsotropicJ2ThermoHEPlasticityBuilder();
  
  // the instance
  static LinearIsotropicJ2ThermoHEPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~LinearIsotropicJ2ThermoHEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * J2 plasticity with nonlinear isotropic hardening.
 */
class NonLinearIsotropicJ2ThermoHEPlasticity3D : public J2ThermoHEPlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NonLinearIsotropicJ2ThermoHEPlasticity3D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra3D>(
          new GeneralThermalHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra3D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra3D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra3D>(
          new StdThermoViscoPlasticitySimple(new ThermalNonLinearIsotropicHardeningModel(),
                                             new ThermalPowerLawRateDependencyModel())) {}
  
  // copy constructor
  NonLinearIsotropicJ2ThermoHEPlasticity3D(const NonLinearIsotropicJ2ThermoHEPlasticity3D& src) 
  : ThermoHyperElasticity<TensorAlgebra3D>(src), ThermoHyperElastoPlasticity<TensorAlgebra3D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NonLinearIsotropicJ2ThermoHEPlasticity3D() {}
};
class NonLinearIsotropicJ2ThermoHEPlasticity2D : public J2ThermoHEPlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  NonLinearIsotropicJ2ThermoHEPlasticity2D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra2D>(
          new GeneralThermalHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra2D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra2D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra2D>(
          new StdThermoViscoPlasticitySimple(new ThermalNonLinearIsotropicHardeningModel(),
                                             new ThermalPowerLawRateDependencyModel())) {}
  
  // copy constructor
  NonLinearIsotropicJ2ThermoHEPlasticity2D(const NonLinearIsotropicJ2ThermoHEPlasticity2D& src) 
  : ThermoHyperElasticity<TensorAlgebra2D>(src), ThermoHyperElastoPlasticity<TensorAlgebra2D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~NonLinearIsotropicJ2ThermoHEPlasticity2D() {}
};
class NonLinearIsotropicJ2ThermoHEPlasticity1D : public J2ThermoHEPlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  NonLinearIsotropicJ2ThermoHEPlasticity1D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra1D>(
          new GeneralThermalHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra1D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra1D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra1D>(
          new StdThermoViscoPlasticitySimple(new ThermalNonLinearIsotropicHardeningModel(),
                                             new ThermalPowerLawRateDependencyModel())) {}
  
  // copy constructor
  NonLinearIsotropicJ2ThermoHEPlasticity1D(const NonLinearIsotropicJ2ThermoHEPlasticity1D& src) 
  : ThermoHyperElasticity<TensorAlgebra1D>(src), ThermoHyperElastoPlasticity<TensorAlgebra1D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~NonLinearIsotropicJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class NonLinearIsotropicJ2ThermoHEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  NonLinearIsotropicJ2ThermoHEPlasticityBuilder();
  
  // the instance
  static NonLinearIsotropicJ2ThermoHEPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~NonLinearIsotropicJ2ThermoHEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * J2 plasticity with nonlinear isotropic hardening and asinh rate-dependency.
 */
class NonLinearASinhIsotropicJ2ThermoHEPlasticity3D
  : public J2ThermoHEPlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NonLinearASinhIsotropicJ2ThermoHEPlasticity3D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra3D>(
          new GeneralThermalHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra3D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra3D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra3D>(
          new StdThermoViscoPlasticitySimple(new ThermalNonLinearIsotropicHardeningModel(),
                                             new ThermalASinhRateDependencyModel())) {}
  
  // copy constructor
  NonLinearASinhIsotropicJ2ThermoHEPlasticity3D(const NonLinearASinhIsotropicJ2ThermoHEPlasticity3D& src) 
  : ThermoHyperElasticity<TensorAlgebra3D>(src), 
    ThermoHyperElastoPlasticity<TensorAlgebra3D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NonLinearASinhIsotropicJ2ThermoHEPlasticity3D() {}
};
class NonLinearASinhIsotropicJ2ThermoHEPlasticity2D : public J2ThermoHEPlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  NonLinearASinhIsotropicJ2ThermoHEPlasticity2D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra2D>(
          new GeneralThermalHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra2D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra2D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra2D>(
          new StdThermoViscoPlasticitySimple(new ThermalNonLinearIsotropicHardeningModel(),
                                             new ThermalASinhRateDependencyModel())) {}
  
  // copy constructor
  NonLinearASinhIsotropicJ2ThermoHEPlasticity2D(const NonLinearASinhIsotropicJ2ThermoHEPlasticity2D& src) 
  : ThermoHyperElasticity<TensorAlgebra2D>(src), 
    ThermoHyperElastoPlasticity<TensorAlgebra2D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~NonLinearASinhIsotropicJ2ThermoHEPlasticity2D() {}
};
class NonLinearASinhIsotropicJ2ThermoHEPlasticity1D : public J2ThermoHEPlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  NonLinearASinhIsotropicJ2ThermoHEPlasticity1D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra1D>(
          new GeneralThermalHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra1D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra1D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra1D>(
          new StdThermoViscoPlasticitySimple(new ThermalNonLinearIsotropicHardeningModel(),
                                             new ThermalASinhRateDependencyModel())) {}
  
  // copy constructor
  NonLinearASinhIsotropicJ2ThermoHEPlasticity1D(const NonLinearASinhIsotropicJ2ThermoHEPlasticity1D& src) 
  : ThermoHyperElasticity<TensorAlgebra1D>(src), 
    ThermoHyperElastoPlasticity<TensorAlgebra1D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~NonLinearASinhIsotropicJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class NonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  NonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder();
  
  // the instance
  static NonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~NonLinearASinhIsotropicJ2ThermoHEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};
        
        
/**
 * J2 plasticity with Norton-Hoff isotropic hardening.
 */
class NortonHoffIsotropicJ2ThermoHEPlasticity3D : public J2ThermoHEPlasticitySimple<TensorAlgebra3D> {

 public:

  // constructor
  NortonHoffIsotropicJ2ThermoHEPlasticity3D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra3D>(
          new GeneralThermalHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra3D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra3D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra3D>(
          new StdThermoViscoPlasticitySimple(0,new ThermalNortonHoffRateDependencyModel())) {}

  // copy constructor
  NortonHoffIsotropicJ2ThermoHEPlasticity3D(const NortonHoffIsotropicJ2ThermoHEPlasticity3D& src)
  : ThermoHyperElasticity<TensorAlgebra3D>(src), ThermoHyperElastoPlasticity<TensorAlgebra3D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra3D>(src) {}

  // destructor
  virtual ~NortonHoffIsotropicJ2ThermoHEPlasticity3D() {}
};
class NortonHoffIsotropicJ2ThermoHEPlasticity2D : public J2ThermoHEPlasticitySimple<TensorAlgebra2D> {

 public:

  // constructor
  NortonHoffIsotropicJ2ThermoHEPlasticity2D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra2D>(
          new GeneralThermalHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra2D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra2D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra2D>(
          new StdThermoViscoPlasticitySimple(0,new ThermalNortonHoffRateDependencyModel())) {}

  // copy constructor
  NortonHoffIsotropicJ2ThermoHEPlasticity2D(const NortonHoffIsotropicJ2ThermoHEPlasticity2D& src)
  : ThermoHyperElasticity<TensorAlgebra2D>(src), ThermoHyperElastoPlasticity<TensorAlgebra2D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra2D>(src) {}

  // destructor
  virtual ~NortonHoffIsotropicJ2ThermoHEPlasticity2D() {}
};
class NortonHoffIsotropicJ2ThermoHEPlasticity1D : public J2ThermoHEPlasticitySimple<TensorAlgebra1D> {

 public:

  // constructor
  NortonHoffIsotropicJ2ThermoHEPlasticity1D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra1D>(
          new GeneralThermalHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra1D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra1D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra1D>(
          new StdThermoViscoPlasticitySimple(0,new ThermalNortonHoffRateDependencyModel())) {}

  // copy constructor
  NortonHoffIsotropicJ2ThermoHEPlasticity1D(const NortonHoffIsotropicJ2ThermoHEPlasticity1D& src)
  : ThermoHyperElasticity<TensorAlgebra1D>(src), ThermoHyperElastoPlasticity<TensorAlgebra1D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra1D>(src) {}

  // destructor
  virtual ~NortonHoffIsotropicJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class NortonHoffIsotropicJ2ThermoHEPlasticityBuilder : public ModelBuilder {

 private:

  // constructor
  NortonHoffIsotropicJ2ThermoHEPlasticityBuilder();

  // the instance
  static NortonHoffIsotropicJ2ThermoHEPlasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~NortonHoffIsotropicJ2ThermoHEPlasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
