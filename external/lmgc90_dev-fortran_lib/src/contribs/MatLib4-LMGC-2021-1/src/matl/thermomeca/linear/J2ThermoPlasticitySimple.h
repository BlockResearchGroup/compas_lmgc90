/*
 *  $Id: J2ThermoPlasticitySimple.h 207 2016-08-19 16:52:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_LINEAR_J2_PLASTICITY_SIMPLE_H
#define ZORGLIB_MATL_MECA_THERMO_LINEAR_J2_PLASTICITY_SIMPLE_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
// std C++ library
#include <limits>
// local
#include <matl/thermomeca/linear/IsotropicThermoElasticity.h>
#include <matl/thermomeca/linear/ThermalHardeningModels.h>
#include <matl/thermomeca/linear/ThermalRateDependencyModels.h>
#include <matl/thermomeca/linear/ThermoElastoPlasticity.h>
#include <matl/thermomeca/linear/ThermoViscoPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * J2 thermo-plasticity with pure isotropic hardening.
 */
template <class ALG>
class J2ThermoPlasticitySimple : virtual public ThermoElastoPlasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:

  // associated thermo-visco-plasticity model
  ThermoViscoPlasticitySimple *viscoPlasticity;

  // empty constructor
  J2ThermoPlasticitySimple(ThermoViscoPlasticitySimple* vp = 0) {
    viscoPlasticity = vp;
  }

 public:

  // constructor
  J2ThermoPlasticitySimple(ThermoViscoPlasticitySimple& vp)
  : ThermoElasticity<ALG>(new IsotropicThermoElasticPotential<ALG>()) {viscoPlasticity = &vp;}
  
  // copy constructor
  J2ThermoPlasticitySimple(const J2ThermoPlasticitySimple& src)
  : ThermoElasticity<ALG>(src), ThermoElastoPlasticity<ALG>(src) {viscoPlasticity = src.viscoPlasticity;}

  // destructor
  virtual ~J2ThermoPlasticitySimple() {
    if (*(this->count) > 1) return;
    if (viscoPlasticity) delete viscoPlasticity;
  }

  // check consistency of properties
  void checkProperties(MaterialProperties& material,std::ostream *os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {
     if (os) (*os) << "\nJ2 thermo-plasticity model with isotropic hardening (small strains):" << std::endl;
    
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
      if (os) (*os) << "\treference temperature = " << TRef << std::endl;
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
        if (os) (*os) << "\treference temperature = " << T0 << std::endl;
      }
      catch (NoSuchPropertyException e) {
        if (os) (*os) << "ERROR: reference temperature cannot be set." << std::endl;
        throw e;
      }
    }
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    ThermoElasticity<ALG>::updateProperties(mater,extPar);
    viscoPlasticity->updateProperties(mater,extPar);
  }
  
  // number of internal variables
  unsigned int nIntVar() const {
    return SYM_TENSOR::MEMSIZE+4+viscoPlasticity->nIntPar();
  }
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 6;}
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
        return SYM_TENSOR::MEMSIZE;
        break;
      case 2:
        return SYM_TENSOR::MEMSIZE+1;
        break;
      case 3:
        return SYM_TENSOR::MEMSIZE+2;
        break;
      case 4:
        return SYM_TENSOR::MEMSIZE+3;
        break;
      case 5:
        return SYM_TENSOR::MEMSIZE+4;
        break;
      default:
        return SYM_TENSOR::MEMSIZE+5;
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

 protected:

  // compute the plastic update
  double plasticUpdate(const MaterialProperties& material,const ParameterSet& extPar,
                       const SYM_TENSOR& eps,SYM_TENSOR& sig,double Th0,double Th1,
                       double& dN,const SYM_TENSOR& epsPl0,SYM_TENSOR& epsPl,
                       const MatLibArray& intV0,MatLibArray& intV,double dTime,
                       SYM_TENSOR4& M,SYM_TENSOR& dSig,double& C,
                       bool update,bool computeTangent) 
   throw (UpdateFailedException) {
    
    static const double ONE_THIRD = 1./3.;
    
    double N;
    SYM_TENSOR epsEl,sigDev,Mp;
    
    // compute reference temperatures (after linearization)
    double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    double T0 = TRef;
    double T1 = TRef+Th1-Th0;

    // extract equivalent plastic strain
    double ePl0 = intV0[0];
    double ePl  = intV[0];
    
    // extract internal parameters
    unsigned int nIntPar = intV.size()-4;
    const MatLibArray intPar0(intV0,nIntPar,4);
    MatLibArray intPar(intV,nIntPar,4);
    
    // compute elastic predictor
    double norm0=0.0e0,coef=0.0e0;
    if (update || computeTangent) {
      
      epsEl = eps-epsPl0;
      this->storedEnergy(material,extPar,epsEl,Th1,sig,N,
                         M,dSig,C,true,false);
      
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
                   intPar0,intPar,sigDev,ePl0,ePl,Mp,
                   Th0,Th1,T0,T1,dTime);
      
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
    double W = this->storedEnergy(material,extPar,epsEl,Th1,sig,
                                  N,M,dSig,C,
                                  update || computeTangent,
                                  computeTangent);
    if (update) intV[2] = W;
    
    // thermal capacity
    if (this->capacity) {
      double NT,CT;
      double WT = this->capacity->internalEnergy(material,extPar,Th1,NT,
                                                 CT,update,computeTangent);
      W += WT;
      if (update) {
        intV[3] = WT;
        N += NT;
      }
      if (computeTangent) C += CT;
    }

    // plastic free energy increment + dissipated energy
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

    // tangents
    if (computeTangent) {
      
      // capacity
      C += Cp;

      // (visco)plastic correction
      dEPl = ePl-ePl0;
      if (dEPl > 0.0e0) {
        
        // get current shear modulus
        double T = TRef+Th1;
        double mu;
        try {
          double E,nu;
          try { // get Young's modulus
            Function& fctE = material.getFunctionProperty("YOUNG_MODULUS_EVOLUTION");
            E = fctE.value(T);
          }
          catch (NoSuchPropertyException) {
            E = material.getDoubleProperty("YOUNG_MODULUS");
          }
          if (E < 0.e0) throw InvalidPropertyException("Young's modulus");
          
          try { // get Poisson's coefficient
            Function& fctN = material.getFunctionProperty("POISSON_COEFFICIENT_EVOLUTION");
            nu = fctN.value(T);
          }
          catch (NoSuchPropertyException) {
            nu = material.getDoubleProperty("POISSON_COEFFICIENT");
          }
          if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException("Poisson's coefficient");
          mu = 0.5*E/(1.+nu);
        }
        catch (NoSuchPropertyException) {
          mu = material.getDoubleProperty("SHEAR_MODULUS");
        }
        double mu2=2*mu,mu3=3*mu;
        
        static const SYM_TENSOR4 II = SYM_TENSOR4::contravariantIdentity();
        static const SYM_TENSOR4 KK = SYM_TENSOR4::baseK();
        double coef1 = mu2*dEPl*coef;
        double coef2 = mu2/(mu3+Hp);
        double coef3 = 4*ONE_THIRD*mu*(1.5e0*coef2-coef1);
        M -= ((coef1*mu2)*(II-KK)+coef3*outerProd(Mp,Mp));
        
        double dEPldT = dSigPl-innerProd2(dSig,Mp);
        dSig += coef2*dEPldT*Mp;
        
        C -= dEPldT*dEPldT/(mu3+Hp);
      }
    }
    
    return W+Wp-intV0[2]-intV0[3]+intV0[1]*(Th1-Th0);
  }

 public:

  // radial return algorithm
  static unsigned int radialReturn(const MaterialProperties& material,const ParameterSet& extPar,
                                   ThermoViscoPlasticitySimple& viscoPlasticity,
                                   const MatLibArray& intPar0,MatLibArray& intPar,
                                   const SYM_TENSOR& sigDev,double ePl0,double& ePl,
                                   const SYM_TENSOR& Mp,double Th0,double Th1,
                                   double T0,double T1,double dTime)
   throw (UpdateFailedException) {

    static const unsigned int ITMAX = 25;
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
    double sigPl,Np,dNp,Hp,dSigPl,Cp;
    viscoPlasticity.irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl,
                                       Th0,Th1,T0,T1,sigPl,Np,dNp,Hp,dSigPl,Cp,
                                       dTime,true,true);
    double fct0 = sigEq-sigPl;
    if (fct0 <= 0.0e0) return 0;
    /*std::cout << 0 << "," << ePl0 << "," << fct0 << "," << Hp << std::endl;*/
    
    // get current shear modulus
    double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
    double T = TRef+Th1;
    double mu;
    try {
      double E,nu;
      try { // get Young's modulus
        Function& fctE = material.getFunctionProperty("YOUNG_MODULUS_EVOLUTION");
        E = fctE.value(T);
      }
      catch (NoSuchPropertyException) {
        E = material.getDoubleProperty("YOUNG_MODULUS");
      }
      if (E < 0.e0) throw InvalidPropertyException("Young's modulus");
      
      try { // get Poisson's coefficient
        Function& fctN = material.getFunctionProperty("POISSON_COEFFICIENT_EVOLUTION");
        nu = fctN.value(T);
      }
      catch (NoSuchPropertyException) {
        nu = material.getDoubleProperty("POISSON_COEFFICIENT");
      }
      if (nu < -1.0e0 || nu > 0.5e0) throw InvalidPropertyException("Poisson's coefficient");
      mu = 0.5*E/(1.+nu);
    }
    catch (NoSuchPropertyException) {
      mu = material.getDoubleProperty("SHEAR_MODULUS");
    }
    double mu3=3*mu;

    // apply plastic corrector
    /*for (int k=0; k < 10; k++) {
      double Htmp;
      viscoPlasticity.irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl0+(k+1)*3.e-6,
                                         Th0,Th1,T0,T1,sigPl,Np,dNp,Htmp,dSigPl,Cp,
                                         dTime,true,true);
      std::cout << 0 << "," << ePl0+(k+1)*3.e-6 << "," << sigEq-(k+1)*3.e-6*mu3-sigPl << "," << Htmp << std::endl;
    }*/
    double dEPl = 0.0e0;
    double ePl00 = ePl0;
    double fct = fct0;
    double fct00 = fct0;
    double test = TOLE*(fct+TOLE);
    unsigned int iter=0;
    for (; iter < maxIt; iter++) {
      //std::cout << iter << "- f=" << fct << ", ePl=" << ePl << " (Hp=" << Hp << ")" << std::endl;
      double coef = 1.0e0/(mu3+Hp);
      if (std::fabs(Hp) < THRSHLD && !std::isnan(coef))
        dEPl = fct*coef;
      else
        dEPl = fct/mu3;
      if (std::fabs(dEPl) < PREC) break;
      if ((ePl+dEPl) < (ePl00+PREC)) {
        /* use secant method */
        //std::cout << "secant-dEPl=" << dEPl << "-ePl00=" << ePl00 << "-f00=" << fct00 << std::endl;
        double mult = fct/(fct00-fct);
        if (mult < -MULT) mult=-MULT;
        dEPl = mult*(ePl-ePl00);
        //std::cout << "dEPl=" << dEPl << "mult=" << mult << std::endl;
      }
      if (std::fabs(dEPl) < PREC) break;
      sigEq -= dEPl*mu3;
      ePl += dEPl;
      viscoPlasticity.irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl,
                                         Th0,Th1,T0,T1,sigPl,Np,dNp,Hp,dSigPl,Cp,
                                         dTime,true,true);
      fct = sigEq-sigPl;
      if (std::fabs(fct) < test) break;
      if (fct > 0.0e0 && dEPl < 0.0e0) {
        fct00 = fct;
        ePl00 = ePl;
      }
    }
    // check convergence
    if (iter == maxIt) {
      //std::cerr << "WARNING: no convergence in radial return (after " << iter << " iterations)" << std::endl;
      throw UpdateFailedException("no convergence in radial return");
    }
    
    return iter;
  }
};


/*
 * Implementations of the model.
 */

/**
 * J2 thermoplasticity with linear isotropic hardening.
 */
class LinearIsotropicJ2ThermoPlasticity3D : public J2ThermoPlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2ThermoPlasticity3D()
  : ThermoElasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra3D>()),
    J2ThermoPlasticitySimple<TensorAlgebra3D>(
                  new StdThermoViscoPlasticitySimple(new ThermalLinearIsotropicHardeningModel(),
                                                     new ThermalPowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicJ2ThermoPlasticity3D(const LinearIsotropicJ2ThermoPlasticity3D& src) 
  : ThermoElasticity<TensorAlgebra3D>(src), ThermoElastoPlasticity<TensorAlgebra3D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2ThermoPlasticity3D() {}
};
class LinearIsotropicJ2ThermoPlasticity2D : public J2ThermoPlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2ThermoPlasticity2D()
  : ThermoElasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra2D>()),
    J2ThermoPlasticitySimple<TensorAlgebra2D>(
                    new StdThermoViscoPlasticitySimple(new ThermalLinearIsotropicHardeningModel(),
                                                       new ThermalPowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicJ2ThermoPlasticity2D(const LinearIsotropicJ2ThermoPlasticity2D& src) 
  : ThermoElasticity<TensorAlgebra2D>(src), ThermoElastoPlasticity<TensorAlgebra2D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2ThermoPlasticity2D() {}
};
class LinearIsotropicJ2ThermoPlasticity1D : public J2ThermoPlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2ThermoPlasticity1D()
  : ThermoElasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra1D>()),
    J2ThermoPlasticitySimple<TensorAlgebra1D>(
                    new StdThermoViscoPlasticitySimple(new ThermalLinearIsotropicHardeningModel(),
                                                       new ThermalPowerLawRateDependencyModel())) {}
  
  // copy constructor
  LinearIsotropicJ2ThermoPlasticity1D(const LinearIsotropicJ2ThermoPlasticity1D& src) 
  : ThermoElasticity<TensorAlgebra1D>(src), ThermoElastoPlasticity<TensorAlgebra1D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class LinearIsotropicJ2ThermoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  LinearIsotropicJ2ThermoPlasticityBuilder();
  
  // the instance
  static LinearIsotropicJ2ThermoPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~LinearIsotropicJ2ThermoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

/**
 * J2 thermoplasticity with nonlinear isotropic hardening.
 */
class NonLinearIsotropicJ2ThermoPlasticity3D : public J2ThermoPlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NonLinearIsotropicJ2ThermoPlasticity3D()
  : ThermoElasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra3D>()),
    J2ThermoPlasticitySimple<TensorAlgebra3D>(
                    new StdThermoViscoPlasticitySimple(new ThermalNonLinearIsotropicHardeningModel(),
                                                       new ThermalPowerLawRateDependencyModel())) {}
  
  // copy constructor
  NonLinearIsotropicJ2ThermoPlasticity3D(const NonLinearIsotropicJ2ThermoPlasticity3D& src) 
  : ThermoElasticity<TensorAlgebra3D>(src), ThermoElastoPlasticity<TensorAlgebra3D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NonLinearIsotropicJ2ThermoPlasticity3D() {}
};
class NonLinearIsotropicJ2ThermoPlasticity2D : public J2ThermoPlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  NonLinearIsotropicJ2ThermoPlasticity2D()
  : ThermoElasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra2D>()),
    J2ThermoPlasticitySimple<TensorAlgebra2D>(
                    new StdThermoViscoPlasticitySimple(new ThermalNonLinearIsotropicHardeningModel(),
                                                       new ThermalPowerLawRateDependencyModel())) {}
  
  // copy constructor
  NonLinearIsotropicJ2ThermoPlasticity2D(const NonLinearIsotropicJ2ThermoPlasticity2D& src) 
  : ThermoElasticity<TensorAlgebra2D>(src), ThermoElastoPlasticity<TensorAlgebra2D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~NonLinearIsotropicJ2ThermoPlasticity2D() {}
};
class NonLinearIsotropicJ2ThermoPlasticity1D : public J2ThermoPlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  NonLinearIsotropicJ2ThermoPlasticity1D()
  : ThermoElasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra1D>()),
    J2ThermoPlasticitySimple<TensorAlgebra1D>(
                    new StdThermoViscoPlasticitySimple(new ThermalNonLinearIsotropicHardeningModel(),
                                                       new ThermalPowerLawRateDependencyModel())) {}
  
  // copy constructor
  NonLinearIsotropicJ2ThermoPlasticity1D(const NonLinearIsotropicJ2ThermoPlasticity1D& src) 
  : ThermoElasticity<TensorAlgebra1D>(src), ThermoElastoPlasticity<TensorAlgebra1D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~NonLinearIsotropicJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class NonLinearIsotropicJ2ThermoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  NonLinearIsotropicJ2ThermoPlasticityBuilder();
  
  // the instance
  static NonLinearIsotropicJ2ThermoPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~NonLinearIsotropicJ2ThermoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

/**
 * J2 thermoplasticity with nonlinear isotropic hardening and asinh rate-dependency.
 */
class NonLinearASinhIsotropicJ2ThermoPlasticity3D : public J2ThermoPlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  NonLinearASinhIsotropicJ2ThermoPlasticity3D()
  : ThermoElasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra3D>()),
    J2ThermoPlasticitySimple<TensorAlgebra3D>(
                    new StdThermoViscoPlasticitySimple(new ThermalNonLinearIsotropicHardeningModel(),
                                                       new ThermalASinhRateDependencyModel())) {}
  
  // copy constructor
  NonLinearASinhIsotropicJ2ThermoPlasticity3D(const NonLinearASinhIsotropicJ2ThermoPlasticity3D& src) 
  : ThermoElasticity<TensorAlgebra3D>(src), ThermoElastoPlasticity<TensorAlgebra3D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~NonLinearASinhIsotropicJ2ThermoPlasticity3D() {}
};
class NonLinearASinhIsotropicJ2ThermoPlasticity2D : public J2ThermoPlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  NonLinearASinhIsotropicJ2ThermoPlasticity2D()
  : ThermoElasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra2D>()),
    J2ThermoPlasticitySimple<TensorAlgebra2D>(
                    new StdThermoViscoPlasticitySimple(new ThermalNonLinearIsotropicHardeningModel(),
                                                       new ThermalASinhRateDependencyModel())) {}
  
  // copy constructor
  NonLinearASinhIsotropicJ2ThermoPlasticity2D(const NonLinearASinhIsotropicJ2ThermoPlasticity2D& src) 
  : ThermoElasticity<TensorAlgebra2D>(src), ThermoElastoPlasticity<TensorAlgebra2D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~NonLinearASinhIsotropicJ2ThermoPlasticity2D() {}
};
class NonLinearASinhIsotropicJ2ThermoPlasticity1D : public J2ThermoPlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  NonLinearASinhIsotropicJ2ThermoPlasticity1D()
  : ThermoElasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra1D>()),
    J2ThermoPlasticitySimple<TensorAlgebra1D>(
                    new StdThermoViscoPlasticitySimple(new ThermalNonLinearIsotropicHardeningModel(),
                                                       new ThermalASinhRateDependencyModel())) {}
  
  // copy constructor
  NonLinearASinhIsotropicJ2ThermoPlasticity1D(const NonLinearASinhIsotropicJ2ThermoPlasticity1D& src) 
  : ThermoElasticity<TensorAlgebra1D>(src), ThermoElastoPlasticity<TensorAlgebra1D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~NonLinearASinhIsotropicJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class NonLinearASinhIsotropicJ2ThermoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  NonLinearASinhIsotropicJ2ThermoPlasticityBuilder();
  
  // the instance
  static NonLinearASinhIsotropicJ2ThermoPlasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~NonLinearASinhIsotropicJ2ThermoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * J2 thermoplasticity with Norton-Hoff isotropic hardening.
 */
class NortonHoffIsotropicJ2ThermoPlasticity3D : public J2ThermoPlasticitySimple<TensorAlgebra3D> {

 public:

  // constructor
  NortonHoffIsotropicJ2ThermoPlasticity3D()
  : ThermoElasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra3D>()),
    J2ThermoPlasticitySimple<TensorAlgebra3D>(
                    new StdThermoViscoPlasticitySimple(0,new ThermalNortonHoffRateDependencyModel())) {}

  // copy constructor
  NortonHoffIsotropicJ2ThermoPlasticity3D(const NortonHoffIsotropicJ2ThermoPlasticity3D& src)
  : ThermoElasticity<TensorAlgebra3D>(src), ThermoElastoPlasticity<TensorAlgebra3D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra3D>(src) {}

  // destructor
  virtual ~NortonHoffIsotropicJ2ThermoPlasticity3D() {}
};
class NortonHoffIsotropicJ2ThermoPlasticity2D : public J2ThermoPlasticitySimple<TensorAlgebra2D> {

 public:

  // constructor
  NortonHoffIsotropicJ2ThermoPlasticity2D()
  : ThermoElasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra2D>()),
    J2ThermoPlasticitySimple<TensorAlgebra2D>(
                    new StdThermoViscoPlasticitySimple(0,new ThermalNortonHoffRateDependencyModel())) {}

  // copy constructor
  NortonHoffIsotropicJ2ThermoPlasticity2D(const NortonHoffIsotropicJ2ThermoPlasticity2D& src)
  : ThermoElasticity<TensorAlgebra2D>(src), ThermoElastoPlasticity<TensorAlgebra2D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra2D>(src) {}

  // destructor
  virtual ~NortonHoffIsotropicJ2ThermoPlasticity2D() {}
};
class NortonHoffIsotropicJ2ThermoPlasticity1D : public J2ThermoPlasticitySimple<TensorAlgebra1D> {

 public:

  // constructor
  NortonHoffIsotropicJ2ThermoPlasticity1D()
  : ThermoElasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra1D>()),
    J2ThermoPlasticitySimple<TensorAlgebra1D>(
                    new StdThermoViscoPlasticitySimple(0,new ThermalNortonHoffRateDependencyModel())) {}

  // copy constructor
  NortonHoffIsotropicJ2ThermoPlasticity1D(const NortonHoffIsotropicJ2ThermoPlasticity1D& src)
  : ThermoElasticity<TensorAlgebra1D>(src), ThermoElastoPlasticity<TensorAlgebra1D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra1D>(src) {}

  // destructor
  virtual ~NortonHoffIsotropicJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class NortonHoffIsotropicJ2ThermoPlasticityBuilder : public ModelBuilder {

 private:

  // constructor
  NortonHoffIsotropicJ2ThermoPlasticityBuilder();

  // the instance
  static NortonHoffIsotropicJ2ThermoPlasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~NortonHoffIsotropicJ2ThermoPlasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
