/*
 *  $Id: J2ChemoPlasticity.h 242 2017-06-13 09:18:04Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFFMECA_LINEAR_J2_CHEMO_PLASTICITY_H
#define ZORGLIB_MATL_DIFFMECA_LINEAR_J2_CHEMO_PLASTICITY_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
// std C++ library
#include <limits>
// local
#include <matl/diffmeca/linear/IsotropicChemoElasticity.h>
#include <matl/diffmeca/linear/ChemoElastoPlasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Uncoupled (weakly coupled) chemo-plastic dissipation potential.
 */
class ChemoPlasticity {
  
 public:
  
  // flag indicating if the model needs initialization
  bool initialize;
  
  // flag indicating if the model needs finalization (update internal parameters)
  bool finalize;
  
  // destructor
  virtual ~ChemoPlasticity() {}

  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // number of internal parameters
  virtual unsigned int nIntPar() const {return 0;}
  
  // compute irreversible energy and derivatives
  virtual double irreversibleEnergy(const MaterialProperties&,const ParameterSet&,
                                    const MatLibArray&,MatLibArray&,
                                    double,double,double,double,double&,double&,
                                    double&,double&,double&,double,bool,bool) = 0;

};

/**
 * Uncoupled (weakly coupled) J2 chemo-plasticity.
 */
template <class ALG>
class J2ChemoPlasticity : virtual public ChemoElastoPlasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:

  // associated chemo-plasticity model
  ChemoPlasticity *chemoPlasticity;

  // empty constructor
  J2ChemoPlasticity(ChemoPlasticity* cp = 0) {
    chemoPlasticity = cp;
  }

 public:

  // constructor
  J2ChemoPlasticity(ChemoPlasticity& cp)
  : ChemoElasticity<ALG>(new IsotropicChemoElasticPotential<ALG>()) {chemoPlasticity = &cp;}
  
  // copy constructor
  J2ChemoPlasticity(const J2ChemoPlasticity& src)
  : ChemoElasticity<ALG>(src), ChemoElastoPlasticity<ALG>(src) {chemoPlasticity = src.chemoPlasticity;}

  // destructor
  virtual ~J2ChemoPlasticity() {
    if (*(this->count) > 1) return;
    if (chemoPlasticity) delete chemoPlasticity;
  }

  // check consistency of properties
  void checkProperties(MaterialProperties& material,std::ostream *os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nuncoupled J2 chemo-plasticity model (small strains):" << std::endl;
    
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

    // chemo-plastic model
    chemoPlasticity->checkProperties(material,os);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    ChemoElasticity<ALG>::updateProperties(mater,extPar);
    chemoPlasticity->updateProperties(mater,extPar);
  }
  
  // number of internal variables
  unsigned int nIntVar() const {
    return SYM_TENSOR::MEMSIZE+4+chemoPlasticity->nIntPar();
  }
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 5;}
  unsigned int getIntVar(const std::string& str) const {
    if (str == "PSTN")
      return 0;
    else if (str == "EPLS")
      return 1;
    else if (str == "CTRN")
      return 2;
    else if (str == "ENRG")
      return 3;
    else if (str == "CNRG")
      return 4;
    else
      return 5;
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
      default:
        return SYM_TENSOR::MEMSIZE+4;
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
        return "concentration";
        break;
      case 3:
        return "elastically stored energy";
        break;
      case 4:
        return "chemically stored energy";
        break;
      default:
        return "";
        break;
    }
  }

 protected:
        
  // compute stored energy (deviatoric part)
  double storedEnergyDev(const MaterialProperties& material,const ParameterSet& extPar,
                         const SYM_TENSOR& eps,double c,SYM_TENSOR& sig,double& mu,
                         SYM_TENSOR4& M,SYM_TENSOR& dSig,double& C,bool first,bool second) {

    // valid only for isotropic potentials
    IsotropicChemoElasticPotential<ALG> *isoPot = dynamic_cast<IsotropicChemoElasticPotential<ALG>*>(this->potential);
    
    // elastic energy
    double W = 0.0e0;
    if (isoPot)
      W = isoPot->storedChMEnergyDev(material,extPar,eps,c,sig,mu,
                                     M,dSig,C,first,second);
    else {
      if (first) {
        sig = 0.0e0;
        mu = 0.0e0;
      }
      if (second) {
        M = 0.0e0;
        dSig = 0.0e0;
        C = 0.0e0;
      }
    }
    
    return W;
  }
        
  // compute stored energy (volumic part)
  double storedEnergyVol(const MaterialProperties& material,const ParameterSet& extPar,
                         double eps,double c,double& sig,double& mu,
                         double& M,double& dSig,double& C,bool first,bool second) {
    
    // valid only for isotropic potentials
    IsotropicChemoElasticPotential<ALG> *isoPot = dynamic_cast<IsotropicChemoElasticPotential<ALG>*>(this->potential);
    IsotropicChemoElasticDilatancy<ALG> *isoDil = dynamic_cast<IsotropicChemoElasticDilatancy<ALG>*>(this->dilatancy);

    // elastic energy
    double W = 0.0e0;
    if (isoPot)
      W = isoPot->storedChMEnergyVol(material,extPar,eps,c,sig,mu,
                                     M,dSig,C,first,second);
    else {
      if (first) {
        sig = 0.0e0;
        mu = 0.0e0;
      }
      if (second) {
        M = 0.0e0;
        dSig = 0.0e0;
        C = 0.0e0;
      }
    }
          
    // dilatancy term
    if (isoDil) {
      double muD,sigD,MD,dSigD,CD;
      W += isoDil->couplingChMEnergyVol(material,extPar,eps,c,sigD,muD,
                                        MD,dSigD,CD,first,second);
      if (first) {
        sig += sigD;
        mu += muD;
      }
      if (second) {
        M += MD;
        dSig += dSigD;
        C += CD;
      }
    }
    
    // chemical capacity
    if (this->capacity) {
      double muC,CC;
      W += this->capacity->GibbsEnergy(material,extPar,c,muC,
                                       CC,first,second);
      if (first) {
        mu += muC;
      }
      if (second) C += CC;
    }
    
    return W;
  }

  // solve constitutive (chemo-plastic) update
  double chemoPlasticUpdate(const MaterialProperties& material,const ParameterSet& extPar,
                            const SYM_TENSOR& eps,SYM_TENSOR& sig,double mu0,double mu1,
                            double& dC,const SYM_TENSOR& epsPl0,SYM_TENSOR& epsPl,
                            const MatLibArray& intV0,MatLibArray& intV,double dTime,
                            SYM_TENSOR4& M,SYM_TENSOR& dSig,double& C,
                            bool update,bool computeTangent)
   throw (UpdateFailedException) {
    
    static const double ONE_THIRD = 1./3.;
    static const double TWO_THIRD = 2./3.;
    static const SYM_TENSOR I = SYM_TENSOR::identity();
    
    double mu,CC,sigEq,trEpsEl;
    SYM_TENSOR epsEl,epsElDev,sigDev,Mp;

    // extract equivalent plastic strain
    double ePl0 = intV0[0];
    double ePl  = intV[0];
    double dEPl=ePl-ePl0;
    
    // extract concentrations
    double c0 = intV0[1];
    double c  = intV[1];
    dC = -c+c0;

    // extract internal variables (chemo-elastic part)
    MatLibArray intVar(intV,2,2);

    // extract internal parameters (chemo-plastic model)
    unsigned int nIntPar = intV.size()-4;
    const MatLibArray intPar0(intV0,nIntPar,4);
    MatLibArray intPar(intV,nIntPar,4);
    
    // compute elastic predictor
    double norm0=0.0e0,coef=0.0e0;
    if (update || computeTangent) {
      
      // compute stress deviator
      epsEl = eps-epsPl0;
      trEpsEl = trace(epsEl);
      epsElDev = epsEl-ONE_THIRD*trEpsEl*I;
      storedEnergyDev(material,extPar,epsElDev,c,
                      sigDev,mu,M,dSig,CC,true,false);

      // compute radial return direction
      norm0 = innerProd2(sigDev,sigDev);
      if (norm0 >= 1.e-16) coef = std::sqrt(1.5/norm0);
      Mp = coef*sigDev;
      sigEq = innerProd2(sigDev,Mp);
    }
    
    // update
    chemoPlasticity->initialize = true;
    chemoPlasticity->finalize = false;
    if (update) {
      // perform update (radial return)
      radialReturn(material,extPar,intPar0,intPar,
                   trEpsEl,sigEq,ePl0,ePl,c0,c,mu1,dTime);
      
      // update internal variables
      intV[0] = ePl;
      intV[1] = c;
      
      // update plastic strain
      dEPl = ePl-ePl0;
      dC = -c+c0;
      epsPl = epsPl0+dEPl*covariant(Mp);
      
      chemoPlasticity->finalize = true;
    }
    
    // elastic deformation
    epsEl = eps-epsPl;
    
    // elastic free energy
    double W = this->storedEnergy(material,extPar,intVar,epsEl,c,sig,mu,
                                  M,dSig,CC,update || computeTangent,
                                  computeTangent);

    // plastic free energy increment + dissipated energy
    double sigPl,muPl,Hp,dSigPl,Cp;
    double Wp = chemoPlasticity->irreversibleEnergy(material,extPar,intPar0,intPar,
                                                    ePl0,ePl,c0,c,sigPl,muPl,Hp,dSigPl,Cp,dTime,
                                                    update || computeTangent,computeTangent);

    // tangents
    if (computeTangent) {

      // get volumic moduli
      double p,K,dP;
      trEpsEl = trace(epsEl);
      storedEnergyVol(material,extPar,trEpsEl,c,p,mu,K,dP,CC,false,true);

      // chemo-plastic correction
      if (dEPl > 0.0e0) {
        
        // get current shear modulus
        double G = material.getDoubleProperty("SHEAR_MODULUS");
        double G2=G+G;
        double G3=G2+G;
        
        
        // compute some intermediate values
        double dEPldMu,dCdMu;
        SYM_TENSOR dEPldEps,dCdEps;
        SYM_TENSOR dSigdP = G2*Mp;
        SYM_TENSOR dSigdC = dP*I;
        double Hpp = G3+Hp;
        double Hpc = dSigPl;
        double Hcc = CC+Cp;
        double detInv = 1.0/(Hpp*Hcc-Hpc*Hpc);
        dEPldEps = detInv*( Hcc*dSigdP+Hpc*dSigdC);
        dCdEps   = detInv*(-Hpc*dSigdP-Hpp*dSigdC);
        dEPldMu = -detInv*Hpc;
        dCdMu   =  detInv*Hpp;
        
        // apply correction
        static const SYM_TENSOR4 II = SYM_TENSOR4::contravariantIdentity();
        static const SYM_TENSOR4 KK = SYM_TENSOR4::baseK();
        double coef1 = G2*dEPl*coef;
        M -= (coef1*G2)*(II-KK-TWO_THIRD*outerProd(Mp,Mp));
        M -= outerProd(dSigdP,dEPldEps);
        M += outerProd(dSigdC,dCdEps);

        dSig = dCdMu*dSigdC-dEPldMu*dSigdP;
        C = -dCdMu;
      }
      else {
        // chemo-elasticity
        static const SYM_TENSOR4 KK = SYM_TENSOR4::baseK();
        C = -1.0/(CC+Cp);
        dSig = -dP*C*I;
        M += 3*C*dP*dP*KK;
      }
    }
    
    return W+Wp-intV0[2]-intV0[3]+mu1*dC;
  }

 public:

  // radial return algorithm
  unsigned int radialReturn(const MaterialProperties& material,const ParameterSet& extPar,
                            const MatLibArray& intPar0,MatLibArray& intPar,
                            double trEpsEl,double& sigEq,double ePl0,double& ePl,
                            double c0,double& c,double mu,double dTime)
   throw (UpdateFailedException) {

    static const unsigned int ITMAX = 25;
    static const double MULT = 0.9;
    static const double PREC = 1.e-14;
    static const double TOLE = 1.e-8;
    //static const double THRSHLD = 0.1*std::numeric_limits<double>::max();
    
    // get algorithmic parameter
    unsigned int maxIt;
    if (material.checkProperty("RR_MAX_ITER_PARAMETER"))
      maxIt = material.getIntegerProperty("RR_MAX_ITER_PARAMETER");
    else
      maxIt = ITMAX;

    // compute test function
    ePl = ePl0;
    double p,muC,K,dP,C;
    c = c0;
    storedEnergyVol(material,extPar,trEpsEl,c,p,muC,K,dP,C,true,true);
    double sigPl,muPl,Hp,dSigPl,Cp;
    chemoPlasticity->irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl,
                                        c0,c,sigPl,muPl,Hp,dSigPl,Cp,dTime,true,true);
    double fct0 = sigEq-sigPl;
    double rMu = mu-muC-muPl;
    double test2 = TOLE*(std::fabs(rMu)+TOLE);
    unsigned int iter=0;
    if (fct0 <= 0.0e0) { // iterate on c only
      for (; iter < maxIt; iter++) {
        if (std::fabs(rMu) < test2) break;
        c += rMu/(C+Cp);
        storedEnergyVol(material,extPar,trEpsEl,c,p,muC,K,dP,C,true,true);
        chemoPlasticity->irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl,
                                            c0,c,sigPl,muPl,Hp,dSigPl,Cp,dTime,true,true);
        fct0 = sigEq-sigPl;
        rMu = mu-muC-muPl;
        if (fct0 > 0.0e0) break;
      }
      if (fct0 <= 0.0e0) return iter;
    }
    //std::cout << iter << "-EPl=" << ePl << ",c=" << c << ",R1=" << fct0 << ",R2=" << rMu << std::endl;
    
    // get shear modulus
    double G = material.getDoubleProperty("SHEAR_MODULUS");
    double G3=3*G;

    // continue with Newton method
    double dc,dEPl = 0.0e0;
    double ePl00 = ePl0;
    double fct = fct0;
    double fct00 = fct0;
    double test1 = TOLE*(fct0+TOLE);
    for (; iter < maxIt; iter++) {
   
      // apply correction
      double Hpp,Hpc,Hcc;
      Hpp = G3+Hp;
      Hpc = dSigPl;
      Hcc = C+Cp;
      double detInv = 1.0/(Hpp*Hcc-Hpc*Hpc);
      dEPl = detInv*( Hcc*fct-Hpc*rMu);
      dc   = detInv*(-Hpc*fct+Hpp*rMu);
      if (std::fabs(dEPl) < PREC && std::fabs(dc) < PREC) break;
      if ((ePl+dEPl) < (ePl00+PREC)) {
        double mult = fct/(fct00-fct);
        if (mult < -MULT) mult=-MULT;
        dEPl = mult*(ePl-ePl00);
      }
      if (std::fabs(dEPl) < PREC && std::fabs(dc) < PREC) break;
      ePl += dEPl;
      c += dc;
      
      // compute residuals
      sigEq -= dEPl*G3;
      storedEnergyVol(material,extPar,trEpsEl,c,p,muC,K,dP,C,true,true);
      chemoPlasticity->irreversibleEnergy(material,extPar,intPar0,intPar,ePl0,ePl,
                                          c0,c,sigPl,muPl,Hp,dSigPl,Cp,dTime,true,true);
      fct = sigEq-sigPl;
      rMu = mu-muC-muPl;
      //std::cout << iter << "-EPl=" << ePl << ",c=" << c << ",R1=" << fct << "(" << test1 << "),R2=" << rMu << "(" << test2 << ")" << std::endl;
      
      // check convergence
      if (std::fabs(fct) < test1 && std::fabs(rMu) < test2) break;
      if (fct > 0.0e0 && dEPl < 0.0e0) {
        fct00 = fct;
        ePl00 = ePl;
      }
    }
    // check convergence
    if (iter == maxIt) {
      std::cerr << "no convergence in radial return (after " << iter << " iterations)" << std::endl;
      throw UpdateFailedException("no convergence in radial return");
    }
    
    return iter;
  }
};

        
/**
 * Rate-independent uncoupled chemo-plastic dissipation potential, without hardening.
 */
class BasicChemoPlasticity : virtual public ChemoPlasticity {
          
 public:

  // empty constructor
  BasicChemoPlasticity() {}
  
  // copy constructor
  BasicChemoPlasticity(const BasicChemoPlasticity&) {}

  // destructor
  virtual ~BasicChemoPlasticity() {}
          
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException);
  
  // compute irreversible energy and derivatives
  double irreversibleEnergy(const MaterialProperties&,const ParameterSet&,
                            const MatLibArray&,MatLibArray&,double,double,
                            double,double,double&,double&,
                            double&,double&,double&,double,bool,bool);
};

/**
 * Rate-independent uncoupled chemo-plastic dissipation potential, without hardening.
 */
class BasicChemoViscoPlasticity : virtual public ChemoPlasticity {

 public:

  // empty constructor
  BasicChemoViscoPlasticity() {}

  // copy constructor
  BasicChemoViscoPlasticity(const BasicChemoViscoPlasticity&) {}

  // destructor
  virtual ~BasicChemoViscoPlasticity() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException);

  // compute irreversible energy and derivatives
  double irreversibleEnergy(const MaterialProperties&,const ParameterSet&,
                            const MatLibArray&,MatLibArray&,double,double,
                            double,double,double&,double&,
                            double&,double&,double&,double,bool,bool);
};

        
/*
 * Implementations of the model.
 */

/**
 * J2 chemoplasticity (no hardening, weakly coupled).
 */
class LinearIsotropicJ2WkChemoPlasticity3D : public J2ChemoPlasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2WkChemoPlasticity3D()
  : ChemoElasticity<TensorAlgebra3D>(new IsotropicChemoElasticPotential<TensorAlgebra3D>(),
                                     new StdLinChemicalCapacity(),
                                     new IsotropicChemoElasticDilatancy<TensorAlgebra3D>()),
    J2ChemoPlasticity<TensorAlgebra3D>(new BasicChemoPlasticity()) {}
  
  // copy constructor
  LinearIsotropicJ2WkChemoPlasticity3D(const LinearIsotropicJ2WkChemoPlasticity3D& src)
  : ChemoElasticity<TensorAlgebra3D>(src), ChemoElastoPlasticity<TensorAlgebra3D>(src),
    J2ChemoPlasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2WkChemoPlasticity3D() {}
};
class LinearIsotropicJ2WkChemoPlasticity2D : public J2ChemoPlasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2WkChemoPlasticity2D()
  : ChemoElasticity<TensorAlgebra2D>(new IsotropicChemoElasticPotential<TensorAlgebra2D>(),
                                     new StdLinChemicalCapacity(),
                                     new IsotropicChemoElasticDilatancy<TensorAlgebra2D>()),
    J2ChemoPlasticity<TensorAlgebra2D>(new BasicChemoPlasticity()) {}
  
  // copy constructor
  LinearIsotropicJ2WkChemoPlasticity2D(const LinearIsotropicJ2WkChemoPlasticity2D& src)
  : ChemoElasticity<TensorAlgebra2D>(src), ChemoElastoPlasticity<TensorAlgebra2D>(src),
    J2ChemoPlasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2WkChemoPlasticity2D() {}
};
class LinearIsotropicJ2WkChemoPlasticity1D : public J2ChemoPlasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2WkChemoPlasticity1D()
  : ChemoElasticity<TensorAlgebra1D>(new IsotropicChemoElasticPotential<TensorAlgebra1D>(),
                                      new StdLinChemicalCapacity(),
                                      new IsotropicChemoElasticDilatancy<TensorAlgebra1D>()),
    J2ChemoPlasticity<TensorAlgebra1D>(new BasicChemoPlasticity()) {}
  
  // copy constructor
  LinearIsotropicJ2WkChemoPlasticity1D(const LinearIsotropicJ2WkChemoPlasticity1D& src)
  : ChemoElasticity<TensorAlgebra1D>(src), ChemoElastoPlasticity<TensorAlgebra1D>(src),
    J2ChemoPlasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2WkChemoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class LinearIsotropicJ2WkChemoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  LinearIsotropicJ2WkChemoPlasticityBuilder();
  
  // the instance
  static LinearIsotropicJ2WkChemoPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~LinearIsotropicJ2WkChemoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * J2 chemoviscoplasticity (no hardening, weak coupling).
 */
class LinearIsotropicJ2WkChemoViscoPlasticity3D : public J2ChemoPlasticity<TensorAlgebra3D> {

 public:
          
  // constructor
  LinearIsotropicJ2WkChemoViscoPlasticity3D()
  : ChemoElasticity<TensorAlgebra3D>(new IsotropicChemoElasticPotential<TensorAlgebra3D>(),
                                     new StdLinChemicalCapacity(),
                                     new IsotropicChemoElasticDilatancy<TensorAlgebra3D>()),
    J2ChemoPlasticity<TensorAlgebra3D>(new BasicChemoViscoPlasticity()) {}

  // copy constructor
  LinearIsotropicJ2WkChemoViscoPlasticity3D(const LinearIsotropicJ2WkChemoViscoPlasticity3D& src)
  : ChemoElasticity<TensorAlgebra3D>(src), ChemoElastoPlasticity<TensorAlgebra3D>(src),
    J2ChemoPlasticity<TensorAlgebra3D>(src) {}

  // destructor
  virtual ~LinearIsotropicJ2WkChemoViscoPlasticity3D() {}
};
class LinearIsotropicJ2WkChemoViscoPlasticity2D : public J2ChemoPlasticity<TensorAlgebra2D> {

 public:

  // constructor
  LinearIsotropicJ2WkChemoViscoPlasticity2D()
  : ChemoElasticity<TensorAlgebra2D>(new IsotropicChemoElasticPotential<TensorAlgebra2D>(),
                                     new StdLinChemicalCapacity(),
                                     new IsotropicChemoElasticDilatancy<TensorAlgebra2D>()),
    J2ChemoPlasticity<TensorAlgebra2D>(new BasicChemoViscoPlasticity()) {}

  // copy constructor
  LinearIsotropicJ2WkChemoViscoPlasticity2D(const LinearIsotropicJ2WkChemoViscoPlasticity2D& src)
  : ChemoElasticity<TensorAlgebra2D>(src), ChemoElastoPlasticity<TensorAlgebra2D>(src),
    J2ChemoPlasticity<TensorAlgebra2D>(src) {}

  // destructor
  virtual ~LinearIsotropicJ2WkChemoViscoPlasticity2D() {}
};
class LinearIsotropicJ2WkChemoViscoPlasticity1D : public J2ChemoPlasticity<TensorAlgebra1D> {

 public:

  // constructor
  LinearIsotropicJ2WkChemoViscoPlasticity1D()
  : ChemoElasticity<TensorAlgebra1D>(new IsotropicChemoElasticPotential<TensorAlgebra1D>(),
                                     new StdLinChemicalCapacity(),
                                     new IsotropicChemoElasticDilatancy<TensorAlgebra1D>()),
    J2ChemoPlasticity<TensorAlgebra1D>(new BasicChemoViscoPlasticity()) {}

  // copy constructor
  LinearIsotropicJ2WkChemoViscoPlasticity1D(const LinearIsotropicJ2WkChemoViscoPlasticity1D& src)
  : ChemoElasticity<TensorAlgebra1D>(src), ChemoElastoPlasticity<TensorAlgebra1D>(src),
    J2ChemoPlasticity<TensorAlgebra1D>(src) {}

  // destructor
  virtual ~LinearIsotropicJ2WkChemoViscoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class LinearIsotropicJ2WkChemoViscoPlasticityBuilder : public ModelBuilder {

 private:

  // constructor
  LinearIsotropicJ2WkChemoViscoPlasticityBuilder();

  // the instance
  static LinearIsotropicJ2WkChemoViscoPlasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~LinearIsotropicJ2WkChemoViscoPlasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
