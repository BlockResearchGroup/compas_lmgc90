/*
 *  $Id: J2ChemoPlasticityCoupled.h 181 2015-10-06 19:19:57Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFFMECA_LINEAR_J2_CHEMO_PLASTICITY_COUPLED_H
#define ZORGLIB_MATL_DIFFMECA_LINEAR_J2_CHEMO_PLASTICITY_COUPLED_H

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
 * Coupled chemo-plastic dissipation potential.
 */
class ChemoPlasticityCoupled {
  
 public:
  
  // flag indicating if the model needs initialization
  bool initialize;
  
  // flag indicating if the model needs finalization (update internal parameters)
  bool finalize;
  
  // destructor
  virtual ~ChemoPlasticityCoupled() {}

  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // number of internal parameters
  virtual unsigned int nIntPar() const {return 0;}
  
  // compute irreversible energy and derivatives
  virtual double irreversibleEnergy(const MaterialProperties&,const ParameterSet&,
                                    const MatLibArray&,MatLibArray&,double,
                                    double&,double&,double,bool,bool) = 0;

};

/**
 * Coupled J2 chemo-plasticity.
 */
template <class ALG>
class J2ChemoPlasticityCoupled : virtual public ChemoElastoPlasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:

  // associated chemo-plasticity model
  ChemoPlasticityCoupled *chemoPlasticity;

  // empty constructor
  J2ChemoPlasticityCoupled(ChemoPlasticityCoupled* cp = 0) {
    chemoPlasticity = cp;
  }

 public:

  // constructor
  J2ChemoPlasticityCoupled(ChemoPlasticityCoupled& cp)
  : ChemoElasticity<ALG>(new IsotropicChemoElasticPotential<ALG>()) {chemoPlasticity = &cp;}
  
  // copy constructor
  J2ChemoPlasticityCoupled(const J2ChemoPlasticityCoupled& src)
  : ChemoElasticity<ALG>(src), ChemoElastoPlasticity<ALG>(src) {chemoPlasticity = src.chemoPlasticity;}

  // destructor
  virtual ~J2ChemoPlasticityCoupled() {
    if (*(this->count) > 1) return;
    if (chemoPlasticity) delete chemoPlasticity;
  }

  // check consistency of properties
  void checkProperties(MaterialProperties& material,std::ostream *os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\ncoupled J2 chemo-plasticity model (small strains):" << std::endl;
    
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
    if (os) (*os) << "\n\t***Coupled chemo-plasticity model***" << std::endl;
    double omega,q;
    try {
      omega = material.getDoubleProperty("CHEMO_PLASTIC_DILATATION_COEFFICIENT");
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: chemo-plastic dilatation coefficient cannot be set." << std::endl;
      throw e;
    }

    try {
      q = material.getDoubleProperty("CHEMO_PLASTIC_COUPLING_COEFFICIENT");
      if ( q < 0.0e0) {
        if (os) (*os) << "ERROR: chemo-plasticity coupling coefficient must be positive." << std::endl;
        throw InvalidPropertyException("chemo-plasticity coefficient");
      }
    }
    catch (NoSuchPropertyException e) {
      if (os) (*os) << "ERROR: chemo-plastic coupling coefficient cannot be set." << std::endl;
      throw e;
    }
    if (os) {
      (*os) << "\tchemo-plastic dilatation coefficient  = " << omega;
      (*os) << "\n\tchemo-plasticity coupling coefficient = " << q << std::endl;
    }
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

    // get chemo-plastic parameters
    double omega = material.getDoubleProperty("CHEMO_PLASTIC_DILATATION_COEFFICIENT");
    double q = material.getDoubleProperty("CHEMO_PLASTIC_COUPLING_COEFFICIENT");

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
      // perform update (radial return in proper metric)
      radialReturn(material,extPar,intPar0,intPar,
                   trEpsEl,sigEq,ePl0,ePl,c0,c,mu1,dTime);
      
      // update internal variables
      intV[0] = ePl;
      intV[1] = c;
      
      // update plastic strain
      dEPl = ePl-ePl0;
      dC = -c+c0;
      epsPl = epsPl0+dEPl*covariant(Mp)-ONE_THIRD*dC*omega*I;
      
      chemoPlasticity->finalize = true;
    }
    
    // elastic deformation
    epsEl = eps-epsPl;
    
    // elastic free energy
    double W = this->storedEnergy(material,extPar,intVar,epsEl,c,sig,mu,
                                  M,dSig,CC,update || computeTangent,
                                  computeTangent);

    // plastic free energy increment + dissipated energy
    double alpha,sigPl,Hp;
    if (q > 0.0e0)
     alpha = std::sqrt(dEPl*dEPl+omega*omega*dC*dC/q);
    else
      alpha = dEPl;
    double Wp = chemoPlasticity->irreversibleEnergy(material,extPar,intPar0,intPar,
                                                    alpha,sigPl,Hp,dTime,
                                                    update || computeTangent,computeTangent);

    // tangents
    if (computeTangent) {

      // chemo-plastic correction
      if (alpha > 0.0e0) {
        
        // get current shear modulus
        double G = material.getDoubleProperty("SHEAR_MODULUS");
        double G2=G+G;
        double G3=G2+G;
        
        // get volumic moduli
        double p,K,dP;
        trEpsEl = trace(epsEl);
        storedEnergyVol(material,extPar,trEpsEl,c,p,mu,K,dP,CC,false,true);
        
        // compute some intermediate values
        double pCoef = dEPl/alpha;
        double vCoef = 0.0e0;
        if (q > 0.0e0)
          vCoef = -dC*omega/(q*alpha);

        SYM_TENSOR dEPldEps,dEPVdEps;
        SYM_TENSOR dSigdP = G2*Mp;
        SYM_TENSOR dSigdV = (K-dP/omega)*I;
        double Hpp = G3+Hp*pCoef*pCoef;
        double Hpc = 0.0e0;
        double Hcc = K-(2*dP-CC/omega)/omega;
        if (q > 0.0e0) {
          Hpp += sigPl*vCoef*vCoef*q/alpha;
          Hpc += (Hp-sigPl/alpha)*pCoef*vCoef;
          Hcc += Hp*vCoef*vCoef+sigPl*pCoef*pCoef/(q*alpha);
          double detHInv = 1.0e0/(Hpp*Hcc-Hpc*Hpc);
          dEPldEps = detHInv*( Hcc*dSigdP-Hpc*dSigdV);
          dEPVdEps = detHInv*(-Hpc*dSigdP+Hpp*dSigdV);
          
          C = -detHInv*Hpp/(omega*omega);
        }
        else {
          dEPldEps = (1.0/Hpp)*dSigdP;
          
          C = 0.0e0;
        }

        // apply correction
        static const SYM_TENSOR4 II = SYM_TENSOR4::contravariantIdentity();
        static const SYM_TENSOR4 KK = SYM_TENSOR4::baseK();
        double coef1 = G2*dEPl*coef;
        M -= (coef1*G2)*(II-KK-TWO_THIRD*outerProd(Mp,Mp));
        M -= outerProd(dSigdP,dEPldEps);
        if (q > 0.0e0) {
          M -= outerProd(dSigdV,dEPVdEps);
        
          dSig = -(1.0/omega)*dEPVdEps;
        }
        else
          dSig = 0.0e0;
      }
    }
    
    return W+Wp-intV0[2]-intV0[3]+mu1*dC;
  }

 public:

  // radial return algorithm
  unsigned int radialReturn(const MaterialProperties& material,const ParameterSet& extPar,
                            const MatLibArray& intPar0,MatLibArray& intPar,
                            double& trEpsEl,double& sigEq,double ePl0,double& ePl,
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

    // get chemo-plastic parameters
    double omega = material.getDoubleProperty("CHEMO_PLASTIC_DILATATION_COEFFICIENT");
    double q = material.getDoubleProperty("CHEMO_PLASTIC_COUPLING_COEFFICIENT");

    // compute test function
    ePl = ePl0;
    double p,muC,K,dP,C;
    c = c0;
    storedEnergyVol(material,extPar,trEpsEl,c,p,muC,K,dP,C,true,true);
    double z = (mu-muC)/omega+p;
    double alpha,sigPl,Hp;
    alpha = 0.0e0;
    chemoPlasticity->irreversibleEnergy(material,extPar,intPar0,intPar,
                                        alpha,sigPl,Hp,dTime,true,true);
    double sigEqZ = std::sqrt(sigEq*sigEq+q*z*z);
    double fct0 = sigEqZ-sigPl;
    if (fct0 <= 0.0e0) return 0;
    
    // get shear modulus
    double G = material.getDoubleProperty("SHEAR_MODULUS");
    double G3=3*G;

    // compute steepest descent direction
    double pCoef = sigEq/sigEqZ;
    double vCoef = q*z/sigEqZ;

    // compute first correction
    double Ceff = K-(2*dP-C/omega)/omega;
    double val = pCoef*pCoef*G3+vCoef*vCoef*Ceff;
    alpha = fct0/val;
    double dEPl = pCoef*alpha;
    ePl += dEPl;
    double dEPV = vCoef*alpha;
    c += dEPV/omega;

    // continue with Newton method
    double ePl00 = ePl0;
    double Rp00 = -sigEq+sigPl*pCoef;
    double c00 = c0;
    double Rc00 = -z+sigPl*vCoef;
    double test = TOLE*(fct0+TOLE);
    unsigned int iter=0;
    for (; iter < maxIt; iter++) {
      
      // compute residuals
      sigEq -= dEPl*G3;
      trEpsEl -= dEPV;
      storedEnergyVol(material,extPar,trEpsEl,c,p,muC,K,dP,C,true,true);
      z = (mu-muC)/omega+p;
      chemoPlasticity->irreversibleEnergy(material,extPar,intPar0,intPar,
                                          alpha,sigPl,Hp,dTime,true,true);
      double Rp,Rc=0.0e0;
      pCoef = (ePl-ePl0)/alpha;
      Rp = -sigEq+sigPl*pCoef;
      if (q > PREC) {
        vCoef = omega*(c-c0)/(q*alpha);
        Rc = -z+sigPl*vCoef;
      }
      
      // check convergence
      double norm = std::sqrt(Rp*Rp+q*Rc*Rc);
      if (norm <= test) break;
      
      if (Rp < 0.0e0) {
        ePl00 = ePl;
        Rp00 = Rp;
      }
      if (Rc < 0.0e0) {
        c00 = c;
        Rc00 = Rc;
      }

      // compute correction
      double Hpp = G3+Hp*pCoef*pCoef;
      double Hpc = 0.0e0;
      double Hcc = K-(2*dP-C/omega)/omega;
      if (q > PREC) {
        Hpp += sigPl*vCoef*vCoef*q/alpha;
        Hpc += (Hp-sigPl/alpha)*pCoef*vCoef;
        Hcc += Hp*vCoef*vCoef+sigPl*pCoef*pCoef/(q*alpha);

        double detHInv = 1.0e0/(Hpp*Hcc-Hpc*Hpc);
        dEPl = -detHInv*( Rp*Hcc-Rc*Hpc);
        dEPV = -detHInv*(-Rp*Hpc+Rc*Hpp);
        if (ePl+dEPl <= ePl0) {
          double pMult = Rp/(Rp-Rp00);
          if (pMult > 0.0e0)
            dEPl = -pMult*(ePl-ePl00);
          else
            dEPl = -MULT*(ePl-ePl0);
        }
        if (c+dEPV/omega <= c0) {
          double cMult = Rc/(Rc-Rc00);
          if (cMult > 0.0e0)
            dEPV = -cMult*(c-c00)*omega;
          else
            dEPV = -MULT*(c-c00)*omega;
        }

        // update internal variables
        ePl += dEPl;
        c += dEPV/omega;
        double eVDot = (c-c0)*omega;
        alpha = std::sqrt((ePl-ePl0)*(ePl-ePl0)+eVDot*eVDot/q);
      }
      else {
        dEPl = -Rp/Hpp;
        if (ePl+dEPl <= ePl0) {
          dEPl = -MULT*(ePl-ePl0);
        }
        dEPV = 0.0e0;
        
        // update internal variables
        ePl += dEPl;
        alpha += dEPl;
      }
      if (dEPl < PREC && std::fabs(dEPV) < PREC) break;
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
 * Rate-independent coupled chemo-plastic dissipation potential, without hardening.
 */
class BasicChemoPlasticityCoupled : virtual public ChemoPlasticityCoupled {
          
 public:

  // empty constructor
  BasicChemoPlasticityCoupled() {}
  
  // copy constructor
  BasicChemoPlasticityCoupled(const BasicChemoPlasticityCoupled&) {}

  // destructor
  virtual ~BasicChemoPlasticityCoupled() {}
          
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException);
  
  // compute irreversible energy and derivatives
  double irreversibleEnergy(const MaterialProperties&,const ParameterSet&,
                            const MatLibArray&,MatLibArray&,double,
                            double&,double&,double,bool,bool);
};

/**
 * Rate-independent coupled chemo-plastic dissipation potential, without hardening.
 */
class BasicChemoViscoPlasticityCoupled : virtual public ChemoPlasticityCoupled {

 public:

  // empty constructor
  BasicChemoViscoPlasticityCoupled() {}

  // copy constructor
  BasicChemoViscoPlasticityCoupled(const BasicChemoViscoPlasticityCoupled&) {}

  // destructor
  virtual ~BasicChemoViscoPlasticityCoupled() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException);

  // compute irreversible energy and derivatives
  double irreversibleEnergy(const MaterialProperties&,const ParameterSet&,
                            const MatLibArray&,MatLibArray&,double,
                            double&,double&,double,bool,bool);
};

        
/*
 * Implementations of the model.
 */

/**
 * J2 chemoplasticity (no hardening).
 */
class LinearIsotropicJ2ChemoPlasticity3D : public J2ChemoPlasticityCoupled<TensorAlgebra3D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2ChemoPlasticity3D()
  : ChemoElasticity<TensorAlgebra3D>(new IsotropicChemoElasticPotential<TensorAlgebra3D>(),
                                     new StdLinChemicalCapacity(),
                                     new IsotropicChemoElasticDilatancy<TensorAlgebra3D>()),
    J2ChemoPlasticityCoupled<TensorAlgebra3D>(new BasicChemoPlasticityCoupled()) {}
  
  // copy constructor
  LinearIsotropicJ2ChemoPlasticity3D(const LinearIsotropicJ2ChemoPlasticity3D& src)
  : ChemoElasticity<TensorAlgebra3D>(src), ChemoElastoPlasticity<TensorAlgebra3D>(src),
    J2ChemoPlasticityCoupled<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2ChemoPlasticity3D() {}
};
class LinearIsotropicJ2ChemoPlasticity2D : public J2ChemoPlasticityCoupled<TensorAlgebra2D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2ChemoPlasticity2D()
  : ChemoElasticity<TensorAlgebra2D>(new IsotropicChemoElasticPotential<TensorAlgebra2D>(),
                                     new StdLinChemicalCapacity(),
                                     new IsotropicChemoElasticDilatancy<TensorAlgebra2D>()),
    J2ChemoPlasticityCoupled<TensorAlgebra2D>(new BasicChemoPlasticityCoupled()) {}
  
  // copy constructor
  LinearIsotropicJ2ChemoPlasticity2D(const LinearIsotropicJ2ChemoPlasticity2D& src)
  : ChemoElasticity<TensorAlgebra2D>(src), ChemoElastoPlasticity<TensorAlgebra2D>(src),
    J2ChemoPlasticityCoupled<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2ChemoPlasticity2D() {}
};
class LinearIsotropicJ2ChemoPlasticity1D : public J2ChemoPlasticityCoupled<TensorAlgebra1D> {
  
 public:
  
  // constructor
  LinearIsotropicJ2ChemoPlasticity1D()
  : ChemoElasticity<TensorAlgebra1D>(new IsotropicChemoElasticPotential<TensorAlgebra1D>(),
                                      new StdLinChemicalCapacity(),
                                      new IsotropicChemoElasticDilatancy<TensorAlgebra1D>()),
    J2ChemoPlasticityCoupled<TensorAlgebra1D>(new BasicChemoPlasticityCoupled()) {}
  
  // copy constructor
  LinearIsotropicJ2ChemoPlasticity1D(const LinearIsotropicJ2ChemoPlasticity1D& src)
  : ChemoElasticity<TensorAlgebra1D>(src), ChemoElastoPlasticity<TensorAlgebra1D>(src),
    J2ChemoPlasticityCoupled<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~LinearIsotropicJ2ChemoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class LinearIsotropicJ2ChemoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  LinearIsotropicJ2ChemoPlasticityBuilder();
  
  // the instance
  static LinearIsotropicJ2ChemoPlasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~LinearIsotropicJ2ChemoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * J2 chemoviscoplasticity (no hardening).
 */
class LinearIsotropicJ2ChemoViscoPlasticity3D : public J2ChemoPlasticityCoupled<TensorAlgebra3D> {

 public:
          
  // constructor
  LinearIsotropicJ2ChemoViscoPlasticity3D()
  : ChemoElasticity<TensorAlgebra3D>(new IsotropicChemoElasticPotential<TensorAlgebra3D>(),
                                     new StdLinChemicalCapacity(),
                                     new IsotropicChemoElasticDilatancy<TensorAlgebra3D>()),
    J2ChemoPlasticityCoupled<TensorAlgebra3D>(new BasicChemoViscoPlasticityCoupled()) {}

  // copy constructor
  LinearIsotropicJ2ChemoViscoPlasticity3D(const LinearIsotropicJ2ChemoViscoPlasticity3D& src)
  : ChemoElasticity<TensorAlgebra3D>(src), ChemoElastoPlasticity<TensorAlgebra3D>(src),
    J2ChemoPlasticityCoupled<TensorAlgebra3D>(src) {}

  // destructor
  virtual ~LinearIsotropicJ2ChemoViscoPlasticity3D() {}
};
class LinearIsotropicJ2ChemoViscoPlasticity2D : public J2ChemoPlasticityCoupled<TensorAlgebra2D> {

 public:

  // constructor
  LinearIsotropicJ2ChemoViscoPlasticity2D()
  : ChemoElasticity<TensorAlgebra2D>(new IsotropicChemoElasticPotential<TensorAlgebra2D>(),
                                     new StdLinChemicalCapacity(),
                                     new IsotropicChemoElasticDilatancy<TensorAlgebra2D>()),
    J2ChemoPlasticityCoupled<TensorAlgebra2D>(new BasicChemoViscoPlasticityCoupled()) {}

  // copy constructor
  LinearIsotropicJ2ChemoViscoPlasticity2D(const LinearIsotropicJ2ChemoViscoPlasticity2D& src)
  : ChemoElasticity<TensorAlgebra2D>(src), ChemoElastoPlasticity<TensorAlgebra2D>(src),
    J2ChemoPlasticityCoupled<TensorAlgebra2D>(src) {}

  // destructor
  virtual ~LinearIsotropicJ2ChemoViscoPlasticity2D() {}
};
class LinearIsotropicJ2ChemoViscoPlasticity1D : public J2ChemoPlasticityCoupled<TensorAlgebra1D> {

 public:

  // constructor
  LinearIsotropicJ2ChemoViscoPlasticity1D()
  : ChemoElasticity<TensorAlgebra1D>(new IsotropicChemoElasticPotential<TensorAlgebra1D>(),
                                     new StdLinChemicalCapacity(),
                                     new IsotropicChemoElasticDilatancy<TensorAlgebra1D>()),
    J2ChemoPlasticityCoupled<TensorAlgebra1D>(new BasicChemoViscoPlasticityCoupled()) {}

  // copy constructor
  LinearIsotropicJ2ChemoViscoPlasticity1D(const LinearIsotropicJ2ChemoViscoPlasticity1D& src)
  : ChemoElasticity<TensorAlgebra1D>(src), ChemoElastoPlasticity<TensorAlgebra1D>(src),
    J2ChemoPlasticityCoupled<TensorAlgebra1D>(src) {}

  // destructor
  virtual ~LinearIsotropicJ2ChemoViscoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class LinearIsotropicJ2ChemoViscoPlasticityBuilder : public ModelBuilder {

 private:

  // constructor
  LinearIsotropicJ2ChemoViscoPlasticityBuilder();

  // the instance
  static LinearIsotropicJ2ChemoViscoPlasticityBuilder const* BUILDER;

 public:

  // destructor
  virtual ~LinearIsotropicJ2ChemoViscoPlasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
