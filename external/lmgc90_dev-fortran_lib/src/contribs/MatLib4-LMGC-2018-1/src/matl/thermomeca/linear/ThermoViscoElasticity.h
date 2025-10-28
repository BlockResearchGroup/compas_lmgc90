/*
 *  $Id: ThermoViscoElasticity.h 215 2016-10-06 20:46:04Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_LINEAR_VISCO_ELASTICITY_H
#define ZORGLIB_MATL_MECA_THERMO_LINEAR_VISCO_ELASTICITY_H

// config
#include <matlib_macros.h>

// std C library
#include <cstdio>
#include <cstring>
// local
#include <matl/thermomeca/linear/ThermoElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for (geometrically linear) thermo-visco-elastic material models.
 */
template <class ALG>
class ThermoViscoElasticity : virtual public ThermoElasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
  // nested classes
  class MaxwellViscoElasticity;
  class ViscousPotential;
  
 protected:

  // Maxwell branches
  std::vector<MaxwellViscoElasticity*> maxwell;

  // Kelvin branch
  ViscousPotential *viscous;

  // empty constructor
  ThermoViscoElasticity(ViscousPotential* v = 0) {
    viscous = v;
  }

 public:

  // constructors
  ThermoViscoElasticity(typename ThermoElasticity<ALG>::Potential& p,
			ViscousPotential& v,LinThermalCapacity& c)
   : ThermoElasticity<ALG>(p,c) {viscous = &v;}
  ThermoViscoElasticity(typename ThermoElasticity<ALG>::Potential& p,
			ViscousPotential& v,LinThermalCapacity& c,
			typename ThermoElasticity<ALG>::Dilatancy& d)
   : ThermoElasticity<ALG>(p,c,d) {viscous = &v;}

  // copy constructor
  ThermoViscoElasticity(const ThermoViscoElasticity& src)
  : ThermoElasticity<ALG>(src) {maxwell = src.maxwell; viscous = src.viscous;}
  
  // destructor
  virtual ~ThermoViscoElasticity() {
    if (*(this->count) > 1) return;
    for (unsigned int n=0; n < maxwell.size(); n++) delete maxwell[n];
    if (viscous) delete viscous;
  }

  // add a Maxwell branch
  void addMaxwellBranch(MaxwellViscoElasticity& vb) {
    maxwell.push_back(&vb);
  }

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nThermo-viscoelasticity model (small strains):" << std::endl;

    // density
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      if (os) (*os) << "\n\tmass density = " << rho << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tmass density is not defined" << std::endl;
    }
    
    // look for algorithmic parameter
    double alpha = 0.5;
    try {
      alpha = material.getDoubleProperty("TVE_ALGORITHMIC_PARAMETER");
    }
    catch (NoSuchPropertyException) {
      material.setProperty("TVE_ALGORITHMIC_PARAMETER",alpha);
    }
    if (os) (*os) << "\n\tthermo-visco-elastic algorithmic parameter = " << alpha << std::endl;
    
    // check elastic potential
    if (this->potential) this->potential->checkProperties(material,os);
    
    // check capacity
    if (this->capacity) this->capacity->checkProperties(material,os);

    // check dilatancy model
    if (this->dilatancy) this->dilatancy->checkProperties(material,os);

    // check viscous potential
    if (viscous) viscous->checkProperties(material,os);
    
    // maxwell branches
    for (unsigned int n=0; n < maxwell.size(); n++)
      maxwell[n]->checkProperties(material,os);
  
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
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    if (this->potential) this->potential->rotateProperties(material,R);
    if (this->dilatancy) this->dilatancy->rotateProperties(material,R);
    if (viscous) viscous->rotateProperties(material,R);
    for (unsigned int n=0; n < maxwell.size(); n++)
      maxwell[n]->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& material,const ParameterSet& extPar) {
    if (this->potential) this->potential->updateProperties(material,extPar);
    if (this->capacity) this->capacity->updateProperties(material,extPar);
    if (this->dilatancy) this->dilatancy->updateProperties(material,extPar);
    if (viscous) viscous->updateProperties(material,extPar);
    for (unsigned int n=0; n < maxwell.size(); n++)
      maxwell[n]->updateProperties(material,extPar);
  }
  
  // how many internal variables ?
  unsigned int nIntVar() const {
    unsigned int n = 3;
    for (unsigned int i=0; i < maxwell.size(); i++)
      n += SYM_TENSOR::MEMSIZE+maxwell[i]->nIntPar();
    return n;
  }
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 3 + 2*maxwell.size();}
  unsigned int getIntVar(const std::string& str) const {
    if (maxwell.size() > 0 && str == "VSTN1")
      return 0;
    else if (maxwell.size() > 1 && str == "VSTN2")
      return 1;
    else if (maxwell.size() > 2 && str == "VSTN3")
      return 2;
    else if (maxwell.size() > 3 && str == "VSTN4")
      return 3;
    else if (maxwell.size() > 4 && str == "VSTN5")
      return 4;
    else if (maxwell.size() > 5 && str == "VSTN6")
      return 5;
    else if (maxwell.size() > 6 && str == "VSTN7")
      return 6;
    else if (maxwell.size() > 7 && str == "VSTN8")
      return 7;
    else if (maxwell.size() > 8 && str == "VSTN9")
      return 8;
    else if (str == "ENTP")
      return maxwell.size();
    else if (str == "ENRG")
      return maxwell.size()+1;
    else if (str == "TNRG")
      return maxwell.size()+2;
    else if (maxwell.size() > 0 && str == "VNRG1")
      return maxwell.size()+3;
    else if (maxwell.size() > 1 && str == "VNRG2")
      return maxwell.size()+4;
    else if (maxwell.size() > 2 && str == "VNRG3")
      return maxwell.size()+5;
    else if (maxwell.size() > 3 && str == "VNRG4")
      return maxwell.size()+6;
    else if (maxwell.size() > 4 && str == "VNRG5")
      return maxwell.size()+7;
    else if (maxwell.size() > 5 && str == "VNRG6")
      return maxwell.size()+8;
    else if (maxwell.size() > 6 && str == "VNRG7")
      return maxwell.size()+9;
    else if (maxwell.size() > 7 && str == "VNRG8")
      return maxwell.size()+10;
    else if (maxwell.size() > 8 && str == "VNRG9")
      return maxwell.size()+11;
    else
      return 2*maxwell.size()+3;
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    if (i < maxwell.size())
      return ConstitutiveModel::TYPE_SYM_TENSOR;
    else if (i < 2*maxwell.size()+3)
      return ConstitutiveModel::TYPE_SCALAR;
    else
      return ConstitutiveModel::TYPE_NONE;
  }
  unsigned int indexIntVar(unsigned int i) const {
    if (i < maxwell.size())
      return i*SYM_TENSOR::MEMSIZE;
    else if (i < 2*maxwell.size()+3)
      return maxwell.size()*SYM_TENSOR::MEMSIZE+i-maxwell.size();
    else
      return maxwell.size()*(SYM_TENSOR::MEMSIZE+1)+3;
  }
  std::string labelIntVar(unsigned int i) const {
    char str[64];
    if (i < maxwell.size()) {
      std::sprintf(str,"viscous strain %u",i+1);
      return str;
    }
    else if (i == maxwell.size())
      return "entropy";
    else if (i == maxwell.size()+1)
      return "elastically stored energy";
    else if (i == maxwell.size()+2)
      return "thermally stored energy";
    else if (i < 2*maxwell.size()+3) {
      std::sprintf(str,"viscous stored energy %u",
                   i+2-static_cast<unsigned int>(maxwell.size()));
      return str;
    }
    else
      return "";
  }
  
  // check if the material behaviour is linear ?
  bool isLinear() const {return false;}
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& M,
                              bool update,bool tangent) 
   throw (UpdateFailedException) {

    // update ?
    if (update) state.internal = state0.internal;
	
    // get tensors
    SYM_TENSOR eps0(state0.grad);
    SYM_TENSOR eps1(state.grad);
    SYM_TENSOR sig(state.flux);
    SYM_TENSOR4 K(M);
    
    // get temperature
    double Th0 = state0.grad[SYM_TENSOR::MEMSIZE];
    double Th1 = state.grad[SYM_TENSOR::MEMSIZE];
    double dN,C;
    SYM_TENSOR dSig;
     
    // compute incremental potential
    double W = viscoelasticUpdate(material,extPar,eps1,Th0,Th1,sig,dN,
                                  state0.internal,state.internal,dTime,
                                  K,dSig,C,update,update,tangent);

    // viscous part
    if (viscous && dTime > 0.0e0) {
      
      // get algorithmic parameter
      double alpha = material.getDoubleProperty("TVE_ALGORITHMIC_PARAMETER");
      SYM_TENSOR eps = (1.0-alpha)*eps0+alpha*eps1;
      double Th = (1.0-alpha)*Th0+alpha*Th1;
      double dT = Th1-Th0;
      
      // compute reference temperatures (after linearization)
      double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
      double T0 = TRef;
      double T1 = TRef+dT;

      // get temperature ratio(s)
      double ratio1 = dT/T0;
      double ratio0 = T1/T0;
      double ratio2 = dT/T1;
      double ratio3 = T0/T1;

      // compute "external" viscous energy
      double dTimeInv = 1.0/dTime;
      SYM_TENSOR epsDot = ratio0*dTimeInv*(eps1-eps0);
      SYM_TENSOR Sv1a,Sv2a,Sv1b,Sv2b,dSv1,dSv2;
      SYM_TENSOR4 Mv11a,Mv22a,Mv12a,Mv11b,Mv22b,Mv12b;
      double Dv1,Dv2,Nv3,Cv3;
      Dv1 = dTime*viscous->dissipatedEnergy(material,extPar,eps,epsDot,Th0,
                                            Sv1a,Sv2a,Nv3,Mv11a,Mv22a,Mv12a,
                                            dSv1,dSv2,Cv3,dTime,
                                            update,tangent);
      Dv2 = dTime*viscous->dissipatedEnergy(material,extPar,eps,epsDot,Th,
                                            Sv1b,Sv2b,Nv3,Mv11b,Mv22b,Mv12b,
                                            dSv1,dSv2,Cv3,dTime,
                                            update,tangent);
      W += ratio3*Dv1+ratio2*Dv2;
      double coef = alpha*dTime;
      if (update) {
        SYM_TENSOR val = Sv2a+ratio1*Sv2b;
        sig += coef*(ratio3*Sv1a+ratio2*Sv1b) + val;
        dN += (innerProd(val,eps1-eps0) + coef*dT*Nv3 
	           + dTime*ratio3*(Dv2-Dv1))/T1;
      }
      if (tangent) {
        SYM_TENSOR4 val1 = Mv12a+ratio1*Mv12b;
        SYM_TENSOR4 val2 = Mv22a+ratio1*Mv22b;
        K += alpha*coef*(ratio3*Mv11a+ratio2*Mv11b) 
            + 2*alpha*val1 + dTimeInv*ratio0*val2;
        SYM_TENSOR4 val3 = coef*val1+ratio0*val2;
        dSig += alpha*ratio2*(coef*dSv1+ratio0*dSv2)
	             + (ratio3/T1)*(val3*epsDot)
	             + Sv2b/T0 + coef*ratio3*(Sv1b-Sv1a)/T1;
        SYM_TENSOR val = val2*epsDot + 2*(Sv2b-Sv2a);
        C += coef*ratio2*(2*innerProd(dSv2,epsDot)/T1 + alpha*Cv3)
	          + 2*dTime*(alpha*Nv3 - (Dv2-Dv1)/T1)*ratio3/T1
	          + innerProd(val,eps1-eps0)/(T1*T1);
      }
    }

    // update
    if (update) {
      state.flux[SYM_TENSOR::MEMSIZE] = dN;
    }
    if (tangent) {
      M[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE] = C;
      for (unsigned int i=0; i < SYM_TENSOR::MEMSIZE; i++)
        M[i][SYM_TENSOR::MEMSIZE] = M[SYM_TENSOR::MEMSIZE][i] = dSig[i];
    }

    return W;
  }

 protected:

  // viscoelastic update (Maxwell branches)
  double viscoelasticUpdate(const MaterialProperties& material,
                            const ParameterSet& extPar,
                            const SYM_TENSOR& eps,double Th0,double Th1,
                            SYM_TENSOR& sig,double& dN,
                            const MatLibArray& intVar0,MatLibArray& intVar,
                            double dTime,SYM_TENSOR4& M,SYM_TENSOR& dSig,
                            double& C,bool update,bool stress,bool tangent) 
   throw (UpdateFailedException) {

    // compute stored energy
    double W = this->storedEnergy(material,extPar,eps,Th1,sig,dN,
                                  M,dSig,C,stress,tangent);
    if (update) {
      intVar[maxwell.size()*SYM_TENSOR::MEMSIZE] = -dN;
      intVar[maxwell.size()*SYM_TENSOR::MEMSIZE+1] = W;
    }
    
    // thermal capacity
    if (this->capacity) {
      double NT,CT;
      double WT = this->capacity->internalEnergy(material,extPar,Th1,NT,
                                                 CT,update,tangent);
      W += WT;
      if (update) {
        intVar[maxwell.size()*SYM_TENSOR::MEMSIZE] -= NT;
        intVar[maxwell.size()*SYM_TENSOR::MEMSIZE+2] = WT;
        dN += NT;
      }
      if (tangent) C += CT;
    }

    // update Maxwell branches
    unsigned int n0 = maxwell.size()*SYM_TENSOR::MEMSIZE+3;
    for (unsigned int n=0; n < maxwell.size(); n++) {
      
      // get viscous strains
      const SYM_TENSOR epsV0(intVar0,n*SYM_TENSOR::MEMSIZE);
      SYM_TENSOR epsV1(intVar,n*SYM_TENSOR::MEMSIZE);
      
      // get internal parameters
      const MatLibArray intPar0(intVar0,maxwell[n]->nIntPar(),n0);
      MatLibArray intPar1(intVar,maxwell[n]->nIntPar(),n0);
      n0 += maxwell[n]->nIntPar();
      
      // update Maxwell branch
      double dNv,Wv,WTv,Nv,Cv;
      SYM_TENSOR sigV,dSigV;
      SYM_TENSOR4 Mv;
      W += maxwell[n]->incrementalPotential(material,extPar,eps,Th0,Th1,
                                            sigV,dNv,epsV0,epsV1,Wv,WTv,Nv,
                                            intPar0,intPar1,dTime,Mv,dSigV,Cv,
                                            update,stress,tangent);
      if (update) {
        intVar[maxwell.size()*SYM_TENSOR::MEMSIZE] -= Nv;
        intVar[maxwell.size()*SYM_TENSOR::MEMSIZE+1] += Wv;
        intVar[maxwell.size()*SYM_TENSOR::MEMSIZE+2] += WTv;
      }
      if (stress) {
        sig += sigV;
        dN += dNv;
      }
      if (tangent) {
        M += Mv;
        dSig += dSigV;
        C += Cv;
      }
    }

    if (stress) {
      dN += intVar0[maxwell.size()*SYM_TENSOR::MEMSIZE];
    }

    return W-intVar0[maxwell.size()*SYM_TENSOR::MEMSIZE+1]
            -intVar0[maxwell.size()*SYM_TENSOR::MEMSIZE+2]
            +intVar0[maxwell.size()*SYM_TENSOR::MEMSIZE]*(Th1-Th0);
  }
};


/**
 * Base class for (linear) thermoviscoelastic models (Maxwell branches).
 */
template <class ALG>
class ThermoViscoElasticity<ALG>::MaxwellViscoElasticity {
  
 protected:
  
  // constructor
  MaxwellViscoElasticity() {}
  
 public:
  
  // destructor
  virtual ~MaxwellViscoElasticity() {}

  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // apply rotation to material properties
  virtual void rotateProperties(MaterialProperties&,const Rotation&) {}
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // number of internal parameters
  virtual unsigned int nIntPar() const = 0;
  
  // compute contribution to incremental potential
  virtual double incrementalPotential(const MaterialProperties&,
                                      const ParameterSet&,
                                      const SYM_TENSOR&,double,double,
                                      SYM_TENSOR&,double&,
                                      const SYM_TENSOR&,SYM_TENSOR&,
                                      double&,double&,double&,
                                      const MatLibArray&,MatLibArray&,
                                      double,SYM_TENSOR4&,SYM_TENSOR&,
                                      double&,bool,bool,bool)
    throw (UpdateFailedException) = 0;
};

  
/**
 * Base class for (linear) viscous potentials (Kelvin-Voigt thermoviscoelasticity).
 */
template <class ALG>
class ThermoViscoElasticity<ALG>::ViscousPotential {
  
 protected:
  
  // constructor
  ViscousPotential() {}
  
 public:
  
  // destructor
  virtual ~ViscousPotential() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // apply rotation to material properties
  virtual void rotateProperties(MaterialProperties&,const Rotation&) {}
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // compute stored energy
  virtual double dissipatedEnergy(const MaterialProperties&,const ParameterSet&,
                                  const SYM_TENSOR&,const SYM_TENSOR&,
                                  double,SYM_TENSOR&,SYM_TENSOR&,double&,
                                  SYM_TENSOR4&,SYM_TENSOR4&,SYM_TENSOR4&,
                                  SYM_TENSOR&,SYM_TENSOR&,double&,double,
                                  bool,bool) = 0;
};


/**
 * Class for standard (linear) thermoviscoelastic models (Maxwell branches).
 */
template <class ALG>
class StdMaxwellThermoViscoElasticity 
: virtual public ThermoViscoElasticity<ALG>::MaxwellViscoElasticity {

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:

  // isochoric?
  bool isochoric;

  // elastic part
  typename ThermoElasticity<ALG>::Potential* potential;
  
  // viscous part
  typename ThermoViscoElasticity<ALG>::ViscousPotential* viscous;
  
  // instance counter
  unsigned int *count;
  
 public:
  
  // constructor
  StdMaxwellThermoViscoElasticity(typename ThermoElasticity<ALG>::Potential& p,
                                  typename ThermoViscoElasticity<ALG>::ViscousPotential& v,
                                  bool i = false) {
    count = new unsigned int(1);
    isochoric = i;
    potential = &p;
    viscous = &v;
  }
  
  // copy constructor
  StdMaxwellThermoViscoElasticity(const StdMaxwellThermoViscoElasticity& src) {
    count = src.count;
    (*count)++;
    isochoric = src.isochoric;
    potential = src.potential;
    viscous = src.viscous;
  }
  
  // destructor
  virtual ~StdMaxwellThermoViscoElasticity() {
    if (--(*count) > 0) return;
    delete count;
    if (potential) delete potential;
    if (viscous) delete viscous;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    potential->checkProperties(material,os);
    viscous->checkProperties(material,os);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    potential->rotateProperties(material,R);
    viscous->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& material,const ParameterSet& extPar) {
    potential->updateProperties(material,extPar);
    viscous->updateProperties(material,extPar);
  }
  
  // number of internal parameters
  unsigned int nIntPar() const {return 3;}
  
  // compute contribution to incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const SYM_TENSOR& eps,double Th0,double Th1,
                              SYM_TENSOR& sig,double& dN,
                              const SYM_TENSOR& epsV0,SYM_TENSOR& epsV1,
                              double& We,double& WT,double& N,
                              const MatLibArray& intPar0,MatLibArray& intPar1,
                              double dTime,SYM_TENSOR4& M,SYM_TENSOR& dSig,
                              double& C,bool update,bool first,bool second)
   throw (UpdateFailedException) {

    // compute elastic predictor strain
    if (update) epsV1 = epsV0;
    SYM_TENSOR epsEl = eps-epsV1;

    // if isochoric, work in deviatoric space
    if (isochoric) {
      static SYM_TENSOR delta = SYM_TENSOR::identity();
      double tr = trace(epsEl);
      epsEl -= (tr/3.0)*delta;
    }

    // compute elastic energy
    We = potential->storedThMEnergy(material,extPar,epsEl,Th1,sig,dN,M,dSig,
                                    C,update || first,update || second);
    
    // no thermal capacity contribution in Maxwell branches
    WT = 0.0e0;

    // compute viscous dissipation
    double Wv = 0.0e0;
    if (dTime > 0.0e0) {
      double dTimeInv = 1.0/dTime;
      
      // get algorithmic parameter
      double alpha = material.getDoubleProperty("TVE_ALGORITHMIC_PARAMETER");
      double coef = alpha*dTime;
      SYM_TENSOR epsV = (1.0-alpha)*epsV0 + alpha*epsV1;
      double Th = (1.0-alpha)*Th0+alpha*Th1;
      double dT = Th1-Th0;
      
      // compute reference temperatures (after linearization)
      double TRef = material.getDoubleProperty("REFERENCE_TEMPERATURE");
      double T0 = TRef;
      double T1 = TRef+dT;
      
      // get temperature ratio(s)
      double ratio1 = dT/T0;
      double ratio0 = T1/T0;
      double ratio2 = dT/T1;
      double ratio3 = T0/T1;

      SYM_TENSOR epsVDot = ratio0*dTimeInv*(epsV1-epsV0);
      SYM_TENSOR Sv1a,Sv2a,Sv1b,Sv2b,dSv1,dSv2,sigV,dSigV;
      SYM_TENSOR4 Mv11a,Mv22a,Mv12a,Mv11b,Mv22b,Mv12b,Mv,Mv22;
      double Wv1,Wv2,Nv,Cv2,Cv;
      Wv1 = dTime*viscous->dissipatedEnergy(material,extPar,epsV,epsVDot,Th0,
                                            Sv1a,Sv2a,Nv,Mv11a,Mv22a,Mv12a,
                                            dSv1,dSv2,Cv2,
                                            dTime,first,update || second);
      Wv2 = dTime*viscous->dissipatedEnergy(material,extPar,epsV,epsVDot,Th,
                                            Sv1b,Sv2b,Nv,Mv11b,Mv22b,Mv12b,
                                            dSv1,dSv2,Cv2,
                                            dTime,first,update || second);
      Wv = ratio3*Wv1+ratio2*Wv2;

      if (update || second) {
        SYM_TENSOR val = Sv2a+ratio1*Sv2b;
        sigV = coef*(ratio3*Sv1a+ratio2*Sv1b) + val;
        double dNv = (innerProd(val,epsV1-epsV0) + coef*dT*Nv
               + dTime*ratio3*(Wv2-Wv1))/T1;
        dN += dNv;

        SYM_TENSOR4 val1 = Mv12a+ratio1*Mv12b;
        SYM_TENSOR4 val2 = Mv22a+ratio1*Mv22b;
        Mv = alpha*coef*(ratio3*Mv11a+ratio2*Mv11b)
            + 2*alpha*val1 + dTimeInv*ratio0*val2;
        Mv22 = dTimeInv*ratio0*val2;
        
        SYM_TENSOR4 val3 = coef*val1+ratio0*val2;
        dSigV = alpha*ratio2*(coef*dSv1+ratio0*dSv2)
               + (ratio3/T1)*(val3*epsVDot)
               + Sv2b/T0 + coef*ratio3*(Sv1b-Sv1a)/T1;
        
        SYM_TENSOR val4 = val2*epsVDot + 2*(Sv2b-Sv2a);
        Cv = coef*ratio2*(2*innerProd(dSv2,epsVDot)/T1 + alpha*Cv2)
            + 2*dTime*(alpha*Nv - (Wv2-Wv1)/T1)*ratio3/T1
            + innerProd(val4,epsV1-epsV0)/(T1*T1);
      }

      if (update) {
        // update viscous strain
        SYM_TENSOR dEpsV;
        M += dTimeInv*Mv22;
        M.solve(dEpsV,sig,true);
        epsV1 = epsV0+dEpsV;
        epsEl -= dEpsV;
        
        // update elastic energy
        We = potential->storedThMEnergy(material,extPar,epsEl,Th1,sig,dN,M,dSig,C,
                                        true,second);
        intPar1[0] = -dN;
        intPar1[1] = We;
        intPar1[2] = 0.0e0; // no thermal capacity in Maxwell branch
        
        // update viscous dissipation
        epsV = (1.0-alpha)*epsV0 + alpha*epsV1;
        epsVDot = ratio0*dTimeInv*dEpsV;
        Wv1 = dTime*viscous->dissipatedEnergy(material,extPar,epsV,epsVDot,Th0,
                                              Sv1a,Sv2a,Nv,Mv11a,Mv22a,Mv12a,
                                              dSv1,dSv2,Cv2,
                                              dTime,first,second);
        Wv2 = dTime*viscous->dissipatedEnergy(material,extPar,epsV,epsVDot,Th,
                                              Sv1b,Sv2b,Nv,Mv11b,Mv22b,Mv12b,
                                              dSv1,dSv2,Cv2,
                                              dTime,first,second);
        Wv = ratio3*Wv1+ratio2*Wv2;
      }
      
      // compute entropy increment
      if (first) {
        SYM_TENSOR val = Sv2a+ratio1*Sv2b;
        double dNv = (innerProd(val,epsV1-epsV0) + coef*dT*Nv
                      + dTime*ratio3*(Wv2-Wv1))/T1;
        dN += intPar0[0]+dNv;
      }
      
      // compute consistent tangent operator
      if (second) {
        SYM_TENSOR4 Mbar;
        SYM_TENSOR4 val1 = Mv12a+ratio1*Mv12b;
        SYM_TENSOR4 val2 = Mv22a+ratio1*Mv22b;
        Mv = alpha*coef*(ratio3*Mv11a+ratio2*Mv11b)
            + 2*alpha*val1 + dTimeInv*ratio0*val2;
        Mbar = M+Mv;
        Mbar.invert();
        M -= M*Mbar*M;
        
        SYM_TENSOR4 val3 = coef*val1+ratio0*val2;
        dSigV = alpha*ratio2*(coef*dSv1+ratio0*dSv2)
               + (ratio3/T1)*(val3*epsVDot)
               + Sv2b/T0 + coef*ratio3*(Sv1b-Sv1a)/T1;
        dSig += dSigV;

        SYM_TENSOR val4 = val2*epsVDot + 2*(Sv2b-Sv2a);
        Cv += coef*ratio2*(2*innerProd(dSv2,epsVDot)/T1 + alpha*Cv2)
             + 2*dTime*(alpha*Nv - (Wv2-Wv1)/T1)*ratio3/T1
             + innerProd(val4,epsV1-epsV0)/(T1*T1);
        C += Cv;
      }
    }
    
    return We-intPar0[1]+Wv;
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
