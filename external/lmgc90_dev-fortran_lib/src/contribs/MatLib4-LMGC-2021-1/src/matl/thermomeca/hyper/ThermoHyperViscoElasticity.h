/*
 *  $Id$
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2020, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_HYPER_VISCO_ELASTICITY_H
#define ZORGLIB_MATL_MECA_THERMO_HYPER_VISCO_ELASTICITY_H

// config
#include <matlib_macros.h>

// std C library
#include <cstdio>
// local
#include <matl/thermomeca/hyper/ThermoHyperElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for thermo-visco-hyperelastic material models.
 */
template <class ALG>
class ThermoViscoHyperElasticity : virtual public ThermoHyperElasticity<ALG> {

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor4          TENSOR4;

  // nested classes
  class MaxwellViscoElasticity;
  class ViscousPotential;

 protected:

  // Maxwell branches
  std::vector<MaxwellViscoElasticity*> maxwell;

  // Kelvin branch
  ViscousPotential *viscous;

  // empty constructor
  ThermoViscoHyperElasticity(ViscousPotential* v = 0) {
    viscous = v;
  }

 public:

  // constructor
  ThermoViscoHyperElasticity(typename ThermoHyperElasticity<ALG>::Potential& p,
                             ViscousPotential& v,ThermalCapacity& c)
  : ThermoHyperElasticity<ALG>(p,c) {viscous = &v;}
  ThermoViscoHyperElasticity(typename ThermoHyperElasticity<ALG>::Potential& p,
                             ViscousPotential& v,ThermalCapacity& c,
                             typename ThermoHyperElasticity<ALG>::Dilatancy& d)
  : ThermoHyperElasticity<ALG>(p,c,d) {viscous = &v;}

  // copy constructor
  ThermoViscoHyperElasticity(const ThermoViscoHyperElasticity& src)
  : ThermoHyperElasticity<ALG>(src) {maxwell = src.maxwell; viscous = src.viscous;}

  // destructor
  virtual ~ThermoViscoHyperElasticity() {
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
    if (os) (*os) << "\nThermo-visco-hyperelasticity model:" << std::endl;

    // density
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      if (os) (*os) << "\n\tmass density = " << rho << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tmass density is not defined" << std::endl;
    }

    // eos
    if (this->eos) this->eos->checkProperties(material,os);

    // look for algorithmic parameter
    double alpha = 0.5;
    try {
      alpha = material.getDoubleProperty("THVE_ALGORITHMIC_PARAMETER");
    }
    catch (NoSuchPropertyException) {
      material.setProperty("THVE_ALGORITHMIC_PARAMETER",alpha);
    }
    if (os) (*os) << "\n\tthermo-hyper-visco-elastic algorithmic parameter = " << alpha << std::endl;

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
    if (this->eos) this->eos->updateProperties(material,extPar);
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
      n += TENSOR::MEMSIZE+maxwell[i]->nIntPar();
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
      return ConstitutiveModel::TYPE_TENSOR;
    else if (i < 2*maxwell.size()+3)
      return ConstitutiveModel::TYPE_SCALAR;
    else
      return ConstitutiveModel::TYPE_NONE;
  }
  unsigned int indexIntVar(unsigned int i) const {
    if (i < maxwell.size())
      return i*TENSOR::MEMSIZE;
    else if (i < 2*maxwell.size()+3)
      return maxwell.size()*TENSOR::MEMSIZE+i-maxwell.size();
    else
      return maxwell.size()*(TENSOR::MEMSIZE+1)+3;
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

  // initialize the state of the material
  void initState(const MaterialProperties& material,MaterialState& state) {
    ConstitutiveModel::initState(material,state);

    // set the gradient of deformation
    TENSOR F(state.grad);
    F = TENSOR::identity();

    // set the initial temperature
    double T0 = material.getDoubleProperty("INITIAL_TEMPERATURE");
    state.grad[TENSOR::MEMSIZE] = T0;

    // set everything else to zero
    state.flux = 0.e0;
    state.internal = 0.e0;

    // set viscous gradient of deformation
    for (unsigned int n=0; n < maxwell.size(); n++) {
      MatLibArray Fv0(state.internal,TENSOR::MEMSIZE,n*TENSOR::MEMSIZE);
      Fv0 = TENSOR::identity();
    }
  }

  // compute the incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& T,
                              bool update,bool tangent)
   throw (UpdateFailedException) {

    // get tensors
    TENSOR F0(state0.grad);
    TENSOR F1(state.grad);
    TENSOR P(state.flux);
    TENSOR4 K(T);

    // update ?
    if (update) state.internal = state0.internal;

    // right Cauchy-Green tensor
    SYM_TENSOR C;
    ALG::RightCauchyGreen(F1,C);

    // get temperature
    double T0 = state0.grad[TENSOR::MEMSIZE];
    double T1 = state.grad[TENSOR::MEMSIZE];
    double dN,Cm;
    SYM_TENSOR dS;

    // compute stored energy
    SYM_TENSOR S;
    SYM_TENSOR4 M;
    double W = viscoelasticUpdate(material,extPar,C,T0,T1,S,dN,
                                  state0.internal,state.internal,dTime,
                                  M,dS,Cm,update,update || tangent,tangent);

    // viscous part (Kelvin-Voigt part)
    if (viscous && dTime > 0.0e0) {
      double dTimeInv = 1.0e0/dTime;
      
      // initial right Cauchy-Green tensor
      SYM_TENSOR C0;
      ALG::RightCauchyGreen(F0,C0);

      // get algorithmic parameter
      double alpha = material.getDoubleProperty("THVE_ALGORITHMIC_PARAMETER");
      SYM_TENSOR Cpar = (1.0-alpha)*C0+alpha*C;
      double T = (1.0-alpha)*T0+alpha*T1;
      double dT = T1-T0;

      // get temperature ratio(s)
      double ratio1 = dT/T0;
      double ratio0 = T1/T0;
      double ratio2 = dT/T1;
      double ratio3 = T0/T1;

      // compute strain rate
      SYM_TENSOR dC;
      dC = covariantPush(C,F0);
      double coef1 = (0.5*ratio0)/dTime; // accounting for temperature influence
      SYM_TENSOR dLogC[SYM_TENSOR::MEMSIZE];
      SYM_TENSOR d2LogC[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
      SYM_TENSOR Dv = coef1*log(dC,dLogC,d2LogC,update || tangent,tangent);
      
      // compute "external" viscous energy
      SYM_TENSOR sigV1a,sigV2a,sigV1b,sigV2b,dsigV1,dsigV2;
      SYM_TENSOR4 hv11a,hv22a,hv12a,hv11b,hv22b,hv12b;
      double Wv1,Wv2,Nv3,Cmv3;
      Wv1 = dTime*viscous->dissipatedEnergy(material,extPar,Dv,Cpar,T0,
                                            sigV1a,sigV2a,Nv3,hv11a,hv22a,hv12a,
                                            dsigV1,dsigV2,Cmv3,update,tangent);
      Wv2 = dTime*viscous->dissipatedEnergy(material,extPar,Dv,Cpar,T,
                                            sigV1b,sigV2b,Nv3,hv11b,hv22b,hv12b,
                                            dsigV1,dsigV2,Cmv3,update,tangent);
      W += ratio3*Wv1+ratio2*Wv2;

      double coef = alpha*dTime;
      SYM_TENSOR sigV1,sigV2;
      if (update || tangent) {
        sigV1 = coef*(ratio3*sigV1a+ratio2*sigV1b);
        sigV2 = sigV2a+ratio1*sigV2b;
      }
      if (update) {
        // viscous stresses
        SYM_TENSOR Sv2;
        for (unsigned int ij=0; ij < SYM_TENSOR::MEMSIZE; ij++)
          Sv2[ij] = innerProd2(sigV2,dLogC[ij]);
        S += sigV1+contravariantPull(Sv2,F0);
        
        // viscous effective entropy increment
        dN += (dTime*innerProd2(sigV2,Dv) + ratio3*(Wv2-Wv1) + coef*dT*Nv3)/T1;
      }
      if (tangent) {
        SYM_TENSOR4 hv11 = ratio3*hv11a+ratio2*hv11b;
        SYM_TENSOR4 hv22 = hv22a+ratio1*hv22b;
        SYM_TENSOR4 hv12 = hv12a+ratio1*hv12b;

        SYM_TENSOR4 Mv12,Mv22;
        const SYM_TENSOR *p = *d2LogC;
        for (unsigned int ij=0; ij < SYM_TENSOR::MEMSIZE; ij++){
          SYM_TENSOR tmp;
          tmp.wrap(hv12[ij],SYM_TENSOR::MEMSIZE);
          for (unsigned int kl=0; kl < SYM_TENSOR::MEMSIZE; kl++, p++){
            Mv22[ij][kl] = ratio0*innerProd2(dLogC[ij],innerProd2(hv22,dLogC[kl]))
                          +2*innerProd2(sigV2,*p);
            Mv12[ij][kl] = innerProd2(tmp,dLogC[kl]);
          }
        }
        M += alpha*coef*hv11 + dTimeInv*contravariantPull(Mv22,F0);
        for (unsigned int ij=0; ij < SYM_TENSOR::MEMSIZE; ij++){
          SYM_TENSOR tmp1;
          tmp1.wrap(Mv12[ij],SYM_TENSOR::MEMSIZE);
          SYM_TENSOR tmp2 = contravariantPull(tmp1,F0);
          for (unsigned int kl=0; kl < SYM_TENSOR::MEMSIZE; kl++)
            M[ij][kl] += 2*alpha*tmp2[kl];
        }

        SYM_TENSOR val1 = (innerProd2(hv22,Dv)+alpha*dT*dsigV2+sigV2b)/T0;
        SYM_TENSOR dSigV;
        for (unsigned int ij=0; ij < SYM_TENSOR::MEMSIZE; ij++)
          dSigV[ij] = innerProd2(val1,dLogC[ij]);
        SYM_TENSOR val2 = innerProd2(hv12,Dv);
        dS += contravariantPull(dSigV,F0)
             +coef*((innerProd2(hv12,Dv)+ratio3*(sigV1b-sigV1a))/T1
                    +alpha*ratio1*dsigV1);

        SYM_TENSOR val = (1.0-ratio1)*sigV2b-2*sigV2a
                        +2*alpha*dT*ratio0*dsigV2;
        Cm += dTime*(innerProd2(val+ratio0*innerProd2(hv22,Dv),Dv)/(T1*T1)
                     +alpha*(2*ratio3*Nv3+alpha*ratio2*Cmv3))
              -2*ratio3*(Wv2-Wv1)/(T1*T1);
      }
    }

    // compute Piola tensor and Lagrangian tangents
    if (update) {
      ALG::PK2ToPK1(S,F1,P);
      state.flux[TENSOR::MEMSIZE] = dN;
    }
    if (tangent) {
      ALG::MaterialToLagrangian(M,S,F1,K);
      T[TENSOR::MEMSIZE][TENSOR::MEMSIZE] = Cm;
      TENSOR dP;
      ALG::PK2ToPK1(dS,F1,dP);
      for (unsigned int i=0; i < TENSOR::MEMSIZE; i++)
        T[i][TENSOR::MEMSIZE] = T[TENSOR::MEMSIZE][i] = dP[i];
    }

    return W;
  }

 protected:

  // viscoelastic update (Maxwell branches)
  double viscoelasticUpdate(const MaterialProperties& material,
                            const ParameterSet& extPar,
                            const SYM_TENSOR& C,double T0,double T1,
                            SYM_TENSOR& S,double& dN,
                            const MatLibArray& intVar0,MatLibArray& intVar,
                            double dTime,SYM_TENSOR4& M,SYM_TENSOR& dS,
                            double& Cm,bool update,bool stress,bool tangent)
   throw (UpdateFailedException) {

    // compute stored energy
    double W = this->storedEnergy(material,extPar,C,T1,S,dN,M,dS,Cm,stress,tangent);

    if (update) {
      intVar[maxwell.size()*TENSOR::MEMSIZE] = -dN;
      intVar[maxwell.size()*TENSOR::MEMSIZE+1] = W;
    }

    // thermal capacity
    if (this->capacity) {
      double NT,CT;
      double WT = this->capacity->internalEnergy(material,extPar,T1,NT,
                                                 CT,update,tangent);
      W += WT;
      if (update) {
        intVar[maxwell.size()*TENSOR::MEMSIZE] -= NT;
        intVar[maxwell.size()*TENSOR::MEMSIZE+2] = WT;
        dN += NT;
      }
      if (tangent) Cm += CT;
    }

    // update Maxwell branches
    unsigned int n0 = maxwell.size()*TENSOR::MEMSIZE+3;
    for (unsigned int n=0; n < maxwell.size(); n++) {

      // get viscous strains
      const TENSOR Fv0(intVar0,n*TENSOR::MEMSIZE);
      TENSOR Fv1(intVar,n*TENSOR::MEMSIZE);

      // get internal parameters
      const MatLibArray intPar0(intVar0,maxwell[n]->nIntPar(),n0);
      MatLibArray intPar1(intVar,maxwell[n]->nIntPar(),n0);
      n0 += maxwell[n]->nIntPar();

      // update Maxwell branch
      double dNv,Wev,WTv,Nv,Cmv; // what's the difference between dNv and Nv? viscous and elastic, resp.? is Wev the elastic energy at each Maxwell branch?
      SYM_TENSOR Sv,dSv;
      SYM_TENSOR4 Mv;
      W += maxwell[n]->incrementalPotential(material,extPar,C,T0,T1,
                                            Sv,Nv,Fv0,Fv1,Wev,WTv,dNv,
                                            intPar0,intPar1,dTime,Mv,dSv,Cmv,
                                            update,stress,tangent);
      if (update) {
        intVar[maxwell.size()*TENSOR::MEMSIZE] -= Nv;
        intVar[maxwell.size()*TENSOR::MEMSIZE+1] += Wev;
        intVar[maxwell.size()*TENSOR::MEMSIZE+2] += WTv;
      }
      if (stress) {
        S += Sv;
        dN += dNv;
      }
      if (tangent) {
        M += Mv;
        dS += dSv;
        Cm += Cmv;
      }
    }

    if (stress) {
      dN += intVar0[maxwell.size()*TENSOR::MEMSIZE];
    }

    return W-intVar0[maxwell.size()*TENSOR::MEMSIZE+1]
            -intVar0[maxwell.size()*TENSOR::MEMSIZE+2]
            +intVar0[maxwell.size()*TENSOR::MEMSIZE]*(T1-T0);
  }
};


/**
 * Base class for viscohyperelastic models (Maxwell branches).
 */
template <class ALG>
class ThermoViscoHyperElasticity<ALG>::MaxwellViscoElasticity {

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
                                      const TENSOR&,TENSOR&,
                                      double&,double&,double&,
                                      const MatLibArray&,MatLibArray&,
                                      double,SYM_TENSOR4&,SYM_TENSOR&,
                                      double&,bool,bool,bool)
   throw (UpdateFailedException) = 0;
};


/**
 * Base class for viscous potentials (Kelvin-Voigt viscohyperelasticity).
 */
template <class ALG>
class ThermoViscoHyperElasticity<ALG>::ViscousPotential {

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

  // compute dissipated energy
  virtual double dissipatedEnergy(const MaterialProperties&,const ParameterSet&,
                                  const SYM_TENSOR&,const SYM_TENSOR&,double,
                                  SYM_TENSOR&,SYM_TENSOR&,double&,
                                  SYM_TENSOR4&,SYM_TENSOR4&,SYM_TENSOR4&,
                                  SYM_TENSOR&,SYM_TENSOR&,double&,bool,bool) = 0;
};

/**
 * Base class for (isotropic) thermo-visco-hyperelastic potentials,
 * which are function of principal stretches.
 */
template <class ALG>
class SpectralThermoHEViscousPotential
: virtual public ThermoViscoHyperElasticity<ALG>::ViscousPotential {

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor::TYPE     TENSOR;

 protected:

  // constructor
  SpectralThermoHEViscousPotential() {}

 public:

  // destructor
  virtual ~SpectralThermoHEViscousPotential() {}

  // compute dissipated energy
  double dissipatedEnergy(const MaterialProperties& material,
                          const ParameterSet& extPar,
                          const SYM_TENSOR& Dv,const SYM_TENSOR& Cpar,double T,
                          SYM_TENSOR& sigV1,SYM_TENSOR& sigV2,double& dNv,
                          SYM_TENSOR4& hv11,SYM_TENSOR4& hv22,SYM_TENSOR4& hv12,
                          SYM_TENSOR& dsigV1,SYM_TENSOR& dsigV2,double& Cmv,
                          bool first,bool second) {

    // compute principal viscous stretch rates
    double lambda[3],t[3],h[3][3];
    SYM_TENSOR N[3];
    Dv.eigenSplit(lambda,N);

    // items for computing the possible parametric dependence wrt strains
    double lambda0[3],tpar0[3],hpar0[3][3],hcross0[3][3];
    SYM_TENSOR N0[3];
    Cpar.eigenSplit(lambda0,N0);

    // items for computing the possible parametric dependence wrt temperature
    double dtpar1[3],dtpar0[3];

    // compute dissipation potential
    double dN,C;
    double phi = dissipatedEnergy(material,extPar,lambda,lambda0,T,
                                  tpar0,t,dN,hpar0,h,hcross0,
                                  dtpar0,dtpar1,C,first || second,second);

    if (first) {
      sigV2 = 0.0e0;  // first derivative wrt Dv
      sigV1 = 0.0e0;  // first derivative wrt Cpar
	    for (unsigned int k=0; k < 3; k++) {
        sigV2 += t[k]*N[k];
        sigV1 += tpar0[k]*N0[k];
      }
      dNv += dN;
    }
    if (second) {
      dsigV2 = 0.0e0; // cross derivative wrt temperature and Dv
      dsigV1 = 0.0e0; // cross derivative wrt temperature and Cpar
      hv22 = 0.0e0;   // second derivative wrt Dv
      hv11 = 0.0e0;   // second derivative wrt CPar
      hv12 = 0.0e0;   // second derivative wrt Dv and Cpar
      for (unsigned int k=0; k < 3; k++){
        dsigV2 += dtpar1[k]*N[k];
        dsigV1 += dtpar0[k]*N[k];
        for (unsigned int l=0; l < 3; l++) {
          hv22 += h[k][l]*outerProd(N[k],N[l]);
          hv11 += hpar0[k][l]*outerProd(N0[k],N0[l]);
          hv12 += hcross0[k][l]*outerProd(N[k],N0[l]);

          if (k == l) continue;

          double dl = lambda[l]-lambda[k];
          double coef;
          if (std::fabs(dl) > 1.0e-16)
            coef = 0.5*(t[l]-t[k])/dl;
          else
            coef = 0.5*(h[l][l]-h[k][l]);
          hv22.addIJKL(coef,N[k],N[l]);

          double dlpar = lambda0[l]-lambda0[k];
          double coefpar;
          if (std::fabs(dlpar) > 1.0e-16)
            coefpar = 0.5*(tpar0[l]-tpar0[k])/dlpar;
          else
            coefpar = 0.5*(hpar0[l][l]-hpar0[k][l]);
          hv11.addIJKL(coefpar,N[k],N[l]);
        }
      }
      Cmv += C;
    }

    return phi;
  }

  // compute dissipated energy from principal stretches
  virtual double dissipatedEnergy(const MaterialProperties&,const ParameterSet&,
                                  const double[],const double[],double,
                                  double[],double[],double&,
                                  double[][3],double[][3],double[][3],
                                  double[],double[],double&,bool,bool) = 0;
};


/**
 * Class for spectral (isotropic) viscohyperelastic models (Maxwell branches).
 */
template <class ALG>
class SpectralMaxwellThermoViscoHyperElasticity
: virtual public ThermoViscoHyperElasticity<ALG>::MaxwellViscoElasticity {

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor::TYPE     TENSOR;

 protected:

  // isochoric?
  bool isochoric;

  // elastic part
  SpectralThermoHEPotential<ALG>* potential;

  // viscous part
  SpectralThermoHEViscousPotential<ALG>* viscous;

  // instance counter
  unsigned int *count;

 public:

  // constructor
  SpectralMaxwellThermoViscoHyperElasticity(SpectralThermoHEPotential<ALG>& p,
                                            SpectralThermoHEViscousPotential<ALG>& v,
                                            bool i = false) {
    count = new unsigned int(1);
    isochoric = i;
    potential = &p;
    viscous = &v;
  }

  // copy constructor
  SpectralMaxwellThermoViscoHyperElasticity(const SpectralMaxwellThermoViscoHyperElasticity& src) {
    count = src.count;
    (*count)++;
    isochoric = src.isochoric;
    potential = src.potential;
    viscous = src.viscous;
  }

  // destructor
  virtual ~SpectralMaxwellThermoViscoHyperElasticity() {
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

  // update properties in function of external parameters
  void updateProperties(MaterialProperties& material,const ParameterSet& extPar) {
    potential->updateProperties(material,extPar);
    viscous->updateProperties(material,extPar);
  }

  // number of internal parameters
  unsigned int nIntPar() const {return 1;}

  // compute contribution to incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const SYM_TENSOR& C,double Th0,double Th1,
                              SYM_TENSOR& S,double& Ne,
                              const TENSOR& Fv0,TENSOR& Fv1,
                              double& Wev,double& Wtv,double& dN,
                              const MatLibArray& intPar0,MatLibArray& intPar1,
                              double dTime,SYM_TENSOR4& M,SYM_TENSOR& dS,double& Cm,
                              bool update,bool first,bool second)
   throw (UpdateFailedException) {

    static const unsigned int ITMAX = 20;
    static const double PREC = 1.e-12;
    static const double TOLE = 1.e-06;
    static const double ONE_THIRD = 1./3.;

    double dNe,Cme;
    SYM_TENSOR Ce,Cv,Cv1,Cv0,Cvpar;

    // temperature correction
    double alpha = material.getDoubleProperty("THVHE_ALGORITHMIC_PARAMETER");
    double Th = (1.0-alpha)*Th0+alpha*Th1;
    double dTh = Th1-Th0;

	// get temperature ratio(s)
    double ratio1 = dTh/Th0;
    double ratio0 = Th1/Th0;
    double ratio2 = dTh/Th1;
    double ratio3 = Th0/Th1;

    double lam[3];
    SYM_TENSOR Nc[3];
    C.eigenSplit(lam,Nc);

    SYM_TENSOR dFv;
    SYM_TENSOR dFvAlt;

    double lambda[3],t[3],h[3][3],hetot[3][3],hvtot[3][3];
    SYM_TENSOR4 Hetot;
    Hetot = 0.0e0;
    SYM_TENSOR logCpr;
    SYM_TENSOR dLogCpr[SYM_TENSOR::MEMSIZE];
	SYM_TENSOR d2LogCpr[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];

    ShortArray sigV(3);
    SYM_TENSOR N[3];

	if (update) {

      // compute elastic predictor strain
      Ce = covariantPush(C,Fv0);

      logCpr = log(Ce,dLogCpr,d2LogCpr,first || second,second);
//      std::cout<<"Ce = "<<Ce<<std::endl;
	  // which implies the following initialization
	  Fv1 = Fv0;

      // compute principal stretches
      Ce.eigenSplit(lambda,N);
      if (isochoric) {
        double J = lambda[0]*lambda[1]*lambda[2];
        double Jinv = std::pow(J,-ONE_THIRD);
        for (unsigned int k=0; k < 3; k++) lambda[k] *= Jinv;
      }

      // additional variables accounting for temperature dependence of the elastic part
      double dte[3];

	  // compute viscous update corrector
      double lp[3],tpA[3],dtpA[3],hpA[3][3];
	  double tpB[3],dtpB[3],hpB[3][3];
      for (unsigned int k=0; k < 3; k++) lp[k] = 0.0e0;
      unsigned int iter=0;

	  // additional variables accounting for parametric and temperature dependence of the viscous part
	  double lp0[3];
	  SYM_TENSOR N0[3];
	  double tp0A[3],dtp0A[3],hp0A[3][3],hcross0A[3][3];
	  double dtp1A[3],hp1A[3][3],hcross1A[3][3];
	  double tp0B[3],dtp0B[3],hp0B[3][3],hcross0B[3][3];
	  double dtp1B[3],hp1B[3][3],hcross1B[3][3];

      double dTimeInv = 1.0/dTime;

	  for (; iter < ITMAX; iter++) {

        // compute elastic "stresses"
        ShortArray sig(3);
        double De = potential->storedThMEnergy(material,extPar,lambda,t,h,Th1,dte,dNe,Cme,true,true);
//        std::cout<<"We = "<<De<<std::endl;
        for (unsigned int k=0; k < 3; k++) sig[k] = 2*lambda[k]*t[k];
        if (isochoric) {
          double tr=ONE_THIRD*(sig[0]+sig[1]+sig[2]);
          for (unsigned int k=0; k < 3; k++) sig[k] -= tr;
        }

		SYM_TENSOR Sev;
		Sev = 0.0e0;
		for (unsigned int k=0; k < 3; k++) Sev += (sig[k]/lambda[k])*N[k];
//		std::cout<<"SeRecon = "<<Sev<<std::endl;

		// possible parametric dependence on viscous strains
	    double alpha = material.getDoubleProperty("THVHE_ALGORITHMIC_PARAMETER");
        ALG::RightCauchyGreen(Fv1,Cv1);
        ALG::RightCauchyGreen(Fv0,Cv0);
	    Cvpar = (1.0-alpha)*Cv0+alpha*Cv1;
	    Cvpar.eigenSplit(lp0,N0);

		// ATTENTION: the temperature correction is necessary at this point also
		// DvA, DvB and all the equivalent stress and tangent terms are needed for a consistent update

		// compute viscous "stresses"
		double dNv,Cmv;

        for (unsigned int k=0; k < 3; k++) lp[k] *= ratio0;
        double DvA = dTime*viscous->dissipatedEnergy(material,extPar,lp,tpA,hpA,dtpA,lp0,tp0A,dtp0A,hp0A,hcross0A,dtp1A,hp1A,hcross1A,Th0,dNv,Cmv,true,true);
        double DvB = dTime*viscous->dissipatedEnergy(material,extPar,lp,tpB,hpB,dtpB,lp0,tp0B,dtp0B,hp0B,hcross0B,dtp1B,hp1B,hcross1B,Th,dNv,Cmv,true,true);

//		// TEST
//		SYM_TENSOR sigVTotB;
//		sigVTotB = 0.0e0;
//		ShortArray sigVeig(3);
//		for (unsigned int k=0; k < 3; k++){
//		  sigVeig[k] = alpha*dTime*(ratio3*tp0A[k]+ratio2*tp0B[k]) + (tpA[k]+ratio1*tpB[k]);
//		  sigVTotB += sigVeig[k]*N[k];
//        }
////        std::cout<<"sigVTotB = "<<sigVTotB<<std::endl;
//        // END TEST

		// equilibrium condition on the Maxwell branch: first derivative wrt eigenvalues of elastic strains
		for (unsigned int k=0; k < 3; k++) sig[k] -= (tpA[k] + ratio1*tpB[k]) + alpha*dTime*(ratio3*tp0A[k] + ratio2*tp0B[k]); // additional stress term due to parametric dependence on Cpar

        // viscous stresses
        for (unsigned int k=0; k < 3; k++) sigV[k] = (tpA[k] + ratio1*tpB[k]) + alpha*dTime*(ratio3*tp0A[k] + ratio2*tp0B[k]);

        // check for convergence
        double norm = normL2(sig);
//        std::cout<<"Maxwell stress balance: norm = "<<norm<<std::endl;
//        std::cout<<"Wv = (ratio3*WvA + ratio2*WvB) = "<< ratio3*DvA + ratio2*DvB <<std::endl;
        if (norm < TOLE*(norm+TOLE)) break;

        // compute correction
        ShortArray dl(3);

        ShortArray dll(4);
        ShortArray sigll(4);
        for (unsigned int i=0; i<3; i++) sigll[i] = sig[i];
        sigll[3] = lp[0]+lp[1]+lp[2];

        ShortSqrMatrix H(3);
        ShortSqrMatrix Hll(4);
        double coef1 = 4*dTime;
        for (unsigned int k=0; k < 3; k++){
          for (unsigned int l=0; l < 3; l++) {
//            H[k][l] = coef1*lambda[k]*lambda[l]*h[k][l]                             // elastic part
//			         + dTimeInv*ratio0*(hpA[k][l] + ratio1*hpB[k][l])               // second derivative of viscous part wrt lp
//				     + alpha*alpha*dTime*(ratio3*hp0A[k][l] + ratio2*hp0B[k][l])    // second derivative of viscous part wrt lp0 (parametric)
//					 + 2*alpha*(hcross0A[k][l] + ratio1*hcross0B[k][l]);            // cross derivative of viscous part wrt lp and lp0 (parametric)
//            hvtot[k][l] = dTimeInv*(hpA[k][l] + ratio1*hpB[k][l])
//				     + alpha*alpha*dTime*(ratio3*hp0A[k][l] + ratio2*hp0B[k][l])
//					 + 2*alpha*(hcross0A[k][l] + ratio1*hcross0B[k][l]);
//            hetot[k][l] = dTimeInv*coef1*lambda[k]*lambda[l]*h[k][l];
//
//            Hetot += hetot[k][l]*outerProd(N[k],N[l]);

            H[k][l] = coef1*lambda[k]*lambda[l]*h[k][l]                             // elastic part
			         + (hpA[k][l] + ratio1*hpB[k][l])                               // second derivative of viscous part wrt lp
				     + alpha*alpha*dTime*(ratio3*hp0A[k][l] + ratio2*hp0B[k][l])    // second derivative of viscous part wrt lp0 (parametric)
					 + 2*alpha*(hcross0A[k][l] + ratio1*hcross0B[k][l]);            // cross derivative of viscous part wrt lp and lp0 (parametric)
            if (k == l) {
//              hetot[k][l] += coef1*lambda[k]*t[k];
              H[k][l] += coef1*lambda[k]*t[k];
            }
            Hll[k][l] = H[k][l];
          }
          Hll[k][3] = 1.0;
          Hll[3][k] = 1.0;
        }
        Hll[3][3] = 0.0;

        H.solve(dl,sig,true);
        Hll.solve(dll,sigll,true);

        // update eigenvalues
        double coef2 = 2*dTime;
        for (unsigned int k=0; k < 3; k++) {
          lambda[k] *= std::exp(-coef2*dll[k]);
          lp[k] *= (1/ratio0);
          lp[k] += dll[k];
		}

        // done at every iteration to allow for the parametric dependence on Cpar to be accounted for correctly
        dFv = 0.0e0;
        for (unsigned int k=0; k < 3; k++) dFv += std::exp(dTime*lp[k])*N[k];
        Fv1 = dFv*Fv0;

        // check for convergence
        if (normL2(dl) < PREC) break;
      }
    }

    // elastic deformation
    Ce = covariantPush(C,Fv1);

//    // TEST: RECONSTRUCTING EVERYTHING ELASTIC FROM EIGS
    double lamCe[3],te[3],dtee[3],he[3][3];
    double dNee,Cmee;
    SYM_TENSOR NCe[3];
    Ce.eigenSplit(lamCe,NCe);

    double Wee = potential->storedThMEnergy(material,extPar,lamCe,te,he,Th1,dtee,dNee,Cmee,true,true);

//    SYM_TENSOR SbarE,SbarEE;
//    SbarEE = 0.0e0;
//    for (unsigned int i=0; i<3; i++) SbarEE += 2*te[i]*NCe[i];
//    SbarE = contravariantPull(SbarEE,Fv1);
//    std::cout<<"SbarE = "<<SbarE<<std::endl;
//
//    SYM_TENSOR4 Mebar,Me;
//    SYM_TENSOR sige;
//    sige = 0.0e0;
//    for (unsigned int k=0; k < 3; k++) sige += 2*lamCe[k]*te[k]*NCe[k];
//    std::cout<<"sige = "<<sige<<std::endl;
//
//    SYM_TENSOR dLogCee[SYM_TENSOR::MEMSIZE];
//	SYM_TENSOR d2LogCee[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
//	SYM_TENSOR logCee = 0.5*log(Ce,dLogCee,d2LogCee, first || second, second);
//    const SYM_TENSOR *q = *d2LogCee;
//    unsigned int sz = SYM_TENSOR::MEMSIZE;
//
//    SYM_TENSOR4 MeTotal;
//    MeTotal = 0.0e0;
//    for (unsigned int k=0; k < 3; k++){
//      for (unsigned int l=0; l < 3; l++) {
//        MeTotal += (4*he[k][l])*outerProd(NCe[k],NCe[l]);
//        if (k == l) continue;
//        double dle = lamCe[l]-lamCe[k];
//        double coefTeste;
//        if (std::fabs(dle) > 1.e-16)
//          coefTeste = 2*(te[l]-te[k])/dle;
//        else
//          coefTeste = 2*(he[l][l]-he[k][l]);
//        MeTotal.addIJKL(coefTeste,NCe[k],NCe[l]);
//      }
//    }
//
//    SYM_TENSOR4 MeeTotal;
//    MeeTotal = contravariantPull(MeTotal,Fv1);
//    std::cout<<"MeTotal = "<<MeeTotal<<std::endl;
//    // END OF TEST

    // elastic free energy
    SYM_TENSOR Sbar,dSbar;
    SYM_TENSOR4 Mbar;
	Wev = potential->storedThMEnergy(material,extPar,Ce,Th1,Sbar,dNe,Mbar,dSbar,Cme,
                                        first,second);
    if (update) intPar1[0] = Wev;

    SYM_TENSOR4 Mv;

    // stresses
    if (first) {
      S = contravariantPull(Sbar,Fv1);
//      std::cout<<"S elastic = "<<S<<std::endl;
      Ne = dNe;
      dN = dNe;
    }

    // compute viscous dissipation
    double WvA = 0.0e0;
    double WvB = 0.0e0;
    if (dTime > 0.0e0) {
      double dTimeInv = 1.0/dTime;

      // compute viscous Cauchy-Green deformation
      ALG::RightCauchyGreen(Fv0,Cv0);
	  ALG::RightCauchyGreen(Fv1,Cv1);
	  Cvpar = (1.0-alpha)*Cv0+alpha*Cv1;

      // compute incremental viscous deformations
	  SYM_TENSOR dCv = covariantPush(Cv1,Fv0);
	  SYM_TENSOR dLogCv[SYM_TENSOR::MEMSIZE];
	  SYM_TENSOR d2LogCv[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
	  double coef1 = (0.5*ratio0)/dTime; // accounting for temperature influence
	  SYM_TENSOR Dv = coef1*log(dCv,dLogCv,d2LogCv,first || second,second);

	  SYM_TENSOR dLogCe[SYM_TENSOR::MEMSIZE];
	  SYM_TENSOR d2LogCe[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
	  SYM_TENSOR logCe = 0.5*log(Ce,dLogCe,d2LogCe, first || second, second);

	  double lDv[3];
	  SYM_TENSOR NDv[3];
	  Dv.eigenSplit(lDv,NDv);

	  // compute viscous dissipation potential
	  SYM_TENSOR sigV1a,sigV2a,sigV1b,sigV2b,dsigV1,dsigV2;  // corresponding derivatives with respect to Dv
      SYM_TENSOR4 hv11a,hv22a,hv12a,hv11b,hv22b,hv12b;       // corresponding derivatives with respect to Dv
	  double dNv,Cmv;
      SYM_TENSOR4 MvA,MvB;

      // temperature corrected dissipation terms
      WvA = dTime*viscous->dissipatedEnergy(material,extPar,Dv,dLogCv,d2LogCv,Cvpar,Th0,
	                                 sigV1a,sigV2a,dNv,hv11a,hv22a,hv12a,dsigV1,dsigV2,Cmv,dTime,
                                     first,second);
      WvB = dTime*viscous->dissipatedEnergy(material,extPar,Dv,dLogCv,d2LogCv,Cvpar,Th,
	                                 sigV1b,sigV2b,dNv,hv11b,hv22b,hv12b,dsigV1,dsigV2,Cmv,dTime,
                                     first,second);

      SYM_TENSOR sigVTot = alpha*dTime*(ratio3*sigV1a+ratio2*sigV1b) + (sigV2a+ratio1*sigV2b);

      unsigned int sz = SYM_TENSOR::MEMSIZE;

//      // TEST: RECONSTRUCTING STRESSES FROM sigVTot USING dLogCv AND dLogCe
//      SYM_TENSOR Sv,Se;
//      for (unsigned int ij=0; ij < sz; ij++) Sv[ij] = innerProd2(sigVTot,dLogCv[ij]);
//      for (unsigned int ij=0; ij < sz; ij++) Se[ij] = innerProd2(sigVTot,dLogCe[ij]);
////      std::cout<<"Sv = "<<Sv<<std::endl;
////      std::cout<<"Se = "<<Se<<std::endl;
//      // END OF TEST

      const SYM_TENSOR *p = *d2LogCv;

      // contribution to entropy
	  if (first) {
        SYM_TENSOR val = sigV2a+ratio1*sigV2b;
        dN += (innerProd2(val,ratio3*dTime*Dv) + alpha*dTime*dTh*dNv
	           + dTime*ratio3*(WvB-WvA))/Th1;
	  }

      // contributions to tangent terms
	  if (second) {
        SYM_TENSOR4 val1 = hv12a+ratio1*hv12b;
        SYM_TENSOR4 val2 = hv22a+ratio1*hv22b;
		SYM_TENSOR4 val3 = alpha*dTime*val1 + ratio0*val2;

        SYM_TENSOR dSv,dsigV;
   		dsigV = alpha*ratio2*(alpha*dTime*dsigV1+ratio0*dsigV2)
	             + (ratio3/Th1)*innerProd2(val3,ratio3*Dv)
	             + sigV2b/Th0 + alpha*dTime*ratio3*(sigV1b-sigV1a)/Th1;

//        // TEST: SINCE THE ACTUAL STRESSES COME USING sigVTot AND dLogCe, SHOULDN'T dSv ALSO COME USING dLogCe INSTEAD OF dLogCv?
//        SYM_TENSOR dSv1;
//        for (unsigned int ij=0; ij < sz; ij++) dSv1[ij] = innerProd2(dsigV,dLogCe[ij]);
//        std::cout<<"dS visco = "<<dSv1<<std::endl;
//        // END OF TEST

        for (unsigned int ij=0; ij < sz; ij++) dSv[ij] = innerProd2(dsigV,dLogCv[ij]);
		dS = contravariantPull(dSv,Fv0);
//		std::cout<<"dS visco = "<<dS<<std::endl;

//        // TEST: ATTEMPTS TO RECONSTRUCT Mv FROM hvtot
//        double hvlam[3][3];
// 	      for (unsigned int k=0; k < 3; k++){
//          for (unsigned int l=0; l < 3; l++) {
//            if (k==l)
//              hvlam[k][l] = (0.25*hvtot[k][l] - 0.5*sigV[k])/(lDv[k]*lDv[k]);
//            else
//              hvlam[k][l] = 0.0e0;
//          }
//        }
//
// 	      SYM_TENSOR4 MvTotal1;
// 	      MvTotal1 = 0.0e0;
// 	      for (unsigned int k=0; k < 3; k++){
//          for (unsigned int l=0; l < 3; l++) {
//            MvTotal1 += (4*hvlam[k][l])*outerProd(NDv[k],NDv[l]);
//            if (k == l) continue;
//            double dl = lDv[l]-lDv[k];
//            double coefTest1;
//            if (std::fabs(dl) > 1.e-16)
//              coefTest1 = (sigV[l]/lDv[l]-sigV[k]/lDv[k])/dl;
//            else
//              coefTest1 = 2*(hvlam[l][l]-hvlam[k][l]);
//            MvTotal1.addIJKL(coefTest1,NDv[k],NDv[l]);
//          }
//	      }
//	      std::cout<<"MvTotal1 = "<<MvTotal1<<std::endl;
//
//
// 	      SYM_TENSOR4 MvTotal;
// 	      MvTotal = 0.0e0;
// 	      for (unsigned int k=0; k < 3; k++){
//          for (unsigned int l=0; l < 3; l++) {
//            MvTotal += (4*hvtot[k][l])*outerProd(NDv[k],NDv[l]);
//            if (k == l) continue;
//            double dl = lDv[l]-lDv[k];
//            double coefTest;
//            if (std::fabs(dl) > 1.e-16)
//              coefTest = 2*(sigV[l]-sigV[k])/dl;
//            else
//              coefTest = 2*(hvtot[l][l]-hvtot[k][l]);
//            MvTotal.addIJKL(coefTest,NDv[k],NDv[l]);
//          }
//	      }
//	      std::cout<<"MvTotal = "<<MvTotal<<std::endl;
//        // END OF TEST

        SYM_TENSOR4 hv;
		hv = alpha*alpha*dTime*(ratio3*hv11a+ratio2*hv11b)
            + 2*alpha*val1 + dTimeInv*ratio0*val2;
        for (unsigned int ij=0; ij < sz; ij++){
          for (unsigned int kl=0; kl < sz; kl++, p++){
            Mv[ij][kl] = innerProd2(dLogCv[ij],innerProd2(hv,dLogCv[kl]))
                             +2*innerProd2(sigVTot,*p);
          }
        }
		M = contravariantPull(Mv,Fv0);
//		std::cout<<"Mv = "<<M<<std::endl;

        SYM_TENSOR val4 = innerProd2(val2,ratio3*Dv) + 2*(sigV2b-sigV2a);

        Cm = alpha*dTime*ratio2*(2*innerProd(dsigV2,ratio3*Dv)/Th0 + alpha*Cmv)
	          + 2*dTime*(alpha*dNv - (WvB-WvA)/Th1)*ratio3/Th1
	          + innerProd2(val4,ratio3*dTime*Dv)/(Th1*Th1);
//        std::cout<<"Cm visco = "<<Cm<<std::endl;
      }
    }

    // tangents
    if (second) {
      SYM_TENSOR dStot;
      dStot = -dS;
//	  dS += contravariantPull(dSbar,Fv1);
	  dS = contravariantPull(dSbar,Fv1);
      M = contravariantPull(Mbar,Fv1);
//      std::cout<<"M elastic = "<<M<<std::endl;

      // correction for variation of eigenvalues (dq/dC)
      SYM_TENSOR4 MbarE,MbarV,MbarTot;
      MbarE = M;
      MbarV = contravariantPull(Mv,Fv0);
      MbarTot = MbarE + MbarV;
      MbarTot.invert();
      MbarE -= MbarE*MbarTot*MbarE;
//      std::cout<<"M corrected for eigenvalues = "<<MbarE<<std::endl;
//      M = MbarE;

      // correction for variation of eigenvectors (dN/dC)
      SYM_TENSOR4 MbarN;
      MbarN = 0.0e0;
      SYM_TENSOR4 dNdC[3];
      static const SYM_TENSOR II = SYM_TENSOR::identity();
      SYM_TENSOR lambdaI,Cor,CorInv;
      double dummy;

      for (unsigned int k=0; k < 3; k++) {
        lambdaI = lambda[k]*II;
        Cor = lambdaI - C;
        CorInv = Cor.inverse(dummy);
        dNdC[k] = outerProd(N[k],CorInv);
//        std::cout<<"dNdC["<<k<<"] = "<<dNdC[k]<<std::endl;
        MbarN += 2*te[k]*dNdC[k];
      }
      // SOMETHING IS WRONG HERE, CORRECTION NOT INCLUDED IN THE FOLLOWING

//      MbarN = innerProd2(MbarTot,dNdC);
//      std::cout<<"MbarN = "<<MbarN<<std::endl;

      dStot += contravariantPull(dSbar,Fv1);
      SYM_TENSOR4 Corr;
      Corr = M*MbarTot;
      dS -= Corr*dStot;
//      std::cout<<"dS corrected = "<<dS<<std::endl;

      Cm += Cme;
//      std::cout<<"Cm viscous+elastic = "<<Cm<<std::endl;

//      // TEST: TANGENT PASSING THROUGH EPSILONS
//      ShortSqrMatrix htot(3),hcorr(3),hvt(3),het(3);
//
//      for (unsigned int i=0; i<3; i++){
//        for (unsigned int j=0; j<3; j++){
//          hvt[i][j] = hvtot[i][j];
//          het[i][j] = hetot[i][j];
//          hcorr[i][j] = 0.0e0;
//          if (i==j) hcorr[i][j] = 1. - hetot[i][j]/(hetot[i][j]+hvtot[i][j]);
//          htot[i][j] = hetot[i][j]*hcorr[i][j];
//          if (i==j) htot[i][j] -= sigV[i]/(2*lambda[i]*lambda[i]);
//        }
//      }
//
//      unsigned int sz = SYM_TENSOR::MEMSIZE;
////      const SYM_TENSOR *q = *d2LogCee;
////      const SYM_TENSOR *q = *d2LogCpr;
//
//      SYM_TENSOR SigV;
//      SYM_TENSOR4 Keps,Kelastic;
//      SigV = 0.0e0;
//      Kelastic = 0.0e0;
//      Keps = 0.0e0;
//      for (unsigned int i=0; i<3; i++){
//        SigV += (sigV[i]/lamCe[i])*NCe[i];
//        for (unsigned int j=0; j<3; j++){
//          Kelastic += hetot[i][j]*outerProd(NCe[i],NCe[j]);
//          Keps += htot[i][j]*outerProd(NCe[i],NCe[j]);
////          Kelastic += hetot[i][j]*outerProd(N[i],N[j]);
////          Keps += htot[i][j]*outerProd(N[i],N[j]);
//          if (i == j) continue;
//          double dl2 = lamCe[j]-lamCe[i];
//          double coefTest2,coefTest3;
//          if (std::fabs(dl2) > 1.e-16) {
//            coefTest2 = (sigV[j]/lamCe[j]-sigV[i]/lamCe[i])/dl2;
//            coefTest3 = coefTest2;
//          }
//          else {
//            coefTest2 = 2*(htot[j][j]-htot[i][j]);
//            coefTest3 = 2*(hetot[j][j]-hetot[i][j]);
//          }
//          Keps.addIJKL(coefTest2,NCe[i],NCe[j]);
//          Kelastic.addIJKL(coefTest3,NCe[i],NCe[j]);
//        }
//      }
//      std::cout<<"SigV = "<<SigV<<std::endl;
//      std::cout<<"Keps = "<<Keps<<std::endl;
//      std::cout<<"Kelastic = "<<Kelastic<<std::endl;
//
//      SYM_TENSOR4 Mepsilon,Melastic;
//      for (unsigned int ij=0; ij < sz; ij++){
//        for (unsigned int kl=0; kl < sz; kl++, q++){
//          Mepsilon[ij][kl] = innerProd2(dLogCee[ij],innerProd2(Keps,dLogCee[kl]))
//                         +2*innerProd2(SigV,*q);
//          Melastic[ij][kl] = innerProd2(dLogCee[ij],innerProd2(Kelastic,dLogCee[kl]))
//                             +2*innerProd2(SigV,*q);
////          Mepsilon[ij][kl] = innerProd2(dLogCpr[ij],innerProd2(Keps,dLogCpr[kl]))
////                         +2*innerProd2(SigV,*q);
////          Melastic[ij][kl] = innerProd2(dLogCpr[ij],innerProd2(Kelastic,dLogCpr[kl]))
////                             +2*innerProd2(SigV,*q);
//        }
//      }
////      std::cout<<"Melastic = "<<Melastic<<std::endl;
////      std::cout<<"Mepsilon = "<<Mepsilon<<std::endl;
//
//      SYM_TENSOR4 Mel,Meps;
//      Meps = contravariantPull(Mepsilon,Fv1);
//      Mel = contravariantPull(Melastic,Fv1);
//      std::cout<<"Meps = "<<Meps<<std::endl;
//      std::cout<<"Mel = "<<Mel<<std::endl;
//      // END OF TEST

//      // NUMERICAL TANGENT
//      SYM_TENSOR4 Mnum;
//      SYM_TENSOR Snum,dSnum;
//      double dNnum,Cmnum;
//      Cmnum = numericalTangent(material,extPar,C,Th0,Th1,Snum,Fv0,Fv1,dNnum,dTime,Mnum,dSnum,Cmnum,update,first,second);

//      std::cout<<"Mnum = "<<Mnum<<std::endl;
//      std::cout<<"dSnum = "<<dSnum<<std::endl;
//      std::cout<<"Snum = "<<Snum<<std::endl;
//      std::cout<<"dNnum = "<<dNnum<<std::endl;
//      std::cout<<"Cmnum = "<<Cmnum<<std::endl;

      M = MbarE; // USING ANALYTIC VERSION
//      M = Mnum; // USING NUMERICAL VERSION
//      dS = dSnum; // USING NUMERICAL VERSION
//      Cm = Cmnum; // USING NUMERICAL VERSION
    }

    double Wv = (ratio3*WvA + ratio2*WvB);

    // what would WTv represent and how could it be calculated? so far, left untouched

	return Wev-intPar0[0]+Wv;
  }

  // compute tangent analytically
  double numericalTangent(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const SYM_TENSOR& Cimp,double Th0,double Th1imp,SYM_TENSOR& S,
                              const TENSOR& Fv0,TENSOR& Fv1imp,
                              double& dN,double dTime,SYM_TENSOR4& M,SYM_TENSOR& dS,double& Cm,
                              bool update,bool first,bool second)
  throw (UpdateFailedException) {

    static const unsigned int ITMAX = 20;
    static const double PREC = 1.e-12;
    static const double TOLE = 1.e-06;
    static const double ONE_THIRD = 1./3.;

    dN = 0.0e0;
    double dNe,Cme;
    SYM_TENSOR Ce,Cv,Cv1,Cv0,Cvpar;

    SYM_TENSOR dFv;
    SYM_TENSOR C;
    TENSOR Fv1;
    Fv1 = Fv1imp;

    double Th1;

    unsigned int sz = SYM_TENSOR::MEMSIZE+1;

    double pertu = 1.0e-8;
    double coef = 0.5/pertu;
    double Wplus=0.0,Wminus=0.0;
    MatLibArray flux(sz),Splus(sz),Sminus(sz),Sdiff(sz);
    MatLibMatrix Mdiff(sz);
    Mdiff = 0.0e0;

    for (unsigned int ij=0; ij <(sz-1); ij++) flux[ij] = Cimp[ij];
    flux[sz-1] = Th1imp;

    // perturbation loop (follows MatlConsistencyTest.cpp)
    for (unsigned int k=0; k < sz; k++) {

      double dNv,Nv;

      // positive perturbation
      flux[k] += pertu;
      for (unsigned int ij=0; ij <(sz-1); ij++) C[ij] = flux[ij];
      Th1 = flux[sz-1];

      // temperature correction
      double alpha = material.getDoubleProperty("THVHE_ALGORITHMIC_PARAMETER");
      double Th = (1.0-alpha)*Th0+alpha*Th1;
      double dTh = Th1-Th0;

      // get temperature ratio(s)
      double ratio1 = dTh/Th0;
      double ratio0 = Th1/Th0;
      double ratio2 = dTh/Th1;
      double ratio3 = Th0/Th1;

      SYM_TENSOR SvPlus;
      double WePlus,WvPlus;
      double WvA,WvB;

	  if (update) {

      // compute elastic predictor strain
      Ce = covariantPush(C,Fv0);
	  // which implies the following initialization
	  Fv1 = Fv0;

      // compute principal stretches
      double lambda[3],t[3],h[3][3];
      SYM_TENSOR N[3];
      Ce.eigenSplit(lambda,N);
      if (isochoric) {
        double J = lambda[0]*lambda[1]*lambda[2];
        double Jinv = std::pow(J,-ONE_THIRD);
        for (unsigned int k=0; k < 3; k++) lambda[k] *= Jinv;
      }

      // additional variables accounting for temperature dependence of the elastic part
      double dte[3];

	  // compute viscous update corrector
      double lp[3],tpA[3],dtpA[3],hpA[3][3];
	  double tpB[3],dtpB[3],hpB[3][3];
      for (unsigned int k=0; k < 3; k++) lp[k] = 0.0e0;
      unsigned int iter=0;

	  // additional variables accounting for parametric and temperature dependence of the viscous part
	  double lp0[3];
	  SYM_TENSOR N0[3];
	  double tp0A[3],dtp0A[3],hp0A[3][3],hcross0A[3][3];
	  double dtp1A[3],hp1A[3][3],hcross1A[3][3];
	  double tp0B[3],dtp0B[3],hp0B[3][3],hcross0B[3][3];
	  double dtp1B[3],hp1B[3][3],hcross1B[3][3];

      double dTimeInv = 1.0/dTime;

	  for (; iter < ITMAX; iter++) {

        // compute elastic "stresses"
        ShortArray sig(3);
        WePlus = potential->storedThMEnergy(material,extPar,lambda,t,h,Th1,dte,dNe,Cme,true,true);
        for (unsigned int k=0; k < 3; k++) sig[k] = 2*lambda[k]*t[k];
        if (isochoric) {
          double tr=ONE_THIRD*(sig[0]+sig[1]+sig[2]);
          for (unsigned int k=0; k < 3; k++) sig[k] -= tr;
        }

		// possible parametric dependence on viscous strains
	    double alpha = material.getDoubleProperty("THVHE_ALGORITHMIC_PARAMETER");
        ALG::RightCauchyGreen(Fv1,Cv1);
        ALG::RightCauchyGreen(Fv0,Cv0);
	    Cvpar = (1.0-alpha)*Cv0+alpha*Cv1;
	    Cvpar.eigenSplit(lp0,N0);

		// compute viscous "stresses"
		double Cmv;
		// both portions of the viscous pseudo-potential receive the same lp and lp
        for (unsigned int k=0; k < 3; k++) lp[k] *= ratio0;
        WvA = dTime*viscous->dissipatedEnergy(material,extPar,lp,tpA,hpA,dtpA,lp0,tp0A,dtp0A,hp0A,hcross0A,dtp1A,hp1A,hcross1A,Th0,Nv,Cmv,true,true);
        WvB = dTime*viscous->dissipatedEnergy(material,extPar,lp,tpB,hpB,dtpB,lp0,tp0B,dtp0B,hp0B,hcross0B,dtp1B,hp1B,hcross1B,Th,Nv,Cmv,true,true);
        WvPlus = (ratio3*WvA + ratio2*WvB);

		SvPlus = 0.0e0;
		ShortArray sigVeig(3);
		for (unsigned int k=0; k < 3; k++){
		  sigVeig[k] = alpha*dTime*(ratio3*tp0A[k]+ratio2*tp0B[k]) + (tpA[k]+ratio1*tpB[k]);
		  SvPlus += sigVeig[k]*N[k];
        }

		// equilibrium condition on the Maxwell branch: first derivative wrt eigenvalues of elastic strains
		for (unsigned int k=0; k < 3; k++) sig[k] -= (tpA[k] + ratio1*tpB[k]) + alpha*dTime*(ratio3*tp0A[k] + ratio2*tp0B[k]); // additional stress term due to parametric dependence on Cpar

        // check for convergence
        double norm = normL2(sig);
        if (norm < TOLE*(norm+TOLE)) break;

        // compute correction
        ShortArray dll(4);
        ShortArray sigll(4);
        for (unsigned int i=0; i<3; i++) sigll[i] = sig[i];
        sigll[3] = lp[0]+lp[1]+lp[2];

        ShortSqrMatrix Hll(4);
        double coef1 = 4*dTime;
        for (unsigned int k=0; k < 3; k++){
          for (unsigned int l=0; l < 3; l++) {
            Hll[k][l] = coef1*lambda[k]*lambda[l]*h[k][l]                             // elastic part
			         + (hpA[k][l] + ratio1*hpB[k][l])               // second derivative of viscous part wrt lp
				     + alpha*alpha*dTime*(ratio3*hp0A[k][l] + ratio2*hp0B[k][l])    // second derivative of viscous part wrt lp0 (parametric)
					 + 2*alpha*(hcross0A[k][l] + ratio1*hcross0B[k][l]);            // cross derivative of viscous part wrt lp and lp0 (parametric)
            if (k == l) {
              Hll[k][l] += coef1*lambda[k]*t[k];
            }
          }
          Hll[k][3] = 1.0;
          Hll[3][k] = 1.0;
        }
        Hll[3][3] = 0.0;

        Hll.solve(dll,sigll,true);

        // update eigenvalues
        double coef2 = 2*dTime;
        for (unsigned int k=0; k < 3; k++) {
          lambda[k] *= std::exp(-coef2*dll[k]);
          lp[k] *= (1/ratio0);
          lp[k] += dll[k];

        // done at every time step to allow for the parametric dependence on Cpar to be accounted for correctly
        dFv = 0.0e0;
        for (unsigned int k=0; k < 3; k++) dFv += std::exp(dTime*lp[k])*N[k];
        Fv1 = dFv*Fv0;
		}

        // check for convergence
        if (normL2(dll) < PREC) break;
      }
        SYM_TENSOR DvPlus,val;
        DvPlus = 0.0e0;
        val = 0.0e0;
        for (unsigned int k=0; k < 3; k++){
          val += (tpA[k]+ratio1*tpB[k])*N[k];
          DvPlus += lp[k]*N[k];
        }
        dNv = (innerProd2(val,ratio3*dTime*DvPlus) + alpha*dTime*dTh*Nv
	           + dTime*ratio3*(WvB-WvA))/Th1;
        dN = dNe + dNv;
      }

	  SYM_TENSOR dCv1plus;
	  dCv1plus = symProd(dFv,dFv);
	  SYM_TENSOR dLogCv[SYM_TENSOR::MEMSIZE];
	  SYM_TENSOR d2LogCv[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
	  double coef1 = (0.5*ratio0)/dTime; // accounting for temperature influence
	  SYM_TENSOR Dvplus = coef1*log(dCv1plus,dLogCv,d2LogCv,first || second,second);

      SYM_TENSOR Ce1;
      Ce1 = contravariantPush(C,Fv1);
	  SYM_TENSOR dLogCe[SYM_TENSOR::MEMSIZE];
	  SYM_TENSOR d2LogCe[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
	  SYM_TENSOR logCe = 0.5*log(Ce1,dLogCe,d2LogCe, first || second, second);

      Wplus = WePlus + WvPlus;
//      for (unsigned int ij=0; ij < sz-1; ij++) Splus[ij] = innerProd2(SvPlus,dLogCv[ij]);
      SYM_TENSOR SvPl;
      for (unsigned int ij=0; ij < sz-1; ij++) SvPl[ij] = innerProd2(SvPlus,dLogCe[ij]);
      SvPlus = contravariantPull(SvPl,Fv1);
      for (unsigned int ij=0; ij < sz-1; ij++) Splus[ij] = SvPlus[ij];
      Splus[sz-1] = dN;
      // end of positive perturbation

      // negative perturbation
      flux[k] -= 2*pertu;
      for (unsigned int ij=0; ij <(sz-1); ij++) C[ij] = flux[ij];
      Th1 = flux[sz-1];

      // temperature correction
      Th = (1.0-alpha)*Th0+alpha*Th1;
      dTh = Th1-Th0;

      // get temperature ratio(s)
      ratio1 = dTh/Th0;
      ratio0 = Th1/Th0;
      ratio2 = dTh/Th1;
      ratio3 = Th0/Th1;

      SYM_TENSOR SvMinus;
      double WeMinus,WvMinus;

	  if (update) {

      // compute elastic predictor strain
      Ce = covariantPush(C,Fv0);
	  // which implies the following initialization
	  Fv1 = Fv0;

      // compute principal stretches
      double lambda[3],t[3],h[3][3];
      SYM_TENSOR N[3];
      Ce.eigenSplit(lambda,N);
      if (isochoric) {
        double J = lambda[0]*lambda[1]*lambda[2];
        double Jinv = std::pow(J,-ONE_THIRD);
        for (unsigned int k=0; k < 3; k++) lambda[k] *= Jinv;
      }

      // additional variables accounting for temperature dependence of the elastic part
      double dte[3];

	  // compute viscous update corrector
      double lp[3],tpA[3],dtpA[3],hpA[3][3];
	  double tpB[3],dtpB[3],hpB[3][3];
      for (unsigned int k=0; k < 3; k++) lp[k] = 0.0e0;
      unsigned int iter=0;

	  // additional variables accounting for parametric and temperature dependence of the viscous part
	  double lp0[3];
	  SYM_TENSOR N0[3];
	  double tp0A[3],dtp0A[3],hp0A[3][3],hcross0A[3][3];
	  double dtp1A[3],hp1A[3][3],hcross1A[3][3];
	  double tp0B[3],dtp0B[3],hp0B[3][3],hcross0B[3][3];
	  double dtp1B[3],hp1B[3][3],hcross1B[3][3];

      double dTimeInv = 1.0/dTime;

	  for (; iter < ITMAX; iter++) {

        // compute elastic "stresses"
        ShortArray sig(3);
        WeMinus = potential->storedThMEnergy(material,extPar,lambda,t,h,Th1,dte,dNe,Cme,true,true);
        for (unsigned int k=0; k < 3; k++) sig[k] = 2*lambda[k]*t[k];
        if (isochoric) {
          double tr=ONE_THIRD*(sig[0]+sig[1]+sig[2]);
          for (unsigned int k=0; k < 3; k++) sig[k] -= tr;
        }

		// possible parametric dependence on viscous strains
	    double alpha = material.getDoubleProperty("THVHE_ALGORITHMIC_PARAMETER");
        ALG::RightCauchyGreen(Fv1,Cv1);
        ALG::RightCauchyGreen(Fv0,Cv0);
	    Cvpar = (1.0-alpha)*Cv0+alpha*Cv1;
	    Cvpar.eigenSplit(lp0,N0);

		// compute viscous "stresses"
		double Cmv;
		// both portions of the viscous pseudo-potential receive the same lp and lp
        for (unsigned int k=0; k < 3; k++) lp[k] *= ratio0;
        WvA = dTime*viscous->dissipatedEnergy(material,extPar,lp,tpA,hpA,dtpA,lp0,tp0A,dtp0A,hp0A,hcross0A,dtp1A,hp1A,hcross1A,Th0,Nv,Cmv,true,true);
        WvB = dTime*viscous->dissipatedEnergy(material,extPar,lp,tpB,hpB,dtpB,lp0,tp0B,dtp0B,hp0B,hcross0B,dtp1B,hp1B,hcross1B,Th,Nv,Cmv,true,true);
        WvMinus = (ratio3*WvA + ratio2*WvB);

		SvMinus = 0.0e0;
		ShortArray sigVeig(3);
		for (unsigned int k=0; k < 3; k++){
		  sigVeig[k] = alpha*dTime*(ratio3*tp0A[k]+ratio2*tp0B[k]) + (tpA[k]+ratio1*tpB[k]);
		  SvMinus += sigVeig[k]*N[k];
        }

		// equilibrium condition on the Maxwell branch: first derivative wrt eigenvalues of elastic strains
		for (unsigned int k=0; k < 3; k++) sig[k] -= (tpA[k] + ratio1*tpB[k]) + alpha*dTime*(ratio3*tp0A[k] + ratio2*tp0B[k]); // additional stress term due to parametric dependence on Cpar

        // check for convergence
        double norm = normL2(sig);
        if (norm < TOLE*(norm+TOLE)) break;

        // compute correction
        ShortArray dll(4);
        ShortArray sigll(4);
        for (unsigned int i=0; i<3; i++) sigll[i] = sig[i];
        sigll[3] = lp[0]+lp[1]+lp[2];

        ShortSqrMatrix Hll(4);
        double coef1 = 4*dTime;
        for (unsigned int k=0; k < 3; k++){
          for (unsigned int l=0; l < 3; l++) {
            Hll[k][l] = coef1*lambda[k]*lambda[l]*h[k][l]                             // elastic part
			         + (hpA[k][l] + ratio1*hpB[k][l])               // second derivative of viscous part wrt lp
				     + alpha*alpha*dTime*(ratio3*hp0A[k][l] + ratio2*hp0B[k][l])    // second derivative of viscous part wrt lp0 (parametric)
					 + 2*alpha*(hcross0A[k][l] + ratio1*hcross0B[k][l]);            // cross derivative of viscous part wrt lp and lp0 (parametric)
            if (k == l) {
              Hll[k][l] += coef1*lambda[k]*t[k];
            }
          }
          Hll[k][3] = 1.0;
          Hll[3][k] = 1.0;
        }
        Hll[3][3] = 0.0;

        Hll.solve(dll,sigll,true);

        // update eigenvalues
        double coef2 = 2*dTime;
        for (unsigned int k=0; k < 3; k++) {
          lambda[k] *= std::exp(-coef2*dll[k]);
          lp[k] *= (1/ratio0);
          lp[k] += dll[k];

        // done at every time step to allow for the parametric dependence on Cpar to be accounted for correctly
        dFv = 0.0e0;
        for (unsigned int k=0; k < 3; k++) dFv += std::exp(dTime*lp[k])*N[k];
        Fv1 = dFv*Fv0;
		}

        // check for convergence
        if (normL2(dll) < PREC) break;
      }
        SYM_TENSOR DvMinus,valMinus;
        DvMinus = 0.0e0;
        valMinus = 0.0e0;
        for (unsigned int k=0; k < 3; k++){
          valMinus += (tpA[k]+ratio1*tpB[k])*N[k];
          DvMinus += lp[k]*N[k];
        }
        dNv = (innerProd2(valMinus,ratio3*dTime*DvMinus) + alpha*dTime*dTh*Nv
	           + dTime*ratio3*(WvB-WvA))/Th1;
        dN = dNe + dNv;
      }

      SYM_TENSOR dCv1minus;
	  dCv1minus = symProd(dFv,dFv);
	  double coef3 = (0.5*ratio0)/dTime; // accounting for temperature influence
	  SYM_TENSOR Dvminus = coef3*log(dCv1minus,dLogCv,d2LogCv,first || second,second);

      SYM_TENSOR Ce2;
      Ce2 = contravariantPush(C,Fv1);
	  SYM_TENSOR logCe2 = 0.5*log(Ce2,dLogCe,d2LogCe, first || second, second);

      Wminus = WeMinus + WvMinus;

      SYM_TENSOR SvMin;
      for (unsigned int ij=0; ij < sz-1; ij++) SvMin[ij] = innerProd2(SvMinus,dLogCe[ij]);
      SvMinus = contravariantPull(SvMin,Fv1);
      for (unsigned int ij=0; ij < sz-1; ij++) Sminus[ij] = SvMinus[ij];
      Sminus[sz-1] = dN;
//      std::cout<<"coef*(dNpl - dNmin) = "<<coef*(Splus[sz-1] - Sminus [sz-1])<<std::endl;

      // compute derivatives
      Sdiff[k] = coef*(Wplus-Wminus);

      for (unsigned int l=0; l < sz; l++) Mdiff[l][k] = coef*(Splus[l]-Sminus[l]);

      // restore perturbation
      flux[k] += pertu;
    }

    SYM_TENSOR4 II;
    II = SYM_TENSOR4::contravariantIdentity();

    for (unsigned int i=0; i < sz-1; i++){
      for (unsigned int j=0; j < sz-1; j++){
        M[i][j] = Mdiff[i][j]*II[j][j];
      }
      S[i] = 2*Sdiff[i];
      dS[i] = Mdiff[i][sz-1];
    }
    S = contravariant(S);
    dS = contravariant(dS);

    dN = Sdiff[sz-1];
    Cm = Mdiff[sz-1][sz-1];

    return Cm;
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
