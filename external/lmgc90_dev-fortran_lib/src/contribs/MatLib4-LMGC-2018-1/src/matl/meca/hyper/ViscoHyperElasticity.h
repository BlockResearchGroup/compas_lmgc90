/*
 *  $Id: ViscoHyperElasticity.h 253 2018-05-18 11:59:32Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2018, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_VISCO_ELASTICITY_H
#define ZORGLIB_MATL_MECA_HYPER_VISCO_ELASTICITY_H

// config
#include <matlib_macros.h>

// std C library
#include <cstdio>
// local
#include <matl/meca/hyper/HyperElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for visco-hyperelastic material models.
 */
template <class ALG>
class ViscoHyperElasticity : virtual public HyperElasticity<ALG> {
  
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
  ViscoHyperElasticity(ViscousPotential* v = 0) {
    viscous = v;
  }
  
 public:
    
  // constructor
  ViscoHyperElasticity(typename HyperElasticity<ALG>::Potential& p,ViscousPotential& v)
  : HyperElasticity<ALG>(p) {viscous = &v;}
  
  // copy constructor
  ViscoHyperElasticity(const ViscoHyperElasticity& src)
  : HyperElasticity<ALG>(src) {maxwell = src.maxwell; viscous = src.viscous;}
  
  // destructor
  virtual ~ViscoHyperElasticity() {
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
    if (os) (*os) << "\nVisco-hyperelasticity model:" << std::endl;

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
    
    // check elastic potential
    if (this->potential) this->potential->checkProperties(material,os);

    // check dilatancy model
    if (this->dilatancy) this->dilatancy->checkProperties(material,os);

    // check viscous potential
    if (viscous) viscous->checkProperties(material,os);

    // maxwell branches
    for (unsigned int n=0; n < maxwell.size(); n++)
      maxwell[n]->checkProperties(material,os);
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
    if (this->dilatancy) this->dilatancy->updateProperties(material,extPar);
    if (viscous) viscous->updateProperties(material,extPar);
    for (unsigned int n=0; n < maxwell.size(); n++)
      maxwell[n]->updateProperties(material,extPar);
  }
  
  // how many internal variables ?
  unsigned int nIntVar() const {
    unsigned int n = 1;
    for (unsigned int i=0; i < maxwell.size(); i++)
      n += TENSOR::MEMSIZE+maxwell[i]->nIntPar();
    return n;
  }
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 1 + 2*maxwell.size();}
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
    else if (str == "ENRG")
      return maxwell.size();
    else if (maxwell.size() > 0 && str == "VNRG1")
      return maxwell.size()+1;
    else if (maxwell.size() > 1 && str == "VNRG2")
      return maxwell.size()+2;
    else if (maxwell.size() > 2 && str == "VNRG3")
      return maxwell.size()+3;
    else if (maxwell.size() > 3 && str == "VNRG4")
      return maxwell.size()+4;
    else if (maxwell.size() > 4 && str == "VNRG5")
      return maxwell.size()+5;
    else if (maxwell.size() > 5 && str == "VNRG6")
      return maxwell.size()+6;
    else if (maxwell.size() > 6 && str == "VNRG7")
      return maxwell.size()+7;
    else if (maxwell.size() > 7 && str == "VNRG8")
      return maxwell.size()+8;
    else if (maxwell.size() > 8 && str == "VNRG9")
      return maxwell.size()+9;
    else
      return 2*maxwell.size()+1;
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    if (i < maxwell.size())
      return ConstitutiveModel::TYPE_TENSOR;
    else if (i < 2*maxwell.size()+1)
      return ConstitutiveModel::TYPE_SCALAR;
    else
      return ConstitutiveModel::TYPE_NONE;
  }
  unsigned int indexIntVar(unsigned int i) const {
    if (i < maxwell.size())
      return i*TENSOR::MEMSIZE;
    else if (i < 2*maxwell.size()+1)
      return maxwell.size()*TENSOR::MEMSIZE+i-maxwell.size();
    else
      return maxwell.size()*(TENSOR::MEMSIZE+1)+1;
  }
  std::string labelIntVar(unsigned int i) const {
    char str[64];
    if (i < maxwell.size()) {
      std::sprintf(str,"viscous strain %u",i+1);
      return str;
    }
    else if (i == maxwell.size())
      return "elastically stored energy";
    else if (i < 2*maxwell.size()+1) {
      std::sprintf(str,"viscous stored energy %u",
                   i-static_cast<unsigned int>(maxwell.size()));
      return str;
    }
    else
      return "";
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

    // compute stored energy
    SYM_TENSOR S;
    SYM_TENSOR4 M;
    double W = viscoelasticUpdate(material,extPar,C,S,
                                  state0.internal,state.internal,dTime,M,
                                  update,update || tangent,tangent);
    
    // viscous part
    if (viscous && dTime > 0.0e0) {
      double dTimeInv = 1.0e0/dTime;
      
      // compute strain rate
      SYM_TENSOR Sv;
      SYM_TENSOR4 Mv;
      W += dTime*viscous->dissipatedEnergy(material,extPar,F0,C,Sv,Mv,
                                           dTime,update,tangent);
      if (update) S += Sv;
      if (tangent) M += dTimeInv*Mv;
    }
    
    // compute Piola tensor and Lagrangian tangents
    if (update) ALG::PK2ToPK1(S,F1,P);
    if (tangent) ALG::MaterialToLagrangian(M,S,F1,K);
    
    return W;
  }
  
 protected:
    
  // viscoelastic update (Maxwell branches)
  double viscoelasticUpdate(const MaterialProperties& material,
                            const ParameterSet& extPar,
                            const SYM_TENSOR& C,SYM_TENSOR& S,
                            const MatLibArray& intVar0,MatLibArray& intVar,
                            double dTime,SYM_TENSOR4& M,
                            bool update,bool stress,bool tangent) 
   throw (UpdateFailedException) {

    // compute stored energy
    double W = this->storedEnergy(material,extPar,C,S,M,stress,tangent);
    if (update) intVar[maxwell.size()*TENSOR::MEMSIZE] = W;

    // update Maxwell branches
    unsigned int n0 = maxwell.size()*TENSOR::MEMSIZE+1;
    for (unsigned int n=0; n < maxwell.size(); n++) {

      // get viscous strains
      const TENSOR Fv0(intVar0,n*SYM_TENSOR::MEMSIZE);
      TENSOR Fv1(intVar,n*SYM_TENSOR::MEMSIZE);

      // get internal parameters
      const MatLibArray intPar0(intVar0,maxwell[n]->nIntPar(),n0);
      MatLibArray intPar1(intVar,maxwell[n]->nIntPar(),n0);
      n0 += maxwell[n]->nIntPar();

      // update Maxwell branch
      SYM_TENSOR Sv;
      SYM_TENSOR4 Mv;
      W += maxwell[n]->incrementalPotential(material,extPar,C,Sv,
                                            Fv0,Fv1,intPar0,intPar1,
                                            dTime,Mv,update,stress,tangent);

      if (stress) S += Sv;
      if (tangent) M += Mv;
    }
      
    return W-intVar0[maxwell.size()*TENSOR::MEMSIZE];
  }
};


/**
 * Base class for viscohyperelastic models (Maxwell branches).
 */
template <class ALG>
class ViscoHyperElasticity<ALG>::MaxwellViscoElasticity {
  
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
                                      const SYM_TENSOR&,SYM_TENSOR&,
                                      const TENSOR&,TENSOR&,
                                      const MatLibArray&,MatLibArray&,
                                      double,SYM_TENSOR4&,bool,bool,bool)
   throw (UpdateFailedException) = 0;
};


/**
 * Base class for viscous potentials (Kelvin-Voigt viscohyperelasticity).
 */
template <class ALG>
class ViscoHyperElasticity<ALG>::ViscousPotential {

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
                                  const TENSOR&,const SYM_TENSOR&,SYM_TENSOR&,
                                  SYM_TENSOR4&,double,bool,bool) = 0;
};


/**
 * Base class for (isotropic) visco-hyperelastic potentials,
 * which are function of principal stretches.
 */
template <class ALG>
class SpectralHEViscousPotential
: virtual public ViscoHyperElasticity<ALG>::ViscousPotential {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  
 protected:
    
  // constructor
  SpectralHEViscousPotential() {}
  
 public:
    
  // destructor
  virtual ~SpectralHEViscousPotential() {}
  
  // compute dissipated energy
  double dissipatedEnergy(const MaterialProperties& material,
                          const ParameterSet& extPar,
                          const TENSOR& Fv0,const SYM_TENSOR& Cv,SYM_TENSOR& S,
                          SYM_TENSOR4& M,double dTime,bool first,bool second) {
    
    // compute viscous strain-rate
    SYM_TENSOR dCv;
    dCv = covariantPush(Cv,Fv0);
    double coef = 0.5/dTime;
    SYM_TENSOR dLogC[SYM_TENSOR::MEMSIZE];
    SYM_TENSOR d2LogC[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
    SYM_TENSOR Dv = coef*log(dCv,dLogC,d2LogC,first || second,second);
    
    // compute principal stretches
    double lambda[3],t[3],h[3][3];
    SYM_TENSOR N[3];
    Dv.eigenSplit(lambda,N);
    
    // compute dissipation potential
    SYM_TENSOR sig;
    SYM_TENSOR4 H;
    double phi = dissipatedEnergy(material,extPar,lambda,t,h,
                                  first || second,second);
    if (first || second) {
      sig = 0.0e0;
      for (unsigned int k=0; k < 3; k++) sig += t[k]*N[k];
    }
    if (second) {
      H = 0.0e0;
      for (unsigned int k=0; k < 3; k++)
        for (unsigned int l=0; l < 3; l++) {
          H += h[k][l]*outerProd(N[k],N[l]);
          if (k == l) continue;
          double dl = lambda[l]-lambda[k];
          double coef;
          if (std::fabs(dl) > 1.e-16)
            coef = 0.5*(t[l]-t[k])/dl;
          else
            coef = 0.5*(h[l][l]-h[k][l]);
          H.addIJKL(coef,N[k],N[l]);
        }
    }
      
    // stresses
    unsigned int sz = SYM_TENSOR::MEMSIZE;
    if (first) {
      SYM_TENSOR Sv;
      for (unsigned int ij=0; ij < sz; ij++)
        Sv[ij] = innerProd2(sig,dLogC[ij]);
      S = contravariantPull(Sv,Fv0);
    }
    
    // tangents
    if (second) {
      SYM_TENSOR4 Mv;
      SYM_TENSOR *p = *d2LogC;
      for (unsigned int ij=0; ij < sz; ij++)
        for (unsigned int kl=0; kl < sz; kl++, p++)
          Mv[ij][kl] = innerProd2(dLogC[ij],innerProd2(H,dLogC[kl]))
                      +(2*dTime)*innerProd2(sig,*p);
      M = contravariantPull(Mv,Fv0);
    }
      
    return phi;
  }
    
  // compute dissipated energy from principal stretches
  virtual double dissipatedEnergy(const MaterialProperties&,const ParameterSet&,
                                  const double[],double[],double[][3],
                                  bool,bool) = 0;
};


/**
 * Class for spectral (isotropic) viscohyperelastic models (Maxwell branches).
 */
template <class ALG>
class SpectralMaxwellViscoElasticity
: virtual public ViscoHyperElasticity<ALG>::MaxwellViscoElasticity {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  
 protected:
    
  // isochoric?
  bool isochoric;
  
  // elastic part
  SpectralHEPotential<ALG>* potential;
  
  // viscous part
  SpectralHEViscousPotential<ALG>* viscous;
  
  // instance counter
  unsigned int *count;
  
 public:
    
  // constructor
  SpectralMaxwellViscoElasticity(SpectralHEPotential<ALG>& p,
                                 SpectralHEViscousPotential<ALG>& v,
                                 bool i = false) {
    count = new unsigned int(1);
    isochoric = i;
    potential = &p;
    viscous = &v;
  }
  
  // copy constructor
  SpectralMaxwellViscoElasticity(const SpectralMaxwellViscoElasticity& src) {
    count = src.count;
    (*count)++;
    isochoric = src.isochoric;
    potential = src.potential;
    viscous = src.viscous;
  }
  
  // destructor
  virtual ~SpectralMaxwellViscoElasticity() {
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
                              const SYM_TENSOR& C,SYM_TENSOR& S,
                              const TENSOR& Fv0,TENSOR& Fv1,
                              const MatLibArray& intPar0,MatLibArray& intPar1,
                              double dTime,SYM_TENSOR4& M,
                              bool update,bool first,bool second)
   throw (UpdateFailedException) {
     
    static const unsigned int ITMAX = 20;
    static const double PREC = 1.e-16;
    static const double TOLE = 1.e-08;
    static const double ONE_THIRD = 1./3.;
     
    // compute elastic predictor strain
    SYM_TENSOR Cpr;
    if (update || second) Cpr = covariantPush(C,Fv0);

    // viscoelastic update
    double lambda[3],t[3],h[3][3];
    double lam0[3],lp[3],tp[3],hp[3][3];
    SYM_TENSOR N[3];
    if (update) {
      
      // compute principal stretches
      Cpr.eigenSplit(lambda,N);
      if (isochoric) {
        double J = lambda[0]*lambda[1]*lambda[2];
        double Jinv = std::pow(J,-ONE_THIRD);
        for (unsigned int k=0; k < 3; k++) lambda[k] *= Jinv;
      }
      for (unsigned int k=0; k < 3; k++) lam0[k] = lambda[k];
      
      // compute viscous update corrector
      for (unsigned int k=0; k < 3; k++) lp[k] = 0.0e0;
      unsigned int iter=0;
      for (; iter < ITMAX; iter++) {

        // compute elastic "stresses"
        ShortArray sig(3);
        potential->storedEnergy(material,extPar,lambda,t,h,true,true);
        for (unsigned int k=0; k < 3; k++) sig[k] = 2*lambda[k]*t[k];
        if (isochoric) {
          double tr=ONE_THIRD*(sig[0]+sig[1]+sig[2]);
          for (unsigned int k=0; k < 3; k++) sig[k] -= tr;
        }
        
        // compute viscous "stresses"
        viscous->dissipatedEnergy(material,extPar,lp,tp,hp,iter,true);
        for (unsigned int k=0; k < 3; k++) sig[k] -= tp[k];
        
        // check for convergence
        double norm = normL2(sig);
        if (norm < TOLE*(norm+TOLE)) break;
        
        // compute correction
        ShortArray dl(3);
        ShortSqrMatrix H(3);
        double coef1 = 4*dTime;
        for (unsigned int k=0; k < 3; k++)
          for (unsigned int l=0; l < 3; l++) {
            H[k][l] = coef1*lambda[k]*lambda[l]*h[k][l] + hp[k][l];
            if (k == l) H[k][l] += coef1*lambda[k]*t[k];
          }
        H.solve(dl,sig,true);
        
        // update eigenvalues
        double coef2 = 2*dTime;
        for (unsigned int k=0; k < 3; k++) {
          lambda[k] *= std::exp(-coef2*dl[k]);
          lp[k] += dl[k];
        }
        
        // check for convergence
        if (normL2(dl) < PREC) break;
      }
      
      // update viscous deformation
      SYM_TENSOR dFv;
      dFv = 0.0e0;
      for (unsigned int k=0; k < 3; k++) dFv += std::exp(dTime*lp[k])*N[k];
      Fv1 = dFv*Fv0;
    }
    
    // elastic deformation
    SYM_TENSOR Ce;
    Ce = covariantPush(C,Fv1);
    
    double We,Wv=0.0e0;
    SYM_TENSOR Sbar;
    SYM_TENSOR4 Mbar;
    if (!second || dTime < PREC) {
      // elastic free energy and derivatives
      We = potential->storedEnergy(material,extPar,Ce,Sbar,Mbar,
                                   first,second);
    }
    else {
      double dTimeInv = 1.0/dTime;
      
      // recompute elastic stretches and viscous rates
      if (!update) {
        
        // predictor
        Cpr.eigenSplit(lam0,N);
        if (isochoric) {
          double J0 = lam0[0]*lam0[1]*lam0[2];
          double Jinv = std::pow(J0,-ONE_THIRD);
          for (unsigned int k=0; k < 3; k++) lam0[k] *= Jinv;
        }
       
        // elastic stretches
        for (unsigned int k=0; k < 3; k++)
          lambda[k] = innerProd2(Ce,N[k]);
        if (isochoric) {
          double J = lambda[0]*lambda[1]*lambda[2];
          double Jinv = std::pow(J,-ONE_THIRD);
          for (unsigned int k=0; k < 3; k++) lambda[k] *= Jinv;
        }

        // viscous rates
        if (dTime > 0.0e0) {
          for (unsigned int k=0; k < 3; k++)
            lp[k] = 0.5*std::log(lam0[k]/lambda[k])*dTimeInv;
        }
      }
    
      // elastic energy
      We = potential->storedEnergy(material,extPar,lambda,t,h,true,true);

      if (first) {
        Sbar = 0.0e0;
        for (unsigned int k=0; k < 3; k++) Sbar += (2*t[k])*N[k];
      }

      // tangents
      double tbar[3],hbar[3][3];
      
      /* compute spectral derivatives w.r.t. predictor */
      
      /* not forgetting correction for isochoricity */

      
      // compute viscous dissipation potential
      Wv = viscous->dissipatedEnergy(material,extPar,lp,tp,hp,false,true);

      Mbar = 0.0e0;
      for (unsigned int k=0; k < 3; k++)
        for (unsigned int l=0; l < 3; l++) {
          Mbar += (4*hbar[k][l])*outerProd(N[k],N[l]);
          if (k == l) continue;
          double dl = lam0[l]-lam0[k];
          double coef;
          if (std::fabs(dl) > 1.e-16)
            coef = 2*(tbar[l]-tbar[k])/dl;
          else
            coef = 2*(hbar[l][l]-hbar[k][l]);
          Mbar.addIJKL(coef,N[k],N[l]);
        }
    }
    if (update) intPar1[0] = We;
     
    // stresses
    if (first) {
      S = contravariantPull(Sbar,Fv1);
    }
    
    // tangents
    if (second) {
      M = contravariantPull(Mbar,Fv0);
    }
    
    return We-intPar0[0]+Wv;
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
