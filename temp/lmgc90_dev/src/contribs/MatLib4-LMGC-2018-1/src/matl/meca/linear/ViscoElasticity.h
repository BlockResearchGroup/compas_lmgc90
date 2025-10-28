/*
 *  $Id: ViscoElasticity.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_VISCO_ELASTICITY_H
#define ZORGLIB_MATL_MECA_LINEAR_VISCO_ELASTICITY_H

// config
#include <matlib_macros.h>

// std C library
#include <cstdio>
#include <cstring>
// local
#include <matl/meca/linear/Elasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for (geometrically linear) visco-elastic material models.
 */
template <class ALG>
class ViscoElasticity : virtual public Elasticity<ALG> {
  
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
  ViscoElasticity(ViscousPotential* v = 0) {
    viscous = v;
  }

 public:

  // constructor
  ViscoElasticity(typename Elasticity<ALG>::Potential& p,ViscousPotential& v)
  : Elasticity<ALG>(p) {viscous = &v;}

  // copy constructor
  ViscoElasticity(const ViscoElasticity& src)
  : Elasticity<ALG>(src) {maxwell = src.maxwell; viscous = src.viscous;}
  
  // destructor
  virtual ~ViscoElasticity() {
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
    if (os) (*os) << "\nViscoelasticity model (small strains):" << std::endl;

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
      alpha = material.getDoubleProperty("VE_ALGORITHMIC_PARAMETER");
    }
    catch (NoSuchPropertyException) {
      material.setProperty("VE_ALGORITHMIC_PARAMETER",alpha);
    }
    if (os) (*os) << "\n\talgorithmic parameter = " << alpha << std::endl;
    
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
      n += SYM_TENSOR::MEMSIZE+maxwell[i]->nIntPar();
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
      return ConstitutiveModel::TYPE_SYM_TENSOR;
    else if (i < 2*maxwell.size()+1)
      return ConstitutiveModel::TYPE_SCALAR;
    else
      return ConstitutiveModel::TYPE_NONE;
  }
  unsigned int indexIntVar(unsigned int i) const {
    if (i < maxwell.size())
      return i*SYM_TENSOR::MEMSIZE;
    else if (i < 2*maxwell.size()+1)
      return maxwell.size()*SYM_TENSOR::MEMSIZE+i-maxwell.size();
    else
      return maxwell.size()*(SYM_TENSOR::MEMSIZE+1)+1;
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
     
    // compute incremental potential
    double W = viscoelasticUpdate(material,extPar,eps1,sig,
                                  state0.internal,state.internal,dTime,K,
                                  update,update,tangent);

    // viscous part
    if (viscous && dTime > 0.0e0) {
      
      // get algorithmic parameter
      double alpha = material.getDoubleProperty("VE_ALGORITHMIC_PARAMETER");
      SYM_TENSOR eps = (1.0-alpha)*eps0+alpha*eps1;
      
      // compute strain rate
      double dTimeInv = 1.0/dTime;
      SYM_TENSOR epsDot = dTimeInv*(eps1-eps0);
      SYM_TENSOR Sv1,Sv2;
      SYM_TENSOR4 Mv11,Mv22,Mv12;
      W += dTime*viscous->dissipatedEnergy(material,extPar,eps,epsDot,
                                           Sv1,Sv2,Mv11,Mv22,Mv12,
                                           dTime,update,tangent);
      double coef = alpha*dTime;
      if (update) state.flux += coef*Sv1 + Sv2;
      if (tangent) M += alpha*coef*Mv11 + 2*alpha*Mv12 + dTimeInv*Mv22;
    }

    return W;
  }

 protected:

  // viscoelastic update (Maxwell branches)
  double viscoelasticUpdate(const MaterialProperties& material,
                            const ParameterSet& extPar,
                            const SYM_TENSOR& eps,SYM_TENSOR& sig,
                            const MatLibArray& intVar0,MatLibArray& intVar,
                            double dTime,SYM_TENSOR4& M,
                            bool update,bool stress,bool tangent) 
   throw (UpdateFailedException) {

    // compute stored energy
    double W = this->storedEnergy(material,extPar,eps,sig,M,stress,tangent);
    if (update) intVar[maxwell.size()*SYM_TENSOR::MEMSIZE] = W;

    // update Maxwell branches
    unsigned int n0 = maxwell.size()*SYM_TENSOR::MEMSIZE+1;
    for (unsigned int n=0; n < maxwell.size(); n++) {
      
      // get viscous strains
      const SYM_TENSOR epsV0(intVar0,n*SYM_TENSOR::MEMSIZE);
      SYM_TENSOR epsV1(intVar,n*SYM_TENSOR::MEMSIZE);
      
      // get internal parameters
      const MatLibArray intPar0(intVar0,maxwell[n]->nIntPar(),n0);
      MatLibArray intPar1(intVar,maxwell[n]->nIntPar(),n0);
      n0 += maxwell[n]->nIntPar();
      
      // update Maxwell branch
      SYM_TENSOR sigV;
      SYM_TENSOR4 Mv;
      W += maxwell[n]->incrementalPotential(material,extPar,eps,sigV,
                                            epsV0,epsV1,intPar0,intPar1,
                                            dTime,Mv,update,stress,tangent);
      if (stress) sig += sigV;
      if (tangent) M += Mv;
    }

    return W-intVar0[maxwell.size()*SYM_TENSOR::MEMSIZE];
  }
};


/**
 * Base class for (linear) viscoelastic models (Maxwell branches).
 */
template <class ALG>
class ViscoElasticity<ALG>::MaxwellViscoElasticity {
  
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
                                      const SYM_TENSOR&,SYM_TENSOR&,
                                      const MatLibArray&,MatLibArray&,
                                      double,SYM_TENSOR4&,bool,bool,bool)
    throw (UpdateFailedException) = 0;
};

  
/**
 * Base class for (linear) viscous potentials (Kelvin-Voigt viscoelasticity).
 */
template <class ALG>
class ViscoElasticity<ALG>::ViscousPotential {
  
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
                                  SYM_TENSOR&,SYM_TENSOR&,
                                  SYM_TENSOR4&,SYM_TENSOR4&,
                                  SYM_TENSOR4&,double,bool,bool) = 0;
};


/**
 * Class for standard (linear) viscoelastic models (Maxwell branches).
 */
template <class ALG>
class StdMaxwellViscoElasticity 
: virtual public ViscoElasticity<ALG>::MaxwellViscoElasticity {

 public:

  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:

  // isochoric?
  bool isochoric;

  // elastic part
  typename Elasticity<ALG>::Potential* potential;
  
  // viscous part
  typename ViscoElasticity<ALG>::ViscousPotential* viscous;
  
  // instance counter
  unsigned int *count;
  
 public:
  
  // constructor
  StdMaxwellViscoElasticity(typename Elasticity<ALG>::Potential& p,
                            typename ViscoElasticity<ALG>::ViscousPotential& v,
                            bool i = false) {
    count = new unsigned int(1);
    isochoric = i;
    potential = &p;
    viscous = &v;
  }
  
  // copy constructor
  StdMaxwellViscoElasticity(const StdMaxwellViscoElasticity& src) {
    count = src.count;
    (*count)++;
    isochoric = src.isochoric;
    potential = src.potential;
    viscous = src.viscous;
  }
  
  // destructor
  virtual ~StdMaxwellViscoElasticity() {
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
  unsigned int nIntPar() const {return 1;}
  
  // compute contribution to incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const SYM_TENSOR& eps,SYM_TENSOR& sig,
                              const SYM_TENSOR& epsV0,SYM_TENSOR& epsV1,
                              const MatLibArray& intPar0,MatLibArray& intPar1,
                              double dTime,SYM_TENSOR4& M,bool update,
                              bool first,bool second)
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
    double W = potential->storedEnergy(material,extPar,epsEl,sig,M,
                                       update || first,update || second);
    
    // compute viscous dissipation
    double Wv = 0.0e0;
    if (dTime > 0.0e0) {
      double dTimeInv = 1.0/dTime;
      SYM_TENSOR epsVDot = dTimeInv*(epsV1-epsV0);
      SYM_TENSOR Sv1,Sv2;
      SYM_TENSOR4 Mv11,Mv22,Mv12;
      Wv = dTime*viscous->dissipatedEnergy(material,extPar,epsV0,epsVDot,
                                           Sv1,Sv2,Mv11,Mv22,Mv12,
                                           dTime,false,update || second);

      if (update) {
        // update viscous strain
        SYM_TENSOR dEpsV;
        M += dTimeInv*Mv22;
        M.solve(dEpsV,sig,true);
        epsV1 = epsV0+dEpsV;
        epsEl -= dEpsV;
        
        // update elastic energy
        W = potential->storedEnergy(material,extPar,epsEl,sig,M,first,second);
        intPar1[0] = W;
        
        // update viscous dissipation
        epsVDot = dTimeInv*dEpsV;
        Wv = dTime*viscous->dissipatedEnergy(material,extPar,epsV0,epsVDot,
                                             Sv1,Sv2,Mv11,Mv22,Mv12,
                                             dTime,false,second);
      }
      
      // compute consistent tangent operator
      if (second) {
        SYM_TENSOR4 Mbar;
        Mbar = M+dTimeInv*Mv22;
        Mbar.invert();
        M -= M*Mbar*M;
      }
    }
    
    return W-intPar0[0]+Wv;
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
