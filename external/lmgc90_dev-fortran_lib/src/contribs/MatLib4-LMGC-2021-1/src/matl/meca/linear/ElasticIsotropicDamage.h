/*
 *  $Id: ElasticIsotropicDamage.h 248 2017-12-20 17:53:04Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ELASTIC_ISOTROPIC_DAMAGE_H
#define ZORGLIB_MATL_MECA_LINEAR_ELASTIC_ISOTROPIC_DAMAGE_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/linear/Elasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for isotropic damage potentials.
 */
class IsotropicDamagePotential {
  
 public:

  // destructor
  virtual ~IsotropicDamagePotential() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // number of internal parameters
  virtual unsigned int nIntPar() const = 0;
  
  // dissipated energy
  virtual double dissipatedEnergy(const MaterialProperties&,const ParameterSet&,
                                  const MatLibArray&,MatLibArray&,double,double,
                                  double&,double&,double&,double&,double&,
                                  bool,bool) = 0;
};


/**
 * Base class for linear elasticity models with isotropic damage.
 */
template <class ALG>
class ElasticIsotropicDamage : virtual public Elasticity<ALG> {
  
 public:
  
  // declare local types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:
  
  // associated damage dissipation pseudo-potential
  IsotropicDamagePotential* damage;
  
  // empty constructor
  ElasticIsotropicDamage(IsotropicDamagePotential* d = 0) {
    damage = d;
  }
  
 public:
    
  // constructor
  ElasticIsotropicDamage(typename Elasticity<ALG>::Potential& p,IsotropicDamagePotential& d)
   : Elasticity<ALG>(p) {damage = &d;}
  
  // copy constructor
  ElasticIsotropicDamage(const ElasticIsotropicDamage& src)
   : Elasticity<ALG>(src) {damage = src.damage;}
  
  // destructor
  virtual ~ElasticIsotropicDamage() {
    if (*(this->count) > 1) return;
    if (damage) delete damage;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nElasticity model with isotropic damage (small strains):" << std::endl;

    // density
    try {
      double rho = material.getDoubleProperty("MASS_DENSITY");
      if (os) (*os) << "\n\tmass density = " << rho << std::endl;
    }
    catch (NoSuchPropertyException) {
      if (os) (*os) << "\n\tmass density is not defined" << std::endl;
    }
      
    // check elastic potential
    if (this->potential) this->potential->checkProperties(material,os);
      
    // check dilatancy model
    if (this->dilatancy) this->dilatancy->checkProperties(material,os);

    // check damage potential
    if (damage) damage->checkProperties(material,os);

    // look for algorithmic parameter
    double alpha = 0.5;
    try {
      alpha = material.getDoubleProperty("ED_ALGORITHMIC_PARAMETER");
    }
    catch (NoSuchPropertyException) {
      material.setProperty("ED_ALGORITHMIC_PARAMETER",alpha);
    }
    if (os) (*os) << "\n\talgorithmic parameter = " << alpha << std::endl;
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    if (this->potential) this->potential->rotateProperties(material,R);
    if (this->dilatancy) this->dilatancy->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& material,const ParameterSet& extPar) {
    if (this->potential) this->potential->updateProperties(material,extPar);
    if (this->dilatancy) this->dilatancy->updateProperties(material,extPar);
    if (damage) damage->updateProperties(material,extPar);
  }
  
  // how many internal variables ?
  unsigned int nIntVar() const {return 2+damage->nIntPar();}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 2;}
  unsigned int getIntVar(const std::string& str) const {
    if (str == "DAMG")
      return 0;
    if (str == "ENRG")
      return 1;
    else
      return 2;
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      case 1:
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
        return 1;
        break;
      default:
        return 2;
        break;
    }
  }
  std::string labelIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "damage";
        break;
      case 1:
        return "elastically stored energy";
        break;
      default:
        return "";
        break;
    }
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
    SYM_TENSOR eps(state.grad);
    SYM_TENSOR sig(state.flux);
    SYM_TENSOR4 K(M);

    // compute incremental potential
    double W = internalUpdate(material,extPar,eps,sig,
                              state0.internal,state.internal,
                              dTime,K,update,update,tangent);
      
    return W;
  }
  
 protected:
    
  // internal variable update
  double internalUpdate(const MaterialProperties& material,
                        const ParameterSet& extPar,
                        const SYM_TENSOR& eps,SYM_TENSOR& sig,
                        const MatLibArray& intVar0,MatLibArray& intVar1,
                        double dTime,SYM_TENSOR4& M,
                        bool update,bool stress,bool tangent)
   throw (UpdateFailedException) {

    static const unsigned int ITMAX=50;
    static const double PRECISION = 1.0e-12;
    static const double TOLERANCE = 1.0e-08;
     
    // get algorithmic parameter
    unsigned int maxIt;
    if (material.checkProperty("ED_MAX_ITER_PARAMETER"))
      maxIt = material.getIntegerProperty("ED_MAX_ITER_PARAMETER");
    else
      maxIt = ITMAX;

    unsigned int nIntPar = damage->nIntPar();
    MatLibArray intPar0(intVar0,nIntPar,2);
    MatLibArray intPar1(intVar1,nIntPar,2);

    // compute a few useful quantities
    double d0 = intVar0[0];
    double d1 = intVar1[0];
    double dTInv = 0.e0;
    if (dTime > 0.e0) dTInv = 1.0e0/dTime;
    double We = this->storedEnergy(material,extPar,eps,sig,M,
                                   stress || tangent,tangent);
    if (update) intVar1[1] = We;
     
    // get algorithmic parameter
    double alpha = material.getDoubleProperty("ED_ALGORITHMIC_PARAMETER");
    double d = (1.0-alpha)*d0+alpha*d1;
    double val = alpha*dTime;
     
    // compute damage update
    double ddot = dTInv*(d1-d0);
    double Y,Yd,K,Kd,Kdd;
    double fact = 1.0-d1;
    if (update && d1 < 1.0e0) {
      // predictor step
      damage->dissipatedEnergy(material,extPar,intPar0,intPar1,
                               ddot,d,Y,Yd,K,Kd,Kdd,true,true);
      double test = -We+Y+val*Yd;
      // corrector step
      if (test < 0.0e0) {
        double dMin=d0,dMax=1.0e0;
        double df = dTInv*K+alpha*(2*Kd+val*Kdd);
        double test0 = std::fabs(test);
        unsigned int iter = 0;
        for (; iter < maxIt; iter++) {
          double dd = -test/df;
          if (std::fabs(dd) < PRECISION) break;
          d1 += dd;
          if ((d1 <= dMin) || (d1 > dMax)) d1 = 0.5*(dMin+dMax);
          fact = 1.0-d1;
          d = (1.0-alpha)*d0+alpha*d1;
          ddot = dTInv*(d1-d0);
          damage->dissipatedEnergy(material,extPar,intPar0,intPar1,
                                   ddot,d,Y,Yd,K,Kd,Kdd,true,true);
          test = -We+Y+val*Yd;
          if (test < 0.0e0)
            dMin = d1;
          else if (test > 0.0e0)
            dMax = d1;
          if ((dMax-dMin) < PRECISION) break;
          //std::cout << iter << "-" << d1 << "," << dMin << "," << dMax << "," << test << std::endl;
          double test1 = std::fabs(test);
          if (test < 0.0e0 && d1 >= 1.0e0)
            break;
          else if (test1 > test0)
            test0 = test1;
          else if (test1 < TOLERANCE*test0)
            break;
          df = dTInv*K+alpha*(2*Kd+val*Kdd);
          //std::cout << df << std::endl;
        }
        // check convergence
        if (iter == maxIt) {
          throw UpdateFailedException("no convergence in internal update");
        }
      
        // save updated value
        intVar1[0] = d1;
      }
    }
    
    // compute damage dissipation
    double phi = damage->dissipatedEnergy(material,extPar,intPar0,intPar1,
                                          ddot,d,Y,Yd,K,Kd,Kdd,stress,tangent);

    // compute tangent matrix
    if (tangent) {
      if (d1 >= 1.0e0)
        M = 0.0e0;
      else {
        M *= fact;
        if (d1-d0 > PRECISION) {
          double coef = 1.e0/(dTInv*K+alpha*(2*Kd+val*Kdd));
          M -= coef*outerProd(sig,sig);
        }
      }
    }
     
    // compute stress tensor
    if (stress) sig *= fact;
     
    return We*fact-intVar0[1]*(1.0e0-intVar0[0])+dTime*phi;
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
