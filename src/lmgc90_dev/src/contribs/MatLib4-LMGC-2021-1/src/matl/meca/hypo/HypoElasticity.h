/*
 *  $Id: HypoElasticity.h 158 2015-01-10 21:14:05Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPO_ELASTICITY_H
#define ZORGLIB_MATL_MECA_HYPO_ELASTICITY_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
// local
#include <matl/ConstitutiveModel.h>
#include <matl/meca/linear/Elasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for hypoelasticity models.
 */
template <class ALG>
class HypoElasticity : virtual public ConstitutiveModel {

 public:
  
public:
  
  // define new types
  typedef typename ALG::ARRAY  ARRAY;
  typedef typename ALG::MATRIX MATRIX;

  
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor4          TENSOR4;
  //typedef typename ALG::SymTensor  SYM_TENSOR;
  //typedef typename ALG::SymTensor4 SYM_TENSOR4;

 protected:
    
  // associated potential
  typename Elasticity<ALG>::Potential *potential;

  // instance counter
  unsigned int *count;
  
 public:
    
  // constructor
  HypoElasticity(typename Elasticity<ALG>::Potential& p) {
    count = new unsigned int(1);
    potential = &p;
  }
  
  // copy constructor
  HypoElasticity(const HypoElasticity& src) {
    count = src.count;
    (*count)++;
    potential = src.potential;
  }
  
  // destructor
  virtual ~HypoElasticity() {
    if (--(*count) > 0) return;
    delete count;
    delete potential;
  }

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
     if (os) (*os) << "\nHypoelastic material:" << std::endl;
     
     // density
     try {
       double rho = material.getDoubleProperty("MASS_DENSITY");
       if (os) (*os) << "\n\tmass density = " << rho << std::endl;
     }
     catch (NoSuchPropertyException) {
       if (os) (*os) << "\n\tmass density is not defined" << std::endl;
     }

     // potential
     potential->checkProperties(material,os);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    potential->rotateProperties(material,R);
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    potential->updateProperties(mater,extPar);
  }
  
  // how many external variables ?
  unsigned int nExtVar() const {return TENSOR::MEMSIZE;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 1;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_TENSOR;
        break;
      default:
        return ConstitutiveModel::TYPE_NONE;
        break;
    }
  }
  unsigned int indexExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return 0;
        break;
      default:
        return TENSOR::MEMSIZE;
        break;
    }
  }
  std::string labelExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "deformation";
        break;
      default:
        return "";
        break;
    }
  }
  std::string labelExtForce(unsigned int i) const {
    switch (i) {
      case 0:
        return "stress";
        break;
      default:
        return "";
        break;
    }
  }

  // how many internal variables ?
  unsigned int nIntVar() const {return 1;}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 1;}
  unsigned int getIntVar(const std::string& str) const {
    if (str == "ENRG")
      return 0;
    else
      return 1;
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int i) const {
    switch (i) {
      case 0:
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
      default:
        return 1;
        break;
    }
  }
  std::string labelIntVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "elastically stored energy";
        break;
      default:
        return "";
        break;
    }
  }

  // initialize the state of the material
  void initState(const MaterialProperties& material,MaterialState& state) {
    ConstitutiveModel::initState(material,state);
    state.grad = TENSOR::identity();
    state.flux = 0.e0;
    state.internal = 0.e0;
  }

  // update the state of the material
  void updateState(const MaterialProperties& material,
                   const ParameterSet& extPar,
                   const MaterialState& state0,MaterialState& state1,
                   double dTime,MatLibMatrix& T,bool tangent) 
   throw (UpdateFailedException) {
    
    // incremental gradient of deformation
    double J0;
    TENSOR F0(state0.grad),F(state1.grad),dF;
    TENSOR F0inv = F0.inverse(J0);
    dF = F*F0inv;

    // RU decomposition
    TENSOR R;
    SYM_TENSOR U;
    ALG::RUDecomposition(dF,R,U);
    
    // logarithmic strain increment
    SYM_TENSOR dEps = log(U);

    // get initial Cauchy stress
    SYM_TENSOR sig0;
    ALG::PK1ToCauchy(state0.flux,F0,sig0);
    
    // compute corotational stress increment 
    SYM_TENSOR sigC;
    SYM_TENSOR4 H;
    double dW = storedEnergy(material,extPar,dEps,sig0,sigC,
                             H,true,tangent);
    state1.internal[0] = state0.internal[0]+dW;

    // rotate stress
    SYM_TENSOR sig;
    ALG::PK2ToKirchhoff(sigC,R,sig); // sig = R*sig_c*R^T
    
    // rotate tangents
    TENSOR4 M;
    if (tangent) {/* DO SOMETHING */}

    // compute Piola tensor and Lagrangian tangents
    ALG::CauchyToPK1(sig,F,state1.flux);
    if (tangent) ALG::SpatialToLagrangian(M,sig,F,T);
  }

  // compute material tangents (without updating)
  void computeTangent(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const MaterialState& state0,
                      const MaterialState& state1,
                      double dTime,MatLibMatrix& T) {
    
    // incremental gradient of deformation
    double J0;
    TENSOR F0(state0.grad),F(state1.grad),dF;
    TENSOR F0inv = F0.inverse(J0);
    dF = F*F0inv;
    
    // RU decomposition
    TENSOR R;
    SYM_TENSOR U;
    ALG::RUDecomposition(dF,R,U);
    
    // logarithmic strain increment
    SYM_TENSOR dEps = log(U);
    
    // get initial Cauchy stress
    SYM_TENSOR sig0;
    ALG::PK1ToCauchy(state0.flux,F0,sig0);
    
    // compute corotational tangents 
    SYM_TENSOR sigC;
    SYM_TENSOR4 H;
    storedEnergy(material,extPar,dEps,sig0,sigC,H,false,true);
    
    // rotate tangents
    TENSOR4 M;
    
    // get Cauchy stress
    SYM_TENSOR sig;
    ALG::PK1ToCauchy(state1.flux,F,sig);
    
    // compute Lagrangian tangents
    ALG::SpatialToLagrangian(M,sig,F,T);
  }

 protected:

  // compute increment in stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,const SYM_TENSOR& dEps,
                      const SYM_TENSOR& sig0,SYM_TENSOR& sig1,
                      SYM_TENSOR4& M,bool first,bool second) {
    
    // stress increment
    SYM_TENSOR dGam,dSig;
    dGam = covariant(dEps);
    double dW = innerProd(sig0,dGam);
    dW += potential->storedEnergy(material,extPar,dGam,dSig,M,first,second);
    
    // final corotational stress
    if (first) sig1 = sig0+dSig;
    
    return dW;
  }
};  

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
