/*
 *  $Id: HyperElastoPlasticity.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_ELASTOPLASTICITY_H
#define ZORGLIB_MATL_MECA_HYPER_ELASTOPLASTICITY_H

// config
#include <matlib_macros.h>

// local
#include "matl/meca/hyper/HyperElasticity.h"
#include "matl/meca/linear/ElastoPlasticity.h"


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for (hyper)elastic-plastic material models.
 */
template <class ALG>
class HyperElastoPlasticity : virtual public HyperElasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor4          TENSOR4;

 protected:

  // constructor
  HyperElastoPlasticity() {}
  
  // copy constructor
  HyperElastoPlasticity(const HyperElastoPlasticity&) {}
  
 public:

  // destructor
  virtual ~HyperElastoPlasticity() {}
  
  // how many internal variables ?
  unsigned int nIntVar() const = 0;
  
  // initialize the state of the material
  void initState(const MaterialProperties& mater,MaterialState& state) {
    ConstitutiveModel::initState(mater,state);
    
    // set the gradient of deformation to identity
    TENSOR F(state.grad);
    F = TENSOR::identity();
    
    // set everything else to zero
    state.flux = 0.0e0;
    state.internal = 0.0e0;
    
    // except the plastic strain
    TENSOR Fp(state.internal);
    Fp = TENSOR::identity();
  }

  // compute the incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& T,
                              bool update,bool tangent)
   throw (UpdateFailedException) {
    
    // update ?
    if (update) state.internal = state0.internal;
    
    // "cast" external variables to tensors
    TENSOR F(state.grad);
    TENSOR P(state.flux);
    TENSOR4 K(T);
    
    // plastic part of F
    const TENSOR Fp0(state0.internal);
    TENSOR Fp(state.internal);
    
    // other internal variables
    unsigned int sz = nIntVar()-TENSOR::MEMSIZE;
    const MatLibArray intVar0(state0.internal,sz,TENSOR::MEMSIZE);
    MatLibArray intVar(state.internal,sz,TENSOR::MEMSIZE);
    
    // right Cauchy-Green tensor
    SYM_TENSOR C;
    ALG::RightCauchyGreen(F,C);
    
    // get the incremental potential
    SYM_TENSOR S;
    SYM_TENSOR4 M;
    double W = plasticUpdate(material,extPar,C,S,Fp0,Fp,intVar0,intVar,
                             dTime,M,update,tangent);
    
    // compute Piola tensor and Lagrangian tangents
    if (update) ALG::PK2ToPK1(S,F,P);
    if (tangent) ALG::MaterialToLagrangian(M,S,F,K);
    
    return W;
  }

 protected:

  // compute the plastic update
  virtual double plasticUpdate(const MaterialProperties&,
                               const ParameterSet& extPar,
                               const SYM_TENSOR&,SYM_TENSOR&,
                               const TENSOR&,TENSOR&,
                               const MatLibArray&,MatLibArray&,double,
                               SYM_TENSOR4&,bool,bool)
    throw (UpdateFailedException) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
