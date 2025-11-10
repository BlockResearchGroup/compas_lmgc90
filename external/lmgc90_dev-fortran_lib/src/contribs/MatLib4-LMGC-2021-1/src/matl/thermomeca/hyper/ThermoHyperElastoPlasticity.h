/*
 *  $Id: ThermoHyperElastoPlasticity.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_HYPER_ELASTOPLASTICITY_H
#define ZORGLIB_MATL_MECA_THERMO_HYPER_ELASTOPLASTICITY_H

// config
#include <matlib_macros.h>

// local
#include <matl/thermomeca/hyper/ThermoHyperElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for thermo-elastic-plastic material models.
 */
template <class ALG>
class ThermoHyperElastoPlasticity : virtual public ThermoHyperElasticity<ALG> {

 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor::TYPE     TENSOR;
  typedef typename ALG::Tensor4          TENSOR4;

 protected:

  // constructor
  ThermoHyperElastoPlasticity() {}

  // copy constructor
  ThermoHyperElastoPlasticity(const ThermoHyperElastoPlasticity&) {}

 public:

  // destructor
  virtual ~ThermoHyperElastoPlasticity() {}
  
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
     
    // get tensors
    TENSOR F(state.grad);
    TENSOR P(state.flux);
    TENSOR4 K(T);

    // temperature
    double T0 = state0.grad[TENSOR::MEMSIZE];
    double T1 = state.grad[TENSOR::MEMSIZE];
    
    // plastic part of F
    const TENSOR Fp0(state0.internal);
    TENSOR Fp(state.internal);
    
    // right Cauchy-Green tensor
    SYM_TENSOR C;
    ALG::RightCauchyGreen(F,C);
    
    // other internal variables
    unsigned int sz = this->nIntVar()-TENSOR::MEMSIZE;
    const MatLibArray intVar0(state0.internal,sz,TENSOR::MEMSIZE);
    MatLibArray intVar(state.internal,sz,TENSOR::MEMSIZE);

    // get incremental potential
    double dN,Cm;
    SYM_TENSOR S,dS;
    SYM_TENSOR4 M;
    double W = plasticUpdate(material,extPar,C,S,T0,T1,dN,
                             Fp0,Fp,intVar0,intVar,dTime,M,dS,Cm,
                             update || tangent,tangent);
    
    // update (compute Piola tensor and Lagrangian tangents)
    if (update) {
      ALG::PK2ToPK1(S,F,P);
      state.flux[TENSOR::MEMSIZE] = dN;
    }
    if (tangent) {
      ALG::MaterialToLagrangian(M,S,F,K);
      T[TENSOR::MEMSIZE][TENSOR::MEMSIZE] = Cm;
      TENSOR dP;
      ALG::PK2ToPK1(dS,F,dP);
      for (unsigned int i=0; i < TENSOR::MEMSIZE; i++)
        T[i][TENSOR::MEMSIZE] = T[TENSOR::MEMSIZE][i] = dP[i];
    }
    
    return W;
  }

 protected:

  // compute the plastic update
  virtual double plasticUpdate(const MaterialProperties&,const ParameterSet&,
                               const SYM_TENSOR&,SYM_TENSOR&,
                               double,double,double&,const TENSOR&,TENSOR&,
                               const MatLibArray&,MatLibArray&,double,
                               SYM_TENSOR4&,SYM_TENSOR&,double&,bool,bool) 
    throw (UpdateFailedException) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
