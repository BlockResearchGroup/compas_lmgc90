/*
 *  $Id: ThermoElastoPlasticity.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_LINEAR_ELASTOPLASTICITY_H
#define ZORGLIB_MATL_MECA_THERMO_LINEAR_ELASTOPLASTICITY_H

// config
#include <matlib_macros.h>

// local
#include <matl/thermomeca/linear/ThermoElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for (geometrically linear) thermo-elastic-plastic material models.
 */
template <class ALG>
class ThermoElastoPlasticity : virtual public ThermoElasticity<ALG> {

 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:

  // constructor
  ThermoElastoPlasticity() {}

  // copy constructor
  ThermoElastoPlasticity(const ThermoElastoPlasticity&) {}

 public:

  // destructor
  virtual ~ThermoElastoPlasticity() {}
  
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
    
    // convert arrays
    SYM_TENSOR eps(state.grad);
    SYM_TENSOR sig(state.flux);
    SYM_TENSOR4 K(M);
 
    // get temperature
    double Th0 = state0.grad[SYM_TENSOR::MEMSIZE];
    double Th1 = state.grad[SYM_TENSOR::MEMSIZE];

    // extract plastic strains
    const SYM_TENSOR epsP0(state0.internal);
    SYM_TENSOR epsP(state.internal);

    // get incremental potential
    double dN,C;
    SYM_TENSOR dSig;
    unsigned int sz = this->nIntVar()-SYM_TENSOR::MEMSIZE;
    const MatLibArray intVar0(state0.internal,sz,SYM_TENSOR::MEMSIZE);
    MatLibArray intVar(state.internal,sz,SYM_TENSOR::MEMSIZE);
    double W = plasticUpdate(material,extPar,eps,sig,Th0,Th1,dN,
                             epsP0,epsP,intVar0,intVar,dTime,K,dSig,C,
                             update,tangent);
    
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

  // compute the plastic update
  virtual double plasticUpdate(const MaterialProperties&,const ParameterSet&,
                               const SYM_TENSOR&,SYM_TENSOR&,
                               double,double,double&,
                               const SYM_TENSOR&,SYM_TENSOR&,
                               const MatLibArray&,MatLibArray&,double,
                               SYM_TENSOR4&,SYM_TENSOR&,double&,bool,bool) 
    throw (UpdateFailedException) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
