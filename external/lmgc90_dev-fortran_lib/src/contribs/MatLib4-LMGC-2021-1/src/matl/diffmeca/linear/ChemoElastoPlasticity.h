/*
 *  $Id: ChemoElastoPlasticity.h 172 2015-08-24 14:44:39Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFFMECA_LINEAR_CHEMO_ELASTOPLASTICITY_H
#define ZORGLIB_MATL_DIFFMECA_LINEAR_CHEMO_ELASTOPLASTICITY_H

// config
#include <matlib_macros.h>

// local
#include <matl/diffmeca/linear/ChemoElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for (geometrically linear) chemo-elastic-plastic material models.
 */
template <class ALG>
class ChemoElastoPlasticity : virtual public ChemoElasticity<ALG> {

 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:

  // constructor
  ChemoElastoPlasticity() {}

  // copy constructor
  ChemoElastoPlasticity(const ChemoElastoPlasticity&) {}

 public:

  // destructor
  virtual ~ChemoElastoPlasticity() {}
  
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
 
    // get chemical potential
    double mu0 = state0.grad[SYM_TENSOR::MEMSIZE];
    double mu1 = state.grad[SYM_TENSOR::MEMSIZE];

    // extract plastic strains
    const SYM_TENSOR epsP0(state0.internal);
    SYM_TENSOR epsP(state.internal);

    // get incremental potential
    double dC,C;
    SYM_TENSOR dSig;
    unsigned int sz = this->nIntVar()-SYM_TENSOR::MEMSIZE;
    const MatLibArray intVar0(state0.internal,sz,SYM_TENSOR::MEMSIZE);
    MatLibArray intVar(state.internal,sz,SYM_TENSOR::MEMSIZE);
    double W = chemoPlasticUpdate(material,extPar,eps,sig,mu0,mu1,dC,
                                  epsP0,epsP,intVar0,intVar,dTime,K,dSig,C,
                                  update,tangent);
    
    // update
    if (update) {
      state.flux[SYM_TENSOR::MEMSIZE] = dC;
    }
    if (tangent) {
      M[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE] = C;
      for (unsigned int i=0; i < SYM_TENSOR::MEMSIZE; i++)
        M[i][SYM_TENSOR::MEMSIZE] = M[SYM_TENSOR::MEMSIZE][i] = dSig[i];
    }
    
    return W;
  }

 protected:

  // solve constitutive update
  virtual double chemoPlasticUpdate(const MaterialProperties&,const ParameterSet&,
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
