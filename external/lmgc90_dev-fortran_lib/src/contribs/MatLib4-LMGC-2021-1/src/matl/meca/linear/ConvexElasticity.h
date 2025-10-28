/*
 *  $Id: ConvexElasticity.h 140 2013-08-30 15:38:09Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_CONVEX_ELASTICITY_H
#define ZORGLIB_MATL_MECA_LINEAR_CONVEX_ELASTICITY_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
// STL
#include <vector>
// local
#include <matl/meca/linear/CompositeElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for linear convexified multi-well elasticity models.
 */
template <class ALG>
class ConvexElasticity : virtual public CompositeElasticity<ALG> {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

  // default parameter value
  static const double DEFAULT_ENTROPY_MIXING_PARAMETER = 1.e3;

 protected:
  
  // empty constructor
  ConvexElasticity() {}

 public:

  // constructor
  ConvexElasticity(std::vector<Elasticity<ALG>*>& w)
  : CompositeElasticity<ALG>(w) {}

  // copy constructor
  ConvexElasticity(const ConvexElasticity& src) 
  : CompositeElasticity<ALG>(src) {}
  
  // destructor
  virtual ~ConvexElasticity() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nLinear convexified multi-well elastic material:" << std::endl;

    // check phases
    this->checkPhases(material,os);

    // algorithmic parameter
    double beta;
    try {
      beta = material.getDoubleProperty("ENTROPY_MIXING_PARAMETER");
    }
    catch (NoSuchPropertyException) {
      beta = DEFAULT_ENTROPY_MIXING_PARAMETER;
      material.setProperty("ENTROPY_MIXING_PARAMETER",beta);
    }
    if (os) (*os) << "\n\tentropy mixing parameter = " << beta << std::endl;
  }
  
  // how many internal variables ?
  unsigned int nIntVar() const {
    unsigned int n = 1;
    for (unsigned int i=0; i < this->phases.size(); i++)
      n += this->phases[i]->nIntVar()+1;
    return n;
  }
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {
    unsigned int n = 1;
    for (unsigned int i=0; i < this->phases.size(); i++)
      n += this->phases[i]->nIntVarBundled()+1;
    return n;
  }
  unsigned int getIntVar(const std::string& str) const {
    unsigned int nPhases = this->phases.size();
    if (nPhases > 0 && str == "VFRC1")
      return 0;
    if (nPhases > 1 && str == "VFRC2")
      return 1;
    if (nPhases > 2 && str == "VFRC3")
      return 2;
    if (nPhases > 3 && str == "VFRC4")
      return 3;
    if (nPhases > 4 && str == "VFRC5")
      return 4;
    if (nPhases > 5 && str == "VFRC6")
      return 5;
    if (nPhases > 6 && str == "VFRC7")
      return 6;
    if (nPhases > 7 && str == "VFRC8")
      return 7;
    if (nPhases > 8 && str == "VFRC9")
      return 8;
    if (nPhases > 9 && str == "VFRC10")
      return 9;
    else if (str == "ENRG")
      return nPhases;
    else {
      size_t p = str.find_last_not_of("0123456789");
      std::string str1(str,0,p);
      unsigned int idx = std::atoi(str.c_str()+p+1)-1;
      unsigned int n = nPhases+1;
      for (unsigned i=0; i < idx; i++)
        n += this->phases[i]->nIntVarBundled();
      return n+this->phases[idx]->getIntVar(str1);
    }
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int idx) const {
    unsigned int nPhases = this->phases.size();
    if (idx <= nPhases)
      return ConstitutiveModel::TYPE_SCALAR;
    else {
      unsigned int n=nPhases+1;
      for (unsigned int i=0; i < nPhases; i++) {
        unsigned int n1 = this->phases[i]->nIntVarBundled();
        if (idx-n < n1) return this->phases[i]->typeIntVar(idx-n);
        n += n1;
      }
    }  
    return ConstitutiveModel::TYPE_NONE;
  }
  unsigned int indexIntVar(unsigned int idx) const {
    unsigned int nPhases = this->phases.size();
    if (idx <= nPhases)
      return idx;
    else {
      unsigned int n=nPhases+1,m=nPhases+1;
      for (unsigned int i=0; i < nPhases; i++) {
        unsigned int n1 = this->phases[i]->nIntVarBundled();
        if (idx-n < n1) return m+this->phases[i]->indexIntVar(idx-n);
        n += n1;
        m += this->phases[i]->nIntVar();
      }
      return m;
    }  
  }
  std::string labelIntVar(unsigned int idx) const {
    unsigned int nPhases = this->phases.size();
    if (idx < nPhases) {
      char str[64];
      std::sprintf(str,"volume fraction variant %u",idx+1);
      return str;
    }
    else if (idx == nPhases)
      return "elastically stored energy";
    else {
      unsigned int n=nPhases+1;
      for (unsigned int i=0; i < this->phases.size(); i++) {
        unsigned int n1 = this->phases[i]->nIntVarBundled();
        if (idx-n < n1) return this->phases[i]->labelIntVar(idx-n);
        n += n1;
      }
    }  
    return "";
  }
  
  // check if the material behaviour is linear ?
  bool isLinear() const {return false;}
  
  // initialize the state of the material
  void initState(const MaterialProperties& material,MaterialState& state) {
    ConstitutiveModel::initState(material,state);
    state.grad = 0.e0;
    state.flux = 0.e0;
    state.internal = 0.0;
    
    // phase initialization
    MaterialProperties phMP;
    MaterialState phMS;
    unsigned int idx=this->phases.size()+1;
    for (unsigned int i=0; i < this->phases.size(); i++) {
      // extract relevant material properties
      phMP.clear();
      MaterialProperties::pullProperties(i,material,phMP);
      // initialize state of phase i
      this->phases[i]->initState(phMP,phMS);
      MatLibArray intV(state.internal,this->phases[i]->nIntVar(),idx);
      intV = phMS.internal;
      idx += this->phases[i]->nIntVar();
    }
  }
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties& material,
                              const ParameterSet& extPar,
                              const MaterialState& state0,MaterialState& state,
                              double dTime,MatLibMatrix& M,
                              bool update,bool tangent) 
   throw (UpdateFailedException) {
     
    // get tensors
    SYM_TENSOR eps(state.grad);
    SYM_TENSOR sig(state.flux);
    SYM_TENSOR4 K(M);
    
    // get regularization parameter
    double beta = material.getDoubleProperty("ENTROPY_MIXING_PARAMETER");

    // set work variables
    unsigned int sz = this->phases.size();
    double l[sz],w[sz],wMax;
    SYM_TENSOR e[sz];
    SYM_TENSOR4 T[sz];
    MaterialProperties m[sz];
     
    // initialization
    unsigned int i;
    for (i=0; i < sz; i++) {
      m[i].clear();
      MaterialProperties::pullProperties(i,material,m[i]);
      e[i] = eps;
      w[i] = this->phases[i]->dualEnergy(m[i],extPar,sig,e[i],T[i],update,update || tangent);
      if (i == 0)
        wMax = w[0];
      else
        wMax = (w[i] > wMax) ? w[i]:wMax;
    }
    double Z=0.0;
    for (i=0; i < sz; i++) {
      l[i] = std::exp(beta*(w[i]-wMax));
      Z += l[i];
    }
    double Zinv = 1.0/Z;
    for (i=0; i < sz; i++) l[i] *= Zinv;

    // find sigma corresponding to convex envelope
    if (update) {
      static const unsigned int ITMAX = 20;
      static const double PRECISION = 1.e-12;
      static const double TOLERANCE = 1.e-08;
      unsigned int iter = 0;
      double norm0;
      while (iter < ITMAX) {
        
        // evaluate residual
        SYM_TENSOR R = eps;
        for (i=0; i < sz; i++) R -= l[i]*e[i];
        double norm1 = normL2(R);
        //std::cout << "iter=" << iter << "-R=" << R << "-norm=" << norm1 << "(norm0=" << norm0 << ")" << std::endl;
        if (norm1 < PRECISION) break;
        if (iter > 0) {
          if (norm1 < TOLERANCE*norm0) break;
        }
        else
          norm0 = norm1;
        
        // solve
        SYM_TENSOR4 TT;
        for (i=0,TT=0.0e0; i < sz; i++)
          TT += l[i]*(T[i]+beta*outerProd(e[i],e[i]+R-eps));
        SYM_TENSOR dSig;
        TT.solve(dSig,R,true);
        sig += dSig;
        
        // update values
        for (i=0; i < sz; i++) {
          w[i] = this->phases[i]->dualEnergy(m[i],extPar,sig,e[i],T[i],true,true);
          if (i == 0)
            wMax = w[0];
          else
            wMax = (w[i] > wMax) ? w[i]:wMax;
        }
        for (i=0,Z=0.0e0; i < sz; i++) {
          l[i] = std::exp(beta*(w[i]-wMax));
          //std::cout << "\ti=" << i+1 << ",l=" << l[i] << std::endl;
          Z += l[i];
        }
        //std::cout << "\tZ=" << Z << std::endl;
        Zinv = 1.0/Z;
        for (i=0; i < sz; i++) {
          l[i] *= Zinv;
          //std::cout << "\ti=" << i+1 << ",W*=" << w[i] << ",eps=" << e[i] << ",l=" << l[i] << std::endl;
        }
        
        iter++;
      }
      if (iter == ITMAX) throw UpdateFailedException("convexification");
    }
     
    // compute energy envelope
    double W = innerProd(eps,sig)-std::log(Z)/beta-wMax;
    if (update) state.internal[sz] = W;

    return  W-state0.internal[sz];
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

  
#endif
