/*
 *  $Id: CompositeElasticity.h 140 2013-08-30 15:38:09Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_COMPOSITE_ELASTICITY_H
#define ZORGLIB_MATL_MECA_LINEAR_COMPOSITE_ELASTICITY_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
// STL
#include <vector>
// local
#include <matl/meca/linear/Elasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for linear composite (multi-material) elasticity models.
 */
template <class ALG>
class CompositeElasticity : virtual public StandardMaterial {
  
 public:
  
  // define new types
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;

 protected:
  
  // elasticity models for individual phases
  std::vector<Elasticity<ALG>*> phases;
  
  // instance counter
  unsigned int *count;
  
  // empty constructor
  CompositeElasticity() {
    count = new unsigned int(1);
    phases.clear();
  }

 public:

  // constructor
  CompositeElasticity(std::vector<Elasticity<ALG>*>& p) {
    count = new unsigned int(1);
    phases = p;
  }

  // copy constructor
  CompositeElasticity(const CompositeElasticity& src) {
    count = src.count;
    (*count)++;
    phases = src.phases;
  }
  
  // destructor
  virtual ~CompositeElasticity() {
    if (--(*count) > 0) return;
    delete count;
    for (unsigned int i=0; i < phases.size(); i++)
      if (phases[i]) delete phases[i];
    phases.clear();
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nLinear (small-strains) composite material:" << std::endl;
    checkPhases(material,os);
  }

  // check individual phases
  void checkPhases(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {
    // loop on elasticity models
    MaterialProperties phMP;
    for (unsigned int i=0; i < phases.size(); i++) {
      if (os) (*os) << "\nElastic variant #" << i+1 << ":" << std::endl;
      if (os) (*os) << "---------------------------------" << std::endl;
      // extract relevant material properties
      phMP.clear();
      MaterialProperties::pullProperties(i,material,phMP);
      // check phase properties
      phases[i]->checkProperties(phMP,os);
      // push back properties
      MaterialProperties::pushProperties(i,phMP,material);
    }
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    rotatePhases(material,R);
  }

  // rotate individual phases
  void rotatePhases(MaterialProperties& material,const Rotation& R) {
    // loop on elasticity models
    MaterialProperties phMP;
    for (unsigned int i=0; i < phases.size(); i++) {
      // extract relevant material properties
      phMP.clear();
      MaterialProperties::pullProperties(i,material,phMP);
      // rotate properties
      phases[i]->rotateProperties(phMP,R);
      // push back properties
      MaterialProperties::pushProperties(i,phMP,material);
    }
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& material,const ParameterSet& extPar) {
    updatePhases(material,extPar);
  }

  // update properties of individual phases
  void updatePhases(MaterialProperties& material,const ParameterSet& extPar) {
    // loop on elasticity models
    MaterialProperties phMP;
    for (unsigned int i=0; i < phases.size(); i++) {
      // extract relevant material properties
      phMP.clear();
      MaterialProperties::pullProperties(i,material,phMP);
      // update properties
      phases[i]->updateProperties(phMP,extPar);
      // push back properties
      MaterialProperties::pushProperties(i,phMP,material);
    }
  }
  
  // how many external variables ?
  unsigned int nExtVar() const {return SYM_TENSOR::MEMSIZE;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 1;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_SYM_TENSOR;
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
        return SYM_TENSOR::MEMSIZE;
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
  unsigned int nIntVar() const {
    unsigned int n = 1;
    for (unsigned int i=0; i < phases.size(); i++)
      n += phases[i]->nIntVar();
    return n;
  }
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {
    unsigned int n = 1;
    for (unsigned int i=0; i < phases.size(); i++)
      n += phases[i]->nIntVarBundled();
    return n;
  }
  unsigned int getIntVar(const std::string& str) const {
    unsigned int nPhases = phases.size();
    if (str == "ENRG")
      return 0;
    else {
      size_t p = str.find_last_not_of("0123456789");
      std::string str1(str,0,p);
      unsigned int idx = std::atoi(str.c_str()+p+1)-1;
      unsigned int n = 1;
      for (unsigned i=0; i < idx; i++)
        n += phases[i]->nIntVarBundled();
      return n+phases[idx]->getIntVar(str1);
    }
  }
  ConstitutiveModel::VariableType typeIntVar(unsigned int idx) const {
    if (idx == 0)
      return ConstitutiveModel::TYPE_SCALAR;
    else {
      unsigned int n=1;
      for (unsigned int i=0; i < phases.size(); i++) {
        unsigned int n1 = phases[i]->nIntVarBundled();
        if (idx-n < n1) return phases[i]->typeIntVar(idx-n);
        n += n1;
      }
    }  
    return ConstitutiveModel::TYPE_NONE;
  }
  unsigned int indexIntVar(unsigned int idx) const {
    if (idx == 0)
      return 0;
    else {
      unsigned int n=1,m=1;
      for (unsigned int i=0; i < phases.size(); i++) {
        unsigned int n1 = phases[i]->nIntVarBundled();
        if (idx-n < n1) return m+phases[i]->indexIntVar(idx-n);
        n += n1;
        m += phases[i]->nIntVar();
      }
      return m;
    }  
  }
  std::string labelIntVar(unsigned int idx) const {
    if (idx == 0)
      return "elastically stored energy";
    else {
      unsigned int n=1;
      for (unsigned int i=0; i < phases.size(); i++) {
        unsigned int n1 = phases[i]->nIntVarBundled();
        if (idx-n < n1) return phases[i]->labelIntVar(idx-n);
        n += n1;
      }
    }  
    return "";
  }
  
  // check if the material behaviour is linear ?
  bool isLinear() const {
    for (unsigned int i=0; i < phases.size(); i++)
      if (!phases[i]->isLinear()) return false;
    return true;
  }
  
  // initialize the state of the material
  void initState(const MaterialProperties& material,MaterialState& state) {
    ConstitutiveModel::initState(material,state);
    state.grad = 0.e0;
    state.flux = 0.e0;
    state.internal = 0.0;

    // phase initialization
    MaterialProperties phMP;
    MaterialState phMS;
    unsigned int idx=1;
    for (unsigned int i=0; i < phases.size(); i++) {
      // extract relevant material properties
      phMP.clear();
      MaterialProperties::pullProperties(i,material,phMP);
      // initialize state of phase i
      phases[i]->initState(phMP,phMS);
      MatLibArray intV(state.internal,idx,phases[i]->nIntVar());
      intV = phMS.internal;
      idx += phases[i]->nIntVar();
    }
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

  
#endif
