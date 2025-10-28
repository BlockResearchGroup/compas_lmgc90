/*
 *  $Id: EAMInteractionPotential.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_CRYSTAL_EAM_INTERACTION_POTENTIAL_H
#define ZORGLIB_MATL_MECA_CRYSTAL_EAM_INTERACTION_POTENTIAL_H

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
// local
#include <data/Function2.h>
#include <math/TensorAlgebra.h>
#include <matl/ModelDictionary.h>
#include <matl/meca/crystal/AtomisticHEPotential.h>

#include <fstream>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for Embedded Atom Method interaction potential.
 */
template <class ALG>
class EAMInteractionPotential : virtual public AtomicInteractionPotential<ALG> {
  
 public:
  
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 public:

  // constructor
  EAMInteractionPotential() {}

  // copy constructor
  EAMInteractionPotential(const EAMInteractionPotential& src) 
    : AtomicInteractionPotential<ALG>(src) {}
  
  // destructor
  virtual ~EAMInteractionPotential() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***EAM interaction potential***" << std::endl;

    // cutoff radius
    double rc = material.getDoubleProperty("CUTOFF_RADIUS");
    AtomicInteractionPotential<ALG>::checkProperties(material,os);
    
    // associated functions
    Function2 *p0 = dynamic_cast<Function2*>(&(material.getFunctionProperty("EAM_PAIR_POTENTIAL")));
    if (!p0) {
      if (os) (*os) << "ERROR: EAM pair potential function must be of C1 type." << std::endl;
      throw InvalidPropertyException("EAM functions must be of C1 type");
    }
    
    // electron density function
    Function2 *ed = dynamic_cast<Function2*>(&(material.getFunctionProperty("EAM_ELECTRON_DENSITY")));
    if (!ed) {
      if (os) (*os) << "ERROR: EAM electron density function must be of C1 type." << std::endl;
      throw InvalidPropertyException("EAM functions must be of C1 type");
    }
    
    // embedding energy function
    Function2 *p1 = dynamic_cast<Function2*>(&(material.getFunctionProperty("EAM_EMBEDDING_ENERGY")));
    if (!p1) {
      if (os) (*os) << "ERROR: EAM embedding energy function must be of C1 type." << std::endl;
      throw InvalidPropertyException("EAM functions must be of C1 type");
    }
    
    // print-out
    if (os) {
      (*os) << "\tcutoff radius    = " << rc << " [angstrom]" << std::endl;
      (*os) << "\tpair potential   = \"" << p0->getName() << "\"" << std::endl;
      (*os) << "\telectron density = \"" << ed->getName() << "\"" << std::endl;
      (*os) << "\tembedding energy = \"" << p1->getName() << "\"" << std::endl;
    }
  }
  
  // compute interaction energy
  double InteractionEnergy(const MaterialProperties& material,
                           const ParameterSet& extPar,
                           const std::vector<Atom>& atoms,
                           const SYM_TENSOR& C,SYM_TENSOR& S,SYM_TENSOR4& M,
                           bool first,bool second) {
    
    // initialize
    double W = 0.0e0;
    if (first) S = 0.0e0;
    if (second) M = 0.0e0;
    
    // pair potential function
    Function2 *p0 = dynamic_cast<Function2*>(&(material.getFunctionProperty("EAM_PAIR_POTENTIAL")));
    if (!p0) throw InvalidPropertyException("EAM functions must be of C1 type");
    
    // electron density function
    Function2 *ed = dynamic_cast<Function2*>(&(material.getFunctionProperty("EAM_ELECTRON_DENSITY")));
    if (!ed) throw InvalidPropertyException("EAM functions must be of C1 type");
    
    // embedding energy function
    Function2 *p1 = dynamic_cast<Function2*>(&(material.getFunctionProperty("EAM_EMBEDDING_ENERGY")));
    if (!p1) throw InvalidPropertyException("EAM functions must be of C1 type");
    
    // loop on atoms
    double elDensity=0.0e0,Wp=0.0e0;
    SYM_TENSOR Se; Se = 0.0e0;
    SYM_TENSOR4 Me; Me = 0.0e0;
    double rc = this->cutoffRadius();
    double rCut2 = rc*rc;
    for (unsigned int n=1; n < atoms.size(); n++) {
      
      // compute radius in deformed configuration
      Vector3D x0 = atoms[n].x;
      double r2 = x0*(C*x0);
      if (r2 > rCut2) continue;
      double rad = std::sqrt(r2);
      double ri = 1.0e0/rad;
      SYM_TENSOR XX = ri*SYM_TENSOR::outerProd(x0);
      
      // compute pair potential (function is actually r*W)
      double fct,df=0.0e0,d2f=0.0e0;
      if (second)
        fct = p0->value(rad,df,d2f);
      else if (first)
        fct = p0->value(rad,df);
      else
        fct = p0->value(rad);
      double val1 = fct*ri;
      Wp += 0.5*val1;
      if (first || second) {
        double val2 = (df-val1)*ri;
        if (first) S += (0.5*val2)*XX;
        if (second) M += (0.5*(d2f-3.0*val2)*ri)*outerProd(XX,XX);
      }
      
      // compute electron density
      double de=0.0e0,d2e=0.0e0;
      if (second)
        elDensity += ed->value(rad,de,d2e);
      else if (first)
        elDensity += ed->value(rad,de);
      else
        elDensity += ed->value(rad);
      if (first) Se += de*XX;
      if (second) Me += (d2e-de*ri)*outerProd(XX,XX);
    }
    
    // add embedding energy
    double We,dW=0.0e0,d2W=0.0e0;
    if (second)
      We = p1->value(elDensity,dW,d2W);
    else if (first)
      We = p1->value(elDensity,dW);
    else
      We = p1->value(elDensity);
    W = Wp+We;
    if (first) S += dW*Se;
    if (second) M += d2W*outerProd(Se,Se)+dW*Me;
    
    return W;
  }
};


/**
 * Implementations of the model.
 */
class EAMHyperElasticity3D : public HyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  EAMHyperElasticity3D()
  : HyperElasticity<TensorAlgebra3D>(
        new AtomisticHEPotential<TensorAlgebra3D>(
                        *(new EAMInteractionPotential<TensorAlgebra3D>()))) {}
  
  // copy constructor
  EAMHyperElasticity3D(const EAMHyperElasticity3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~EAMHyperElasticity3D() {}
};
class EAMHyperElasticity2D : public HyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  EAMHyperElasticity2D()
  : HyperElasticity<TensorAlgebra2D>(
        new AtomisticHEPotential<TensorAlgebra2D>(
                        *(new EAMInteractionPotential<TensorAlgebra2D>()))) {}
  
  // copy constructor
  EAMHyperElasticity2D(const EAMHyperElasticity2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~EAMHyperElasticity2D() {}
};
class EAMHyperElasticity1D : public HyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  EAMHyperElasticity1D()
  : HyperElasticity<TensorAlgebra1D>(
        new AtomisticHEPotential<TensorAlgebra1D>(
                        *(new EAMInteractionPotential<TensorAlgebra1D>()))) {}
  
  // copy constructor
  EAMHyperElasticity1D(const EAMHyperElasticity1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~EAMHyperElasticity1D() {}
};

/**
 * The associated model builder
 */
class EAMHyperElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  EAMHyperElasticityBuilder();
  
  // the instance
  static EAMHyperElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~EAMHyperElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
