/*
 *  $Id: AtomicInteractionPotential.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_CRYSTAL_ATOMIC_INTERACTION_POTENTIAL_H
#define ZORGLIB_MATL_MECA_CRYSTAL_ATOMIC_INTERACTION_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <matl/ConstitutiveModel.h>
#include <matl/meca/crystal/BravaisLattice.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for atomic interaction potentials.
 */
template <class ALG>
class AtomicInteractionPotential {
  
 public:
  
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:
  
  // cutoff radius
  double rCut;

  // constructor
  AtomicInteractionPotential() {}
  
  // copy constructor
  AtomicInteractionPotential(const AtomicInteractionPotential& src) {
    rCut = src.rCut;
  }
  
 public:

  // destructor
  virtual ~AtomicInteractionPotential() {}

  // get cutoff-radius
  double cutoffRadius() const {return rCut;}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    // cutoff radius
    rCut = material.getDoubleProperty("CUTOFF_RADIUS");
  }
  
  // apply rotation to material properties
  virtual void rotateProperties(MaterialProperties&,const Rotation&) {}
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // compute interaction energy
  virtual double InteractionEnergy(const MaterialProperties&,const ParameterSet&,
                                   const std::vector<Atom>&,
                                   const SYM_TENSOR&,SYM_TENSOR&,SYM_TENSOR4&,
                                   bool,bool)  = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
