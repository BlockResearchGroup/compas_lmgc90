/*
 *  $Id: AtomisticHEPotential.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_CRYSTAL_ATOMISTIC_HYPERELASTIC_POTENTIAL_H
#define ZORGLIB_MATL_MECA_CRYSTAL_ATOMISTIC_HYPERELASTIC_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/crystal/AtomicInteractionPotential.h>
#include <matl/meca/crystal/BravaisLattice.h>
#include <matl/meca/hyper/HyperElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing hyperelastic potentials derived from atomistics,
 * using Cauchy-Born rule and some interaction potential.
 */
template <class ALG>
class AtomisticHEPotential : virtual public HyperElasticity<ALG>::Potential {
  
 public:
  
  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  typedef typename ALG::Tensor::TYPE     TENSOR;

 protected:

  // counter
  unsigned int *count;

  // associated Bravais lattice
  BravaisLattice *lattice;
  
  // associated interaction potential
  AtomicInteractionPotential<ALG> *potential;

  // sample crystal atoms
  std::vector<Atom> atoms;

 public:

  // constructor
  AtomisticHEPotential(AtomicInteractionPotential<ALG>& p) {
    count = new unsigned int(1);
    potential = &p;
    lattice = 0;
  }

  // copy constructor
  AtomisticHEPotential(const AtomisticHEPotential& src) {
    count = src.count;
    ++(*count);
    potential = src.potential;
    atoms = src.atoms;
    lattice = new BravaisLattice(*(src.lattice));
  }

  // destructor
  virtual ~AtomisticHEPotential() {
    if (lattice) delete lattice;
    if (--(*count) != 0) return;
    delete count;
    if (potential) delete potential;
  }

  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***Atomistic hyperelastic potential***" << std::endl;
     
    // interaction potential
    potential->checkProperties(material,os);
    
    // create lattice
    double bLattice;
    if (lattice) delete lattice;
    try {
      bLattice = material.getDoubleProperty("FCC_LATTICE_PARAMETER");
      if (bLattice <= 0.0e0) {
        if (os) (*os) << "ERROR: lattice parameter must be strictly positive." << std::endl;
        throw InvalidPropertyException("lattice parameter");
      }
      lattice = new FCCBravaisLattice(bLattice);
      if (os) {
        (*os) << "\n\t***FCC lattice***" << std::endl;
        (*os) << "\tlattice parameter = " << bLattice << " [angstrom]" << std::endl;
      }
    }
    catch (NoSuchPropertyException) {
      try {
        bLattice = material.getDoubleProperty("BCC_LATTICE_PARAMETER");
        if (bLattice <= 0.0e0) {
          if (os) (*os) << "ERROR: lattice parameter must be strictly positive." << std::endl;
          throw InvalidPropertyException("lattice parameter");
        }
        lattice = new BCCBravaisLattice(bLattice);
        if (os) {
          (*os) << "\n\t***BCC lattice***" << std::endl;
          (*os) << "\tlattice parameter = " << bLattice << " [angstrom]" << std::endl;
        }
      }
      catch (NoSuchPropertyException e) {
        lattice = 0;
        if (os) (*os) << "ERROR: only BCC and FCC lattices are implemented." << std::endl;
        throw e;
      }
    }

    // build sample crystal
    double rCut = potential->cutoffRadius();
    double rMax;
    try {
      rMax = material.getDoubleProperty("CRYSTALLITE_RADIUS");
    }
    catch (NoSuchPropertyException) {
      rMax = 3.0*rCut;
      material.setProperty("CRYSTALLITE_RADIUS",rMax);
    }
    lattice->genCrystal(rMax,atoms);
    
    if (os) {
      (*os) << "\tsample crystallite of radius " << rMax;
      (*os) << " (" << atoms.size() << " atoms)" << std::endl;
    }
    
    // select atoms within cutoff radius
    std::vector<Atom> atoms1;
    atoms1.push_back(atoms[0]);
    for (unsigned int n=1; n < atoms.size(); n++) {
      if (atoms[n].r > rCut) continue;
      atoms1.push_back(atoms[n]);
    }
    
    // compute reference energy (in undeformed configuration)
    ParameterSet extPar;
    SYM_TENSOR C,S;
    SYM_TENSOR4 M;
    C = SYM_TENSOR::identity();
    double W0 = potential->InteractionEnergy(material,extPar,atoms1,
                                             C,S,M,false,false);
    material.setProperty("REFERENCE_ENERGY",W0);
    if (os) {
      (*os) << "\n\treference energy density = " << W0 << " [eV/A^3]" << std::endl;
    }
    
    // local units in terms of global units
    double ao,eV;
    try {
      ao = material.getDoubleProperty("UNIT_ANGSTROM");
    }
    catch (NoSuchPropertyException) {
      ao = 1.0e-10; /* SI: meter */
      material.setProperty("UNIT_ANGSTROM",ao);
    }    
    try {
      eV = material.getDoubleProperty("UNIT_ELECTRON_VOLT");
    }
    catch (NoSuchPropertyException) {
      eV = 1.60217646e-19; /* SI: Joule */
      material.setProperty("UNIT_ELECTRON_VOLT",eV);
    }    
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    
    // interaction potential
    potential->rotateProperties(material,R);
    
    // rotate atomic positions of crystallite
    TENSOR R0;
    R.toTensor(R0);
    for (unsigned int n=1; n < atoms.size(); n++) {
      atoms[n].x = R0*atoms[n].x;
    }
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& mater,const ParameterSet& extPar) {
    // interaction potential
    potential->updateProperties(mater,extPar);
  }
  
  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& C,SYM_TENSOR& S,
                      SYM_TENSOR4& M,bool first,bool second) {

    // compute min stretch
    double l[3],lMin;
    C.eigenValues(l);
    lMin = l[0];
    for (unsigned int i=1; i < 3; i++) if (l[i] < lMin) lMin = l[i];

    // update cutoff radius
    double rCut0 = potential->cutoffRadius();
    double rCut1 = rCut0/lMin;

    // estimate number of atoms
    static const double PI43 = 16./3.*std::atan(1.0e0);
    double r2 = rCut1*rCut1;
    double V = PI43*rCut1*r2;

    // select atoms within cutoff radius
    std::vector<Atom> atoms1;
    atoms1.reserve(2*static_cast<size_t>(std::ceil(V/lattice->volume())));
    atoms1.push_back(atoms[0]);
    for (unsigned int n=1; n < atoms.size(); n++) {
      if (atoms[n].r > rCut1) continue;
      atoms1.push_back(atoms[n]);
    }
    
    // compute interaction potential (and its derivatives)
    double W = potential->InteractionEnergy(material,extPar,atoms1,
                                            C,S,M,first,second);
    double W0 = material.getDoubleProperty("REFERENCE_ENERGY");
    
    // change units and divide by volume
    double angstrom = material.getDoubleProperty("UNIT_ANGSTROM");
    double eVolt = material.getDoubleProperty("UNIT_ELECTRON_VOLT");
    double vol = angstrom*angstrom*angstrom*lattice->volume();
    double coef = eVolt/vol;
    if (first) S *= coef;
    if (second) M *= coef;

    return (W-W0)*coef;
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
