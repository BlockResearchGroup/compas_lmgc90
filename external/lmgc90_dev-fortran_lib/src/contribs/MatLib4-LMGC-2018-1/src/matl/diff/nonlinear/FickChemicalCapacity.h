/*
 *  $Id: FickChemicalCapacity.h 207 2016-08-19 16:52:36Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFFUSION_NONLINEAR_FICK_CHEMICAL_CAPACITY_H
#define ZORGLIB_MATL_DIFFUSION_NONLINEAR_FICK_CHEMICAL_CAPACITY_H

// config
#include <matlib_macros.h>

// local
#include <matl/diff/nonlinear/VariationalDiffusion.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for Fickian chemical capacity potential.
 */
class FickChemicalCapacity : virtual public ChemicalCapacity {
  
 public:
  
  // default constructor
  FickChemicalCapacity() {}
  
  // copy constructor
  FickChemicalCapacity(const FickChemicalCapacity&) {}
  
  // destructor
  virtual ~FickChemicalCapacity() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // compute 
  double GibbsEnergy(const MaterialProperties&,const ParameterSet&,
                     double,double&,double&,bool,bool);
  double dualGibbsEnergy(const MaterialProperties&,const ParameterSet&,
                         double,double&,double&,bool,bool);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
