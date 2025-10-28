/*
 *  $Id: StdLinChemicalCapacity.h 169 2015-08-10 09:34:30Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_DIFFUSION_LINEAR_STD_CHEMICAL_CAPACITY_H
#define ZORGLIB_MATL_DIFFUSION_LINEAR_STD_CHEMICAL_CAPACITY_H

// config
#include <matlib_macros.h>

// local
#include <matl/diff/linear/LinVariationalDiffusion.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for standard (linearized) chemical capacity potential.
 */
class StdLinChemicalCapacity : virtual public LinChemicalCapacity {
  
 public:
  
  // default constructor
  StdLinChemicalCapacity() {}
  
  // copy constructor
  StdLinChemicalCapacity(const StdLinChemicalCapacity&) {}
  
  // destructor
  virtual ~StdLinChemicalCapacity() {}
  
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
