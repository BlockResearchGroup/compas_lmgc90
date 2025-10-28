/*
 *  $Id: StdLinThermalCapacity.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_THERMO_LINEAR_STD_THERMAL_CAPACITY_H
#define ZORGLIB_MATL_THERMO_LINEAR_STD_THERMAL_CAPACITY_H

// config
#include <matlib_macros.h>

// local
#include <matl/thermo/linear/LinVariationalConduction.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for standard (linearized) thermal capacity potential.
 */
class StdLinThermalCapacity : virtual public LinThermalCapacity {
  
 public:
  
  // default constructor
  StdLinThermalCapacity() {}
  
  // copy constructor
  StdLinThermalCapacity(const StdLinThermalCapacity&) {}
  
  // destructor
  virtual ~StdLinThermalCapacity() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // compute 
  double internalEnergy(const MaterialProperties&,const ParameterSet&,
                        double,double&,double&,bool,bool);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
