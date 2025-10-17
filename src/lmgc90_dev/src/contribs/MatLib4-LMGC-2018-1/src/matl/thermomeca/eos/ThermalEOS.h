/*
 *  $Id: ThermalEOS.h 142 2014-02-07 12:51:54Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_EOS_EQUATION_OF_STATE_H
#define ZORGLIB_MATL_MECA_THERMO_EOS_EQUATION_OF_STATE_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/eos/EOS.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Virtual base class for equation-of-states with thermal dependence.
 */
class ThermalEOS : virtual public EOS {

 protected:
  
  // constructor
  ThermalEOS() {}

 public:
  
  // destructor
  virtual ~ThermalEOS() {}
  
  // compute stored energy (with explicit temperature dependence)
  virtual double storedThMEnergy(const MaterialProperties&,const ParameterSet&,
                                 double,double,double&,double&,
                                 double&,double&,double&,bool,bool) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
