/*
 *  $Id: EOS.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_EOS_EQUATION_OF_STATE_H
#define ZORGLIB_MATL_MECA_EOS_EQUATION_OF_STATE_H

// config
#include <matlib_macros.h>

// local
#include <matl/ConstitutiveModel.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Virtual base class for equation-of-states.
 */
class EOS {

 protected:
  
  // constructor
  EOS() {}

 public:
  
  // destructor
  virtual ~EOS() {}

  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // compute stored energy
  virtual double storedEnergy(const MaterialProperties&,const ParameterSet&,
                              double,double&,double&,bool,bool) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
