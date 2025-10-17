/*
 *  $Id: StandardEOS.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_EOS_STANDARD_EOS_H
#define ZORGLIB_MATL_MECA_EOS_STANDARD_EOS_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/eos/EOS.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for standard equation-of-state.
 */
class StandardEOS : virtual public EOS {
  
 public:
  
  // default constructor
  StandardEOS() {};
  
  // copy constructor
  StandardEOS(const StandardEOS&) {}
  
  // destructor
  virtual ~StandardEOS() {}

  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException);
  
  // compute stored energy
  double storedEnergy(const MaterialProperties&,const ParameterSet&,
                      double,double&,double&,bool,bool);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
