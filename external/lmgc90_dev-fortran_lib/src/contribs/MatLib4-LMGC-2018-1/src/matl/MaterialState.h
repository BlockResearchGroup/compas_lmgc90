/*
 *  $Id: MaterialState.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MATERIAL_STATE_H
#define ZORGLIB_MATL_MATERIAL_STATE_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include <data/Copiable.h>
#include <data/ShortArray.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/*
 * Define MatLib type (for interface).
 */
typedef ShortArray MatLibArray;

/**
 * Class describing the state of a material point.
 */
class MaterialState {

 public:

  // constructor
  MaterialState() {extra = 0;}

  // destructor
  ~MaterialState() {if (extra) delete extra;}

  // assignment operator
  MaterialState& operator=(const MaterialState&);

  // external variables
  MatLibArray grad;

  // associated forces
  MatLibArray flux;

  // internal variables
  MatLibArray internal;
  
  // extra data
  Copiable* extra;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
