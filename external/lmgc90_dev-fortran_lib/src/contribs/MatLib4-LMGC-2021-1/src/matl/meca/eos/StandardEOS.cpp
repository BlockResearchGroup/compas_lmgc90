/*
 *  $Id: StandardEOS.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "StandardEOS.h"

// std C library
#include <cmath>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


// check consistency of material properties
void StandardEOS::checkProperties(MaterialProperties& mater,std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {

  if (os) (*os) << "\n\t***Standard equation-of-state***\n";

  // get bulk modulus
  double K = mater.getDoubleProperty("BULK_MODULUS");
  if (K < 0.0e0) {
    if (os) (*os) << "ERROR: bulk modulus must be positive.\n";
    throw InvalidPropertyException("bulk modulus");
  }

  if (os) (*os) << "\tbulk modulus = " << K << std::endl;   
}

// compute stored energy
double StandardEOS::storedEnergy(const MaterialProperties& mater,
                                 const ParameterSet& extPat,
                                 double J,double& p,double& K,
                                 bool first,bool second) {
  // bulk modulus
  double B = mater.getDoubleProperty("BULK_MODULUS");
  
  // potential
  double defVol = std::log(J);
  double press = B*defVol;
  double W = 0.5e0*press*defVol;
  
  // pressure
  if (first) p = press/J;
  
  // bulk modulus
  if (second) K = (B/J-p)/J;
  
  return W;
}
