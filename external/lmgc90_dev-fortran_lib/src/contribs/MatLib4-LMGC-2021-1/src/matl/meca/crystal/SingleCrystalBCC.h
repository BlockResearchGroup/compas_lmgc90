/*
 *  $Id: SingleCrystalBCC.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_CRYSTAL_SINGLE_CRYSTAL_BCC_H
#define ZORGLIB_MATL_MECA_CRYSTAL_SINGLE_CRYSTAL_BCC_H

// config
#include <matlib_macros.h>

// STL
#include <utility>
#include <vector>
// local
#include <matl/meca/crystal/SingleCrystal.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing the BCC crystallographic structure.
 */
class SingleCrystalBCC : public SingleCrystal {

 public:
  
  // number of slip systems
  static const unsigned int NSYS = 48;

 private:
  
  // slip systems
  static const double S[48][3],M[48][3];
  static const char SYS_NAME[48][25];
  
 public:
    
  //constructor
  SingleCrystalBCC() {}
  
  // copy constructor
  SingleCrystalBCC(const SingleCrystalBCC&) {}
  
  // destructor
  virtual ~SingleCrystalBCC() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties&,const Rotation&);
  
  // compute plastic potential and derivatives
  unsigned int nSystems() {return NSYS;}
  
  // self-documenting utility
  std::string labelSystem(unsigned int n) const {return SYS_NAME[n];}
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
