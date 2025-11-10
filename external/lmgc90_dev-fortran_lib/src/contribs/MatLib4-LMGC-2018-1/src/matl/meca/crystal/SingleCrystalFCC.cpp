/*
 *  $Id: SingleCrystalFCC.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "SingleCrystalFCC.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

// std C library
#include <cmath>
// local
#include <math/Vector3D.h>
#include <math/Tensor3D.h>

// description of slip systems

static const double RAC2 = std::sqrt(0.5e0);       // 0.70710678118654746
static const double RAC3 = std::sqrt(1.0e0/3.0e0); // 0.57735026918962584

// slip directions
const double SingleCrystalFCC::S[24][3] = {
  {0.0e0,-RAC2, RAC2},{ RAC2,0.0e0, RAC2},{ RAC2, RAC2,0.0e0},
  {0.0e0,-RAC2, RAC2},{ RAC2,0.0e0,-RAC2},{ RAC2,-RAC2,0.0e0},
  {0.0e0, RAC2, RAC2},{ RAC2,0.0e0, RAC2},{ RAC2,-RAC2,0.0e0},
  {0.0e0, RAC2, RAC2},{ RAC2,0.0e0,-RAC2},{ RAC2, RAC2,0.0e0},

  {0.0e0, RAC2,-RAC2},{-RAC2,0.0e0,-RAC2},{-RAC2,-RAC2,0.0e0},
  {0.0e0, RAC2,-RAC2},{-RAC2,0.0e0, RAC2},{-RAC2, RAC2,0.0e0},
  {0.0e0,-RAC2,-RAC2},{-RAC2,0.0e0,-RAC2},{-RAC2, RAC2,0.0e0},
  {0.0e0,-RAC2,-RAC2},{-RAC2,0.0e0, RAC2},{-RAC2,-RAC2,0.0e0}};

// slip plane normals
const double SingleCrystalFCC::M[24][3] = {
  {-RAC3, RAC3, RAC3},{-RAC3, RAC3, RAC3},{-RAC3, RAC3, RAC3},
  { RAC3, RAC3, RAC3},{ RAC3, RAC3, RAC3},{ RAC3, RAC3, RAC3},
  { RAC3, RAC3,-RAC3},{ RAC3, RAC3,-RAC3},{ RAC3, RAC3,-RAC3},
  { RAC3,-RAC3, RAC3},{ RAC3,-RAC3, RAC3},{ RAC3,-RAC3, RAC3},

  {-RAC3, RAC3, RAC3},{-RAC3, RAC3, RAC3},{-RAC3, RAC3, RAC3},
  { RAC3, RAC3, RAC3},{ RAC3, RAC3, RAC3},{ RAC3, RAC3, RAC3},
  { RAC3, RAC3,-RAC3},{ RAC3, RAC3,-RAC3},{ RAC3, RAC3,-RAC3},
  { RAC3,-RAC3, RAC3},{ RAC3,-RAC3, RAC3},{ RAC3,-RAC3, RAC3}};

// slip system description
const char SingleCrystalFCC::SYS_NAME[24][25] = {
  "A2:[ 0 -1  1](-1  1  1)","A3:[ 1  0  1](-1  1  1)","A6:[ 1  1  0](-1  1  1)",
  "B2:[ 0 -1  1]( 1  1  1)","B4:[ 1  0 -1]( 1  1  1)","B5:[ 1 -1  0]( 1  1  1)",
  "C1:[ 0  1  1]( 1  1 -1)","C3:[ 1  0  1]( 1  1 -1)","C5:[ 1 -1  0]( 1  1 -1)",
  "D1:[ 0  1  1]( 1 -1  1)","D4:[ 1  0 -1]( 1 -1  1)","D6:[ 1  1  0]( 1 -1  1)",

  "A2:[ 0  1 -1](-1  1  1)","A3:[-1  0 -1](-1  1  1)","A6:[-1 -1  0](-1  1  1)",
  "B2:[ 0  1 -1]( 1  1  1)","B4:[-1  0  1]( 1  1  1)","B5:[-1  1  0]( 1  1  1)",
  "C1:[ 0 -1 -1]( 1  1 -1)","C3:[-1  0 -1]( 1  1 -1)","C5:[-1  1  0]( 1  1 -1)",
  "D1:[ 0 -1 -1]( 1 -1  1)","D4:[-1  0  1]( 1 -1  1)","D6:[-1 -1  0]( 1 -1  1)"};

// check consistency of material properties
void SingleCrystalFCC::checkProperties(MaterialProperties& mater,std::ostream* out)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (out) (*out) << "\n\t***FCC crystallographic structure***\n";
  
  // store slip system description
  SlipSystemsProperty<Vector3D> systems(NSYS);
  for (unsigned int k=0; k < NSYS; k++)
    for (unsigned char i=0; i < 3; i++) {
      (systems.system(k).first)[i]  = S[k][i];
      (systems.system(k).second)[i] = M[k][i];
    }
  mater.setProperty("SLIP_SYSTEMS",systems);
}

// apply rotation to material properties
void SingleCrystalFCC::rotateProperties(MaterialProperties& mater,const Rotation& R) {
  
  // get slip systems
  SlipSystemsProperty<Vector3D>& systems
    = dynamic_cast<SlipSystemsProperty<Vector3D>&>(mater.getProperty("SLIP_SYSTEMS"));
  
  // rotate vectors
  Tensor3D R0;
  R.toTensor(R0);
  for (unsigned int k=0; k < NSYS; k++) {
    systems.system(k).first  = R0*systems.system(k).first;
    systems.system(k).second = R0*systems.system(k).second;
  }
}

