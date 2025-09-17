/*
 *  $Id: SingleCrystalBCC.cpp 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "SingleCrystalBCC.h"

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
static const double RAC6 = std::sqrt(1.0e0/6.0e0); // 0.40824829046386302
static const double RTRD = std::sqrt(2.0e0/3.0e0); // 0.81649658092772603

// slip directions
const double SingleCrystalBCC::S[48][3] = {
  { RAC3, RAC3,-RAC3},{ RAC3,-RAC3, RAC3},   // C1 , D1
  { RAC3, RAC3,-RAC3},{ RAC3,-RAC3, RAC3},   // C1', D1" (T)
  {-RAC3, RAC3, RAC3},{ RAC3, RAC3, RAC3},   // A2 , B2
  {-RAC3, RAC3, RAC3},{ RAC3, RAC3, RAC3},   // A2', B2" (T)
  {-RAC3, RAC3, RAC3},{ RAC3, RAC3,-RAC3},   // A3 , C3
  {-RAC3, RAC3, RAC3},{ RAC3, RAC3,-RAC3},   // A3', C3" (T)
  { RAC3, RAC3, RAC3},{ RAC3,-RAC3, RAC3},   // B4 , D4
  { RAC3, RAC3, RAC3},{ RAC3,-RAC3, RAC3},   // B4', D4" (T)
  { RAC3, RAC3, RAC3},{ RAC3, RAC3,-RAC3},   // B5 , C5
  { RAC3, RAC3, RAC3},{ RAC3, RAC3,-RAC3},   // B5', C5" (T)
  {-RAC3, RAC3, RAC3},{ RAC3,-RAC3, RAC3},   // A6 , D6
  {-RAC3, RAC3, RAC3},{ RAC3,-RAC3, RAC3},   // A6', D6" (T)
  { RAC3, RAC3,-RAC3},{ RAC3,-RAC3, RAC3},   // C1 , D1
  { RAC3, RAC3,-RAC3},{ RAC3,-RAC3, RAC3},   // C1', D1" (AT)
  {-RAC3, RAC3, RAC3},{ RAC3, RAC3, RAC3},   // A2 , B2
  {-RAC3, RAC3, RAC3},{ RAC3, RAC3, RAC3},   // A2', B2" (AT)
  {-RAC3, RAC3, RAC3},{ RAC3, RAC3,-RAC3},   // A3 , C3
  {-RAC3, RAC3, RAC3},{ RAC3, RAC3,-RAC3},   // A3', C3" (AT)
  { RAC3, RAC3, RAC3},{ RAC3,-RAC3, RAC3},   // B4 , D4
  { RAC3, RAC3, RAC3},{ RAC3,-RAC3, RAC3},   // B4', D4" (AT)
  { RAC3, RAC3, RAC3},{ RAC3, RAC3,-RAC3},   // B5 , C5
  { RAC3, RAC3, RAC3},{ RAC3, RAC3,-RAC3},   // B5', C5" (AT)
  {-RAC3, RAC3, RAC3},{ RAC3,-RAC3, RAC3},   // A6 , D6
  {-RAC3, RAC3, RAC3},{ RAC3,-RAC3, RAC3}};  // A6', D6" (AT)

// slip plane normals
const double SingleCrystalBCC::M[48][3] = {
  {0.0e0, RAC2, RAC2},{0.0e0, RAC2, RAC2},   // C1 , D1
  { RTRD,-RAC6, RAC6},{ RTRD, RAC6,-RAC6},   // C1', D1" (T)
  {0.0e0,-RAC2, RAC2},{0.0e0,-RAC2, RAC2},   // A2 , B2
  {-RTRD,-RAC6,-RAC6},{ RTRD,-RAC6,-RAC6},   // A2', B2" (T)
  { RAC2,0.0e0, RAC2},{ RAC2,0.0e0, RAC2},   // A3 , C3
  { RAC6, RTRD,-RAC6},{-RAC6, RTRD, RAC6},   // A3', C3" (T)
  { RAC2,0.0e0,-RAC2},{ RAC2,0.0e0,-RAC2},   // B4 , D4
  {-RAC6, RTRD,-RAC6},{-RAC6,-RTRD,-RAC6},   // B4', D4" (T)
  { RAC2,-RAC2,0.0e0},{ RAC2,-RAC2,0.0e0},   // B5 , C5
  {-RAC6,-RAC6, RTRD},{-RAC6,-RAC6,-RTRD},   // B5', C5" (T)
  { RAC2, RAC2,0.0e0},{ RAC2, RAC2,0.0e0},   // A6 , D6
  { RAC6,-RAC6, RTRD},{-RAC6, RAC6, RTRD},   // A6', D6" (T)
  {0.0e0,-RAC2,-RAC2},{0.0e0,-RAC2,-RAC2},   // C1 , D1
  {-RTRD, RAC6,-RAC6},{-RTRD,-RAC6, RAC6},   // C1', D1" (AT)
  {0.0e0, RAC2,-RAC2},{0.0e0, RAC2,-RAC2},   // A2 , B2
  { RTRD, RAC6, RAC6},{-RTRD, RAC6, RAC6},   // A2', B2" (AT)
  {-RAC2,0.0e0,-RAC2},{-RAC2,0.0e0,-RAC2},   // A3 , C3
  {-RAC6,-RTRD, RAC6},{ RAC6,-RTRD,-RAC6},   // A3', C3" (AT)
  {-RAC2,0.0e0, RAC2},{-RAC2,0.0e0, RAC2},   // B4 , D4
  { RAC6,-RTRD, RAC6},{ RAC6, RTRD, RAC6},   // B4', D4" (AT)
  {-RAC2, RAC2,0.0e0},{-RAC2, RAC2,0.0e0},   // B5 , C5
  { RAC6, RAC6,-RTRD},{ RAC6, RAC6, RTRD},   // B5', C5" (AT)
  {-RAC2,-RAC2,0.0e0},{-RAC2,-RAC2,0.0e0},   // A6 , D6
  {-RAC6, RAC6,-RTRD},{ RAC6,-RAC6,-RTRD}};  // A6', D6" (AT)

// slip system description
const char SingleCrystalBCC::SYS_NAME[48][25] = {
  "C1 :[ 1  1 -1]( 0  1  1)","D1 :[ 1 -1  1]( 0  1  1)",
  "C1':[ 1  1 -1]( 2 -1  1)","D1\":[ 1 -1  1]( 2  1 -1)",
  "A2 :[-1  1  1]( 0 -1  1)","B2 :[ 1  1  1]( 0 -1  1)",
  "A2':[-1  1  1](-2 -1 -1)","B2\":[ 1  1  1]( 2 -1 -1)",
  "A3 :[-1  1  1]( 1  0  1)","C3 :[ 1  1 -1]( 1  0  1)",
  "A3':[-1  1  1]( 1  2 -1)","C3\":[ 1  1 -1](-1  2  1)",
  "B4 :[ 1  1  1]( 1  0 -1)","D4 :[ 1 -1  1]( 1  0 -1)",
  "B4':[ 1  1  1](-1  2 -1)","D4\":[ 1 -1  1](-1 -2 -1)",
  "B5 :[ 1  1  1]( 1 -1  0)","C5 :[ 1  1 -1]( 1 -1  0)",
  "B5':[ 1  1  1](-1 -1  2)","C5\":[ 1  1 -1](-1 -1 -2)",
  "A6 :[-1  1  1]( 1  1  0)","D6 :[ 1 -1  1]( 1  1  0)",
  "A6':[-1  1  1]( 1 -1  2)","D6\":[ 1 -1  1](-1  1  2)",
  "C1 :[ 1  1 -1]( 0 -1 -1)","D1 :[ 1 -1  1]( 0 -1 -1)",
  "C1':[ 1  1 -1](-2  1 -1)","D1\":[ 1 -1  1](-2 -1  1)",
  "A2 :[-1  1  1]( 0  1 -1)","B2 :[ 1  1  1]( 0  1 -1)",
  "A2':[-1  1  1]( 2  1  1)","B2\":[ 1  1  1](-2  1  1)",
  "A3 :[-1  1  1](-1  0 -1)","C3 :[ 1  1 -1](-1  0 -1)",
  "A3':[-1  1  1](-1 -2  1)","C3\":[ 1  1 -1]( 1 -2 -1)",
  "B4 :[ 1  1  1](-1  0  1)","D4 :[ 1 -1  1](-1  0  1)",
  "B4':[ 1  1  1]( 1 -2  1)","D4\":[ 1 -1  1]( 1  2  1)",
  "B5 :[ 1  1  1](-1  1  0)","C5 :[ 1  1 -1](-1  1  0)",
  "B5':[ 1  1  1]( 1  1 -2)","C5\":[ 1  1 -1]( 1  1  2)",
  "A6 :[-1  1  1](-1 -1  0)","D6 :[ 1 -1  1](-1 -1  0)",
  "A6':[-1  1  1](-1  1 -2)","D6\":[ 1 -1  1]( 1 -1 -2)"};

// check consistency of material properties
void SingleCrystalBCC::checkProperties(MaterialProperties& mater,std::ostream* os)
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\n\t***BCC crystallographic structure***\n";
  
  // store slip system description
  SlipSystemsProperty<Vector3D> systems(NSYS);
  for (unsigned int k=0; k < NSYS; k++)
    for (unsigned int i=0; i < 3; i++) {
      (systems.system(k).first)[i]  = S[k][i];
      (systems.system(k).second)[i] = M[k][i];
    }
  mater.setProperty("SLIP_SYSTEMS",systems);
}

// apply rotation to material properties
void SingleCrystalBCC::rotateProperties(MaterialProperties& mater,const Rotation& R) {
  
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
