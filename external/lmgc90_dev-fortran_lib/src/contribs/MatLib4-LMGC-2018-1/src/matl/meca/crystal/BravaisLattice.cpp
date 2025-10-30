/*
 *  $Id: BravaisLattice.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "BravaisLattice.h"

// std C library
#include <cmath>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for BravaisLattice.
 */

// constructor
BravaisLattice::BravaisLattice(double a1[],double a2[],double a3[]) {
  
  // assign lattice vectors
  for (unsigned int i=0; i < 3; i++) {
    v[0][i] = a1[i];
    v[1][i] = a2[i];
    v[2][i] = a3[i];
  }
  vol = computeVolume();
  
  // make sure we are well-oriented
  if (vol < 0.0e0) {
    Vector3D tmp = v[1];
    v[1] = v[2];
    v[2] = tmp;
    vol = -vol;
  }
}

// copy constructor
BravaisLattice::BravaisLattice(const BravaisLattice& src) {
  for (unsigned int i=0; i < 3; i++) v[i] = src.v[i];
  vol = src.vol;
}

// compute volume
double BravaisLattice::computeVolume() {
  return v[0][0]*(v[1][1]*v[2][2]-v[1][2]*v[2][1])
        -v[1][0]*(v[0][1]*v[2][2]-v[0][2]*v[2][1])
        +v[2][0]*(v[0][1]*v[1][2]-v[0][2]*v[1][1]);
}

// generate a crystallite (crystal sample) of radius R centered at origin
void BravaisLattice::genCrystal(double rMax,std::vector<Atom>& atoms) const {
  
  static const double PI43 = 16./3.*std::atan(1.0e0);

  // estimate number of atoms
  double rMax2 = rMax*rMax;
  double V = PI43*rMax*rMax2;
  atoms.clear();
  atoms.reserve(2*static_cast<size_t>(std::ceil(V/vol)));
  
  // first atom is at origin
  Atom atom;
  for (unsigned int i=0; i < 3; i++) atom.x[i] = 0.0e0;
  atom.r = 0.0e0;
  atoms.push_back(atom);
                     
  // compute normal to 1-2 plane
  Vector3D N3;
  N3[0] =  v[0][1]*v[1][2]-v[0][2]*v[1][1];
  N3[1] = -v[0][0]*v[1][2]+v[0][2]*v[1][0];
  N3[2] =  v[0][0]*v[1][1]-v[0][1]*v[1][0];
  double lN3 = normL2(N3);
  double coef = 1.0e0/lN3;
  N3 *= coef;
  double l3N = v[2]*N3; /* should always be positive, because checked before */
  Vector3D T3 = v[2]-l3N*N3;
  
  // compute in-plane orthonormal axes
  Vector3D N1,N2;
  double l1 = normL2(v[0]);
  coef = 1.0e0/l1;
  N1 = coef*v[0];
  double l2T = v[1]*N1;
  N2 = v[1]-l2T*N1;
  double l2N = v[1]*N2;
  double lT31 = T3*N1;
  double lT32 = T3*N2;

  // loop along lattice vectors
  int nl3 = static_cast<int>(std::ceil(rMax/l3N));
  int nf3 = static_cast<int>(std::floor(-rMax/l3N));
  for (int n3=nf3; n3 <= nl3; n3++) {
    Vector3D d3 = n3*v[2];
    double h = n3*l3N;
    double h2 = h*h;
    if (h2 > rMax2) continue;
    double r3 = std::sqrt(rMax2-h2);
    double r32 = r3*r3;
    double dy = n3*lT32;
    int nl2 = static_cast<int>(std::ceil((r3-dy)/l2N));
    int nf2 = static_cast<int>(std::floor((-r3-dy)/l2N));
    for (int n2=nf2; n2 <= nl2; n2++) {
      Vector3D d2 = n2*v[1];
      double g = dy+n2*l2N;
      double g2 = g*g;
      if (g2 > r32) continue;
      double r2 = std::sqrt(r32-g2);
      double dx = n3*lT31+n2*l2T;
      int nl1 = static_cast<int>(std::ceil((r2-dx)/l1));
      int nf1 = static_cast<int>(std::floor((-r2-dx)/l1));
      for (int n1=nf1; n1 <= nl1; n1++) {
        Vector3D d1 = n1*v[0];
        for (unsigned int i=0; i < 3; i++) atom.x[i] = d1[i]+d2[i]+d3[i];
        double RR = atom.x*atom.x;
        if (RR > rMax2) continue;
        if (n1 == 0 && n2 == 0 && n3 == 0) continue;
        atom.r = std::sqrt(RR);
        atoms.push_back(atom);
      }
    }
  }
}


/*
 * Methods for BCCBravaisLattice.
 */

// constructor
BCCBravaisLattice::BCCBravaisLattice(double a) {
  
  // assign lattice vectors (from lattice parameter)
  double val = 0.5*a;
  v[0][0] = -val; v[0][1] =  val; v[0][2] =  val;
  v[1][0] =  val; v[1][1] = -val; v[1][2] =  val;
  v[2][0] =  val; v[2][1] =  val; v[2][2] = -val;
  
  // compute volume
  vol = computeVolume();
}


/*
 * Methods for FCCBravaisLattice.
 */

// constructor
FCCBravaisLattice::FCCBravaisLattice(double a) {

  // assign lattice vectors (from lattice parameter)
  double val = 0.5*a;
  v[0][0] = 0.0; v[0][1] = val; v[0][2] = val;
  v[1][0] = val; v[1][1] = 0.0; v[1][2] = val;
  v[2][0] = val; v[2][1] = val; v[2][2] = 0.0;
  
  // compute volume
  vol = computeVolume();
}
