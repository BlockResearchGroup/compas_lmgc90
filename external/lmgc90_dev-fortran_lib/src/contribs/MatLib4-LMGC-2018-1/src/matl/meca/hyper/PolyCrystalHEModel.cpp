/*
 *  $Id: PolyCrystalHEModel.cpp 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "PolyCrystalHEModel.h"

// std C++ library
#include <fstream>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class TextureProperty.
 */

// copy constructor
TextureProperty::TextureProperty(const TextureProperty& src) {
  orientations = src.orientations;
  weights = src.weights;
}

// read from a file
void TextureProperty::readFrom(const char* iFileName) throw (FileException) {
  // open input file
  if (iFileName) {
    std::ifstream file(iFileName);
    if (!file.is_open()) {
      std::string msg("cannot open mesh file: ");
      msg += iFileName;
      throw FileException(msg);
    }
    readFrom(file);
  }
  else 
    readFrom(std::cin);
}
void TextureProperty::readFrom(std::istream& is) {
  char buffer[256];
  
  // comment line
  is.getline(buffer,255);

  // number of grains
  unsigned int ng,type;
  is >> ng >> type;
  orientations.reserve(ng);
  weights.reserve(ng);
  
  // read grains
  double a1,a2,a3,w;
  for (unsigned int n=0; n < ng; n++) {
    is >> a1 >> a2 >> a3 >> w;
    Rotation3D R(a1,a2,a3,static_cast<Rotation3D::Type>(type));
    orientations.push_back(R);
    weights.push_back(w);
  }
}

// output as a string
std::string TextureProperty::toString() const {
  O_STRING_STREAM os;
  for (unsigned int n=0; n < orientations.size(); n++) {
    double a1,a2,a3;
    orientations[n].toEulerAngles(a1,a2,a3);
    os << a1 << "," << a2 << "," << a3 << "," << weights[n] << std::endl;
  }
#ifdef HAVE_SSTREAM
  return os.str();
#else
  return std::string(os.str(),os.pcount());
#endif
}


/*
 * Methods for class CubicPolyCrystalHElasticityBuilder.
 */

// the instance
CubicPolyCrystalHElasticityBuilder const* CubicPolyCrystalHElasticityBuilder::BUILDER 
= new CubicPolyCrystalHElasticityBuilder();

// constructor
CubicPolyCrystalHElasticityBuilder::CubicPolyCrystalHElasticityBuilder() {
  ModelDictionary::add("CUBIC_POLY_CRYSTAL_HYPER_ELASTICITY",*this);
}

// build model
ConstitutiveModel* CubicPolyCrystalHElasticityBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new CubicPolyCrystalHElasticity3D();
      break;
    default:
      return 0;
      break;
  }
}
