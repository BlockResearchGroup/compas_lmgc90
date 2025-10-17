/*
 *  $Id: SingleCrystal.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_CRYSTAL_SINGLE_CRYSTAL_H
#define ZORGLIB_MATL_MECA_CRYSTAL_SINGLE_CRYSTAL_H

// config
#include <matlib_macros.h>

// local
#include <matl/ConstitutiveModel.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Generic class for describing a crystallographic structure.
 */
class SingleCrystal {

 protected:

  //constructor
  SingleCrystal() {}
  
  // copy constructor
  SingleCrystal(const SingleCrystal&) {}

 public:
  
  // destructor
  virtual ~SingleCrystal() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // apply rotation to material properties
  virtual void rotateProperties(MaterialProperties&,const Rotation&) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // compute plastic potential and derivatives
  virtual unsigned int nSystems() = 0;
  
  // self-documenting utility
  virtual std::string labelSystem(unsigned int) const = 0;
};


/**
 * Template class to store slip systems.
 */
template <class T>
class SlipSystemsProperty : public Property {
  
 private:
  
  // data
  std::vector< std::pair<T,T> > systems;
  
 public:
  
  // default constructor
  SlipSystemsProperty(unsigned int n) {systems.resize(n);}
  
  // copy constructor
  SlipSystemsProperty(const SlipSystemsProperty& src) {systems = src.systems;}
  
  // destructor
  virtual ~SlipSystemsProperty() {}
  
  // get value
  std::pair<T,T>& system(unsigned int n) {return systems[n];}
  
  // duplicate object
  SlipSystemsProperty* clone() const {return new SlipSystemsProperty(*this);}
  
  // output as a string
  std::string toString() const {
    O_STRING_STREAM os;
    for (unsigned int n=0; n < systems.size(); n++)
      os << systems[n].first << "," << systems[n].second << std::endl;
#ifdef HAVE_SSTREAM
    return os.str();
#else
    return std::string(os.str(),os.pcount());
#endif
  }
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
