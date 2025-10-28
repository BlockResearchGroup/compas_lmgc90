/*
 *  $Id: MaterialProperties.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MATERIAL_PROPERTIES_H
#define ZORGLIB_MATL_MATERIAL_PROPERTIES_H

// config
#include <matlib_macros.h>

// std C++ library
#include <iostream>
#include <string>
// std C library
#include <cstdlib>
// STL
#include <vector>
// local
#ifndef WITH_MATLIB_H
#include <data/FileException.h>
#include <data/Property.h>
#include <data/SyntaxError.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Exception thrown when a property is not found in the table.
 */
class NoSuchPropertyException : public ZException {
  
 public:
  
  // default constructor
  NoSuchPropertyException(const std::string& msg = "no such property")
  : ZException(msg) {}
  
  // copy constructor
  NoSuchPropertyException(const NoSuchPropertyException& src)
  : ZException(src) {}
};


/**
 * Class containing a set of material properties.
 */
class MaterialProperties {

 public:

  // define iterators
  typedef PropertyTable::iterator Iterator;
  typedef PropertyTable::const_iterator ConstIterator;
  
 private:
  
  // name of material
  std::string name;
  
  // table of properties
  PropertyTable properties;
  
 public:
  
  // constructor
  MaterialProperties(const std::string& s = "no name") {name = s;}
  
  // copy constructor
  MaterialProperties(const MaterialProperties&);
  
  // destructor
  ~MaterialProperties();
  
  // assignment operator
  MaterialProperties& operator=(const MaterialProperties&);
  
  // clear data
  void clear() {properties.clear();}

  // get material's name
  std::string getName() const {return name;}

  // check if property exists
  bool checkProperty(const std::string&) const;

  // get property associated to keyword
  Property& getProperty(const std::string&) const
    throw (NoSuchPropertyException);
  int getIntegerProperty(const std::string&) const
    throw (NoSuchPropertyException);
  double getDoubleProperty(const std::string&) const
    throw (NoSuchPropertyException);
  std::string getStringProperty(const std::string&) const
    throw (NoSuchPropertyException);
  Function& getFunctionProperty(const std::string&) const
    throw (NoSuchPropertyException);
  
  // set property associated to keyword
  void setProperty(const std::string&,Property&);
  void setProperty(const std::string&,int);
  void setProperty(const std::string&,double);
  void setProperty(const std::string&,const char*);
  void setProperty(const std::string&,Function&);
  
  // iterators
  ConstIterator begin() const {return properties.begin();}
  ConstIterator end() const {return properties.end();}

  // read from an input stream
  void readFrom(std::istream&,const char* = 0) throw (SyntaxError);
  void readFrom(const char* = 0) throw (FileException, SyntaxError);
  
  // utility functions
  static void pullProperties(unsigned int,const MaterialProperties&,MaterialProperties&);
  static void pushProperties(unsigned int,const MaterialProperties&,MaterialProperties&);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
