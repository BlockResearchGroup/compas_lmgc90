/*
 *  $Id: Property.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_PROPERTY_H
#define ZORGLIB_DATA_PROPERTY_H

// config
#include <matlib_macros.h>

// std C++ library
#include <iostream>
#include <string>
// local
#ifndef WITH_MATLIB_H
#include <data/Cloneable.h>
#include <data/Function.h>
#include <data/StringMap.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for multi-type properties.
 */
class Property : virtual public Cloneable {

 public:

  // destructor
  virtual ~Property() {}

  // duplicate object
  virtual Property* clone() const = 0;

  // output as a string
  virtual std::string toString() const = 0;
};

// overload the output stream operator
inline std::ostream& operator<<(std::ostream& os,const Property& x) {
  os << x.toString(); return os;
}


/**
 * Integer-valued property.
 */
class IntegerProperty : public Property {

 private:

  int val;

 public:

  // default constructor
  IntegerProperty(int v = 0) {val = v;}

  // copy constructor
  IntegerProperty(const IntegerProperty& src) {val = src.val;}

  // destructor
  virtual ~IntegerProperty() {}

  // get value
  int value() const {return val;}

  // set value
  void setValue(int v) {val = v;}

  // duplicate object
  IntegerProperty* clone() const {return new IntegerProperty(*this);}

  // output as a string
  std::string toString() const;
};


/**
 * Real-valued (double precision) property.
 */
class DoubleProperty : public Property {

 private:

  double val;

 public:

  // default constructor
  DoubleProperty(double v = 0.e0) {val = v;}

  // copy constructor
  DoubleProperty(const DoubleProperty& src) {val = src.val;}

  // destructor
  virtual ~DoubleProperty() {}

  // get value
  double value() const {return val;}

  // set value
  void setValue(double v) {val = v;}

  // duplicate object
  DoubleProperty* clone() const {return new DoubleProperty(*this);}

  // output as a string
  std::string toString() const;
};


/**
 * String-valued (character string) property.
 */
class StringProperty : public Property {
  
 private:
  
  std::string val;
  
 public:
  
  // default constructor
  StringProperty(const char* s) {val = s;}
  
  // copy constructor
  StringProperty(const StringProperty& src) {val = src.val;}
  
  // destructor
  virtual ~StringProperty() {}
  
  // get value
  std::string value() const {return val;}
  
  // set value
  void setValue(const char* s) {val = s;}
  
  // duplicate object
  StringProperty* clone() const {return new StringProperty(*this);}
  
  // output as a string
  std::string toString() const {return val;}
};


/**
 * Function-valued property.
 */
class FunctionProperty : public Property {

 private:

  Function *fct;

 public:

  // default constructor
  FunctionProperty(Function& f) {fct = f.clone();}

  // copy constructor
  FunctionProperty(const FunctionProperty& src) {
    fct = (src.fct)->clone();
  }

  // destructor
  virtual ~FunctionProperty() {delete fct;}

  // get value
  Function& function() const {return *fct;}

  // set value
  void setFunction(Function& f) {
    delete fct; fct = f.clone();
  }

  // duplicate object
  FunctionProperty* clone() const {return new FunctionProperty(*this);}

  // output as a string
  std::string toString() const;
};


/**
 * Template-based property.
 */
template <class T>
class StdProperty : public Property {

 private:
  
  // data
  T *data;

 public:

  // default constructor
  StdProperty() {data = new T();}
  
  // constructor
  StdProperty(const T& d) {data = new T(d);}
  
  // copy constructor
  StdProperty(const StdProperty& src) {data = new T(*(src.data));}
  
  // destructor
  virtual ~StdProperty() {delete data;}
  
  // get value
  T& value() const {return *data;}
  
  // duplicate object
  StdProperty* clone() const {return new StdProperty(*this);}
  
  // output as a string
  std::string toString() const {return data->toString();}
};


/**
 * Define new type PropertyTable
 */
typedef StringMap<Property*>::Type PropertyTable;

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
