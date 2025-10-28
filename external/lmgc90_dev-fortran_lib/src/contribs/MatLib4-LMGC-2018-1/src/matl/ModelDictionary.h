/*
 *  $Id: ModelDictionary.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MODEL_DICTIONARY_H
#define ZORGLIB_MATL_MODEL_DICTIONARY_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include <data/Exceptions.h>
#include <data/StringMap.h>
#include <matl/ConstitutiveModel.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Interface for constitutive model builders
 */
class ModelBuilder {

 protected:
  
  // constructor
  ModelBuilder() {}

 public:
  
  // destructor
  virtual ~ModelBuilder() {}

  // build model
  virtual ConstitutiveModel* build(unsigned int) const = 0;
};


/**
 * Exception thrown when a model is not found in the dictionary.
 */
class NoSuchModelException : public ZException {
  
 public:
  
  // default constructor
  NoSuchModelException(const std::string& msg = "no such model")
  : ZException(msg) {}
  
  // copy constructor
  NoSuchModelException(const NoSuchModelException& src)
  : ZException(src) {}
};

/**
 * Constitutive model dictionary.
 */
class ModelDictionary {
  
 private:
  
  // list of constitutive models
  static StringMap<ModelBuilder*>::Type& models();
    
  // private constructor
  ModelDictionary() {}
  
 public:

  // add model associated to keyword
  static void add(const std::string&,ModelBuilder&);

  // build model associated to keyword
  static ConstitutiveModel* build(const std::string& key,unsigned int d = 3)
    throw (NoSuchModelException) {return get(key).build(d);}

  // get model associated to keyword
  static ModelBuilder& get(const std::string&)
    throw (NoSuchModelException);
  
  // list all models
  static void list(std::ostream&);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
