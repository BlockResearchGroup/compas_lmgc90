/*
 *  $Id: CriterionDictionary.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_CRITERION_DICTIONARY_H
#define ZORGLIB_MATL_CRITERION_DICTIONARY_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include <data/Exceptions.h>
#include <data/StringMap.h>
#include <matl/MaterialCriterion.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Interface for material criterion builders
 */
class CriterionBuilder {

 protected:
  
  // constructor
  CriterionBuilder() {}

 public:
  
  // destructor
  virtual ~CriterionBuilder() {}

  // build model
  virtual MaterialCriterion* build(unsigned int) const = 0;
};


/**
 * Exception thrown when a criterion is not found in the dictionary.
 */
class NoSuchCriterionException : public ZException {
  
 public:
  
  // default constructor
  NoSuchCriterionException(const std::string& msg = "no such criterion")
  : ZException(msg) {}
  
  // copy constructor
  NoSuchCriterionException(const NoSuchCriterionException& src)
  : ZException(src) {}
};

/**
 * Material criterion dictionary.
 */
class CriterionDictionary {
  
 private:
  
  // list of constitutive models
  static StringMap<CriterionBuilder*>::Type& criteria();
    
  // private constructor
  CriterionDictionary() {}
  
 public:

  // add criterion associated to keyword
  static void add(const std::string&,CriterionBuilder&);

  // build criterion associated to keyword
  static MaterialCriterion* build(const std::string& key,unsigned int d = 3)
    throw (NoSuchCriterionException) {return get(key).build(d);}

  // get model associated to keyword
  static CriterionBuilder& get(const std::string&)
    throw (NoSuchCriterionException);
  
  // list all models
  static void list(std::ostream&);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
