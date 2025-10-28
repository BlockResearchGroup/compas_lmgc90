/*
 *  $Id: ModelDictionary.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "ModelDictionary.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class ModelDictionary.
 */

// get list of constitutive models
StringMap<ModelBuilder*>::Type& ModelDictionary::models() {
  static StringMap<ModelBuilder*>::Type theModels;
  return theModels;
}

// add model associated to keyword
void ModelDictionary::add(const std::string& key,ModelBuilder& m) {
  // overwrite existing model, if any
  if (models().count(key)) delete models()[key];
  models()[key] = &m;
}

// get model associated to keyword
ModelBuilder& ModelDictionary::get(const std::string& key)
 throw (NoSuchModelException) {
  // check for model
  if (models().count(key))
    return *(models()[key]);
  else
    throw NoSuchModelException(key);
}

// list all models
void ModelDictionary::list(std::ostream& os) {
  os << "Material model dictionary contents:" << std::endl;
  StringMap<ModelBuilder*>::Type::iterator it;
  for (it = models().begin(); it != models().end(); it++)
    os << "\t" << (*it).first << std::endl;
}

