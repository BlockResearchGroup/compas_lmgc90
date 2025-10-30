/*
 *  $Id: CriterionDictionary.cpp 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "CriterionDictionary.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class CriterionDictionary.
 */

// get list of material criteria
StringMap<CriterionBuilder*>::Type& CriterionDictionary::criteria() {
  static StringMap<CriterionBuilder*>::Type theCriteria;
  return theCriteria;
}

// add criterion associated to keyword
void CriterionDictionary::add(const std::string& key,CriterionBuilder& c) {
  // overwrite existing criterion, if any
  if (criteria().count(key)) delete criteria()[key];
  criteria()[key] = &c;
}

// get model associated to keyword
CriterionBuilder& CriterionDictionary::get(const std::string& key)
 throw (NoSuchCriterionException) {
  // check for criterion
  if (criteria().count(key))
    return *(criteria()[key]);
  else
    throw NoSuchCriterionException(key);
}

// list all criteria
void CriterionDictionary::list(std::ostream& os) {
  os << "Material criterion dictionary contents:" << std::endl;
  StringMap<CriterionBuilder*>::Type::iterator it;
  for (it = criteria().begin(); it != criteria().end(); it++)
    os << "\t" << (*it).first << std::endl;
}

