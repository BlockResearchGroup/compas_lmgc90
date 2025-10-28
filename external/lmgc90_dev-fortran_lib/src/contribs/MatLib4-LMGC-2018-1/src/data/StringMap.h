/*
 *  $Id: StringMap.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_STRING_MAP_H
#define ZORGLIB_DATA_STRING_MAP_H

// config
#include <matlib_macros.h>

// std C++ library
#include <string>
// STL
#ifdef HAVE_UNORDERED_MAP
#include <unordered_map>
#elif defined(__GNUG__) && defined(HAVE_TR1_UNORDERED_MAP)
#include <tr1/unordered_map>
#elif defined(HAVE_HASH_MAP)
#include <hash_map>
#elif defined(__GNUG__) && defined(HAVE_EXT_HASH_MAP)
#include <ext/hash_map>
#else
#include <map>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Hash function for strings (taken from Stroustrup).
 */
struct HashString : public std::unary_function<std::string,size_t> {
  size_t operator()(const std::string&) const;
};


/**
 * StringMap structure, emulating a templated typedef.
 */
template <class T>
struct StringMap {

  // typedef
#ifdef HAVE_UNORDERED_MAP
  typedef std::unordered_map<std::string,T,HashString> Type;
#elif defined(__GNUG__) && defined(HAVE_TR1_UNORDERED_MAP)
  typedef std::tr1::unordered_map<std::string,T,HashString> Type;
#elif defined(HAVE_HASH_MAP)
  typedef std::hash_map<std::string,T,HashString> Type;
#elif defined(__GNUG__) && defined(HAVE_EXT_HASH_MAP)
#if __GNUC__ == 3 && __GNUC_MINOR__ == 0
  typedef std::hash_map<std::string,T,HashString> Type;
#else
  typedef __gnu_cxx::hash_map<std::string,T,HashString> Type;
#endif
#else
  typedef std::map<std::string,T> Type;
#endif
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
