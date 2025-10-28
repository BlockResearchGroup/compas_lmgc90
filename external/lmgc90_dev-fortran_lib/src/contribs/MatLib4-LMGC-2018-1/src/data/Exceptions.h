/*
 *  $Id: Exceptions.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_EXCEPTIONS_H
#define ZORGLIB_DATA_EXCEPTIONS_H

// config
#include <matlib_macros.h>

// std C++ library
#include <string>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for Zorglib exceptions.
 */
class ZException {

 private:
  
  // associated message
  std::string message;
  
 public:
  
  // constructor
  ZException(const std::string& s) {message = s;}
  
  // copy constructor
  ZException(const ZException& src) {message = src.message;}
  
  // destructor
  ~ZException() {}
  
  // get message
  std::string mesg() const {return message;}
};


/**
 * Base class for Zorglib errors.
 */
class ZError {
  
 private:
  
  // associated message
  std::string message;
  
 public:
  
  // constructor
  ZError(const std::string& s) {message = s;}
  
  // copy constructor
  ZError(const ZError& src) {message = src.message;}
  
  // destructor
  ~ZError() {}
  
  // get message
  std::string mesg() const {return message;}
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
