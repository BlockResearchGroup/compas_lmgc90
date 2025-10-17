/*
 *  $Id: FileException.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_FILE_EXCEPTION_H
#define ZORGLIB_DATA_FILE_EXCEPTION_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include <data/Exceptions.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Thrown when a file is not found or cannot be opened.
 */
class FileException : public ZException {
  
 public:
  
  // default constructor
  FileException(const std::string& msg = "") : ZException(msg) {}
  
  // copy constructor
  FileException(const FileException& src) : ZException(src) {}
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
