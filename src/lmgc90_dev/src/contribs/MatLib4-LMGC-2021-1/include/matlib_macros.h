/*
 *  $Id: matlib_macros.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef MATLIB_MACROS_H
#define MATLIB_MACROS_H

// configuration parameters
#include "matlib_config.h"

// macro for string streams
#ifdef HAVE_SSTREAM
#include <sstream>
#define O_STRING_STREAM std::ostringstream
#else
#include <strstream>
#define O_STRING_STREAM std::ostrstream
#endif

// macros for namespace
#if defined(MATLIB_USE_NAMESPACE) && defined(MATLIB_USE_MATLIB_NAMESPACE)
#define MATLIB_NAMESPACE matlib::
#define BEGIN_MATLIB_NAMESPACE namespace matlib {
#define END_MATLIB_NAMESPACE }
#define USING_MATLIB_NAMESPACE using namespace matlib;
#elif defined(MATLIB_USE_NAMESPACE) && defined(MATLIB_USE_ZORGLIB_NAMESPACE)
#define MATLIB_NAMESPACE zorglib::
#define BEGIN_MATLIB_NAMESPACE namespace zorglib {
#define END_MATLIB_NAMESPACE }
#define USING_MATLIB_NAMESPACE using namespace zorglib;
#else
#define MATLIB_NAMESPACE   
#endif

// macro for fortran calls
#ifdef FORTRAN_UNDERSCORE
#define FORTRAN(name) name ## _
#else
#define FORTRAN(name) name
#endif

// Windows Visual Studio
#if defined(_WIN32) || defined(_WIN64)
#pragma warning( disable : 4290 )
#endif

// anything else ?


#endif
