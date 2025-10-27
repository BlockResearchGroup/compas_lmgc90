/*
 *  $Id: config.h.cmake.in 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */

/* Define to 1 if you have _ appended to F77 functions */
#define FORTRAN_UNDERSCORE 1

/* Define to 1 if you have the <unordered_map> header file. */
#define HAVE_UNORDERED_MAP 1

/* Define to 1 if you have the <unordered_map> header file. */
#define HAVE_TR1_UNORDERED_MAP 1

/* Define to 1 if you have the <hash_map> header file. */
#define HAVE_HASH_MAP 1

/* Define to 1 if you have the <ext/hash_map> header file. */
#define HAVE_EXT_HASH_MAP 1

/* Define to 1 if you have the <sstream> header file. */
#define HAVE_SSTREAM 1

/* Define to 1 to use Lapack library */
#define MATLIB_USE_LAPACK 1

/* Define to 1 to use vecLib library */
/* #undef MATLIB_USE_VECLIB */

/* Define to 1 to use a namespace */
#define MATLIB_USE_NAMESPACE 1

/* Define to 1 to use namespace zorglib */
#define MATLIB_USE_ZORGLIB_NAMESPACE ON

/* Define to 1 to use namespace matlib */
/* #undef MATLIB_USE_MATLIB_NAMESPACE */
