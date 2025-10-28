/*
 *  $Id: MathUtils.h 143 2014-04-18 07:12:34Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_UTILS_H
#define ZORGLIB_MATH_UTILS_H

// config
#include <matlib_macros.h>

// std C library
#include <stddef.h>
//std C++ library
#include <limits>
// local
#ifndef WITH_MATLIB_H
#include <data/ShortArray.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

// default precision for matrix inversion
static const double DEFAULT_MATH_PRECISION = 1.0e-16;
//static const double DEFAULT_MATH_PRECISION = std::numeric_limits<double>::min();

/*
 * Prototypes
 */

// vector functions
void addvec(const double[],const double[],double[],unsigned int);
void mulvec(double,const double[],double[],unsigned int);
double nrmvec0(const double[],unsigned int);
double nrmvec1(const double[],unsigned int);
double nrmvec2(const double[],unsigned int);

// 2x2 matrix functions
void mulmat2(const double *,const double *,double *);
double detmat2(const double *);
double invmat2(const double *,double *,double = DEFAULT_MATH_PRECISION);
double tnvmat2(const double *,double *,double = DEFAULT_MATH_PRECISION);
double detsym2(const double *);
double invsym2(const double *,double *,double = DEFAULT_MATH_PRECISION);

// 3x3 matrix functions
void mulmat3(const double *,const double *,double *);
double detmat3(const double *);
double invmat3(const double *,double *,double = DEFAULT_MATH_PRECISION);
double tnvmat3(const double *,double *,double = DEFAULT_MATH_PRECISION);
double detsym3(const double *);
double invsym3(const double *,double *,double = DEFAULT_MATH_PRECISION);

// templated functions
template<unsigned int DIM>
double determinant(const ShortArray&);
template<unsigned int DIM>
void eigenSym(const ShortArray&,double[]);
template<unsigned int DIM>
double innerProd(const ShortArray&,const ShortArray&);
template<unsigned int DIM>
double invert(const ShortArray&,ShortArray&);
template<unsigned int DIM>
void transpose(const ShortArray&,ShortArray&);

template<unsigned int DIM>
ShortArray identSym();
template<unsigned int DIM>
ShortArray identity();
template<unsigned int DIM>
double trSym(const ShortArray&);
template<unsigned int DIM>
double trace(const ShortArray&);

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
