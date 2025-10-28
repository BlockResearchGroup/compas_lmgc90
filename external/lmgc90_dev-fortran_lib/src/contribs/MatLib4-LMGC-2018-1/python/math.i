/*
 *  $Id: math.i 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
%{
#include <math/MathUtils.h>
#include <math/TensorAlgebra.h>
  
#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif
%}


/*****************************
   Tensor algebra operations
 *****************************/

/**
 * Class encapsulating 1D vector objects.
 * It inherits methods and internal structure from ShortArray.
 */
class Vector1D : virtual public ShortArray {
  
 public:
  
  static const unsigned int MEMSIZE=1;
  
  // default constructor
  Vector1D();
  
  // constructor (also serves as copy constructor)
  Vector1D(const ShortArray&,unsigned int = 0);
  Vector1D(const ShortArrayExpression&,unsigned int = 0);
  
  // assignment operator
  %ignore operator=;
  Vector1D& operator=(const Vector1D&) throw (std::range_error);
  Vector1D& operator=(const ShortArrayExpression&) throw (std::range_error);
  Vector1D& operator=(double);
  
  // inner product operator
  double operator*(const Vector1D&) const;
};

/**
 * Class encapsulating 2D vector objects.
 * It inherits methods and internal structure from ShortArray.
 */
class Vector2D : virtual public ShortArray {
  
 public:
  
  static const unsigned int MEMSIZE=2;
  
  // default constructor
  Vector2D();
  
  // constructor (also serves as copy constructor)
  Vector2D(const ShortArray&,unsigned int = 0);
  Vector2D(const ShortArrayExpression&,unsigned int = 0);
  
  // assignment operator
  %ignore operator=;
  Vector2D& operator=(const Vector2D&) throw (std::range_error);
  Vector2D& operator=(const ShortArrayExpression&) throw (std::range_error);
  Vector2D& operator=(double);
  
  // inner product operator
  double operator*(const Vector2D&) const;
};

/**
 * Class encapsulating 3D vector objects.
 * It inherits methods and internal structure from ShortArray.
 */
class Vector3D : virtual public ShortArray {
  
 public:
  
  static const unsigned int MEMSIZE=3;
  
  // default constructor
  Vector3D();
  
  // constructor (also serves as copy constructor)
  Vector3D(const ShortArray&,unsigned int = 0);
  Vector3D(const ShortArrayExpression&,unsigned int = 0);
  
  // assignment operator
  %ignore operator=;
  Vector3D& operator=(const Vector3D&) throw (std::range_error);
  Vector3D& operator=(const ShortArrayExpression&) throw (std::range_error);
  Vector3D& operator=(double);
  
  // inner product operator
  double operator*(const Vector3D&) const;
};

Vector3D crossProd(const Vector3D&,const Vector3D&);

/****************************
   Basic algebra operations
 ****************************/

// default precision for matrix inversion
static const double DEFAULT_MATH_PRECISION = 1.0e-16;

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
