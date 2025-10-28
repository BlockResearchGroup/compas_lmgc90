/*
 *  $Id: TensorAlgebra.h 124 2013-01-11 16:41:33Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATH_TENSOR_ALGEBRA_H
#define ZORGLIB_MATH_TENSOR_ALGEBRA_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include <data/ShortArray.h>
#include <data/ShortSqrMatrix.h>
#endif
#include <math/SymTensor3D.h>
#include <math/SymTensor2D.h>
#include <math/SymTensor1D.h>
#include <math/SymTensor4_3D.h>
#include <math/SymTensor4_2D.h>
#include <math/SymTensor4_1D.h>
#include <math/Tensor3D.h>
#include <math/Tensor2D.h>
#include <math/Tensor1D.h>
#include <math/Tensor4_3D.h>
#include <math/Tensor4_2D.h>
#include <math/Tensor4_1D.h>
#include <math/Vector3D.h>
#include <math/Vector2D.h>
#include <math/Vector1D.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing standard 3D Euclidean tensor algebra.
 */
struct StdTensorAlgebra3D {
  
  // constants
  static const unsigned int DIMENSION = 3;

  // types
  typedef SymTensor3D SymTensor;
  typedef Vector3D    Vector;
};

/**
 * Class describing standard 2D Euclidean tensor algebra.
 */
struct StdTensorAlgebra2D {
  
  // constants
  static const unsigned int DIMENSION = 2;
  
  // types
  typedef StdSymTensor2D SymTensor;
  typedef Vector2D       Vector;
};

/**
 * Class describing standard 2D Euclidean tensor algebra.
 */
struct StdTensorAlgebra1D {
  
  // constants
  static const unsigned int DIMENSION = 1;
  
  // types
  typedef StdSymTensor1D SymTensor;
  typedef StdSymTensor1D Tensor;
  typedef Vector1D Vector;
};

/**
 * Class describing 3D Euclidean tensor algebra for mechanics.
 */
struct TensorAlgebra3D {
  
  // useful shortcuts
  typedef ShortArray     ARRAY;
  typedef ShortSqrMatrix MATRIX;

  // constants
  static const unsigned int DIMENSION = 3;
  
  // types
  struct SymTensor {
    typedef SymTensor3D TYPE;
    static const unsigned int MEMSIZE = 6;
    static ARRAY identity();
    static void cofactor(const ARRAY&,ARRAY&);
    static double inverse(const ARRAY&,ARRAY&);
    static double determinant(const ARRAY&);
    static double trace(const ARRAY&);
    static double innerProd(const ARRAY&,const ARRAY&);
    static void log(const ARRAY&,ARRAY&,ARRAY[] = 0,ARRAY[][6] = 0,
                    bool = false,bool = false);
    static void covariant(const ARRAY&,ARRAY&);
    static void contravariant(const ARRAY&,ARRAY&);
  };
  struct SymTensor4 {
    typedef SymTensor4_3D TYPE;
    static MATRIX identity();
    static MATRIX contravariantIdentity();
    static MATRIX covariantIdentity();
    static MATRIX baseJ();
    static MATRIX baseK();
    static void addIKJL(double,const ARRAY&,MATRIX&);
    static void outerProd(const ARRAY&,MATRIX&);
    static void outerProd2(const ARRAY&,const ARRAY&,MATRIX&);
    static void rightProd(const MATRIX&,const ARRAY&,ARRAY&);
  };
  struct Tensor {
    typedef Tensor3D TYPE;
    static const unsigned int MEMSIZE = 9;
    static ARRAY identity();
    static void cofactor(const ARRAY&,ARRAY&);
    static double inverse(const ARRAY&,ARRAY&);
    static double determinant(const ARRAY&);
    static double trace(const ARRAY&);
    static void prod(const ARRAY&,const ARRAY&,ARRAY&);
    static void log(const ARRAY&,ARRAY&,ARRAY[] = 0,ARRAY[][9] = 0,
                    bool = false,bool = false);
  };
  
  typedef Tensor4_3D Tensor4;
  typedef Vector3D   Vector;
  typedef Vector3D   LongVector;

  // operations
  static void RightCauchyGreen(const ARRAY&,ARRAY&);
  static void RUDecomposition(const ARRAY&,ARRAY&,ARRAY&);
  static void CauchyToPK1(const ARRAY&,const ARRAY&,ARRAY&);
  static void PK1ToCauchy(const ARRAY&,const ARRAY&,ARRAY&);
  static void PK2ToKirchhoff(const ARRAY&,const ARRAY&,ARRAY&);
  static void PK2ToPK1(const ARRAY&,const ARRAY&,ARRAY&);
  static void MaterialToLagrangian(const MATRIX&,const ARRAY&,const ARRAY&,
                                   MATRIX&);
  static void SpatialToLagrangian(const MATRIX&,const ARRAY&,const ARRAY&,
                                  MATRIX&);
};

/**
 * Class describing 2D Euclidean tensor algebra for mechanics.
 */
struct TensorAlgebra2D {
  
 public:
  
  // useful shortcuts
  typedef ShortArray     ARRAY;
  typedef ShortSqrMatrix MATRIX;
  
  // constants
  static const unsigned int DIMENSION = 2;
  
  // types
  struct SymTensor {
    typedef SymTensor2D TYPE;
    static const unsigned int MEMSIZE = 4;
    static ARRAY identity();
    static void cofactor(const ARRAY&,ARRAY&);
    static double inverse(const ARRAY&,ARRAY&);
    static double determinant(const ARRAY&);
    static double trace(const ARRAY&);
    static double innerProd(const ARRAY&,const ARRAY&);
    static void log(const ARRAY&,ARRAY&,ARRAY[] = 0,ARRAY[][4] = 0,
                    bool = false,bool = false);
    static void covariant(const ARRAY&,ARRAY&);
    static void contravariant(const ARRAY&,ARRAY&);
  };
  struct SymTensor4 {
    typedef SymTensor4_2D TYPE;
    static MATRIX identity();
    static MATRIX contravariantIdentity();
    static MATRIX covariantIdentity();
    static MATRIX baseJ();
    static MATRIX baseK();
    static void addIKJL(double,const ARRAY&,MATRIX&);
    static void outerProd(const ARRAY&,MATRIX&);
    static void outerProd2(const ARRAY&,const ARRAY&,MATRIX&);
    static void rightProd(const MATRIX&,const ARRAY&,ARRAY&);
  };
  struct Tensor {
    typedef Tensor2D TYPE;
    static const unsigned int MEMSIZE = 5;
    static ARRAY identity();
    static void cofactor(const ARRAY&,ARRAY&);
    static double inverse(const ARRAY&,ARRAY&);
    static double determinant(const ARRAY&);
    static double trace(const ARRAY&);
    static void prod(const ARRAY&,const ARRAY&,ARRAY&);
    static void log(const ARRAY&,ARRAY&,ARRAY[] = 0,ARRAY[][5] = 0,
                    bool = false,bool = false);
  };
  
  typedef Tensor4_2D Tensor4;
  typedef Vector2D   Vector;
  typedef Vector3D   LongVector;

  // operations
  static void RightCauchyGreen(const ARRAY&,ARRAY&);
  static void RUDecomposition(const ARRAY&,ARRAY&,ARRAY&);
  static void CauchyToPK1(const ARRAY&,const ARRAY&,ARRAY&);
  static void PK1ToCauchy(const ARRAY&,const ARRAY&,ARRAY&);
  static void PK2ToKirchhoff(const ARRAY&,const ARRAY&,ARRAY&);
  static void PK2ToPK1(const ARRAY&,const ARRAY&,ARRAY&);
  static void MaterialToLagrangian(const MATRIX&,const ARRAY&,const ARRAY&,
                                   MATRIX&);
  static void SpatialToLagrangian(const MATRIX&,const ARRAY&,const ARRAY&,
                                  MATRIX&);
};

/**
 * Class describing 1D Euclidean tensor algebra for mechanics.
 */
struct TensorAlgebra1D {
  
 public:
  
  // useful shortcuts
  typedef ShortArray     ARRAY;
  typedef ShortSqrMatrix MATRIX;
  
  // constants
  static const unsigned int DIMENSION = 1;
  
  // types
  struct SymTensor {
    typedef SymTensor1D TYPE;
    static const unsigned int MEMSIZE = 3;
    static ARRAY identity();
    static void cofactor(const ARRAY&,ARRAY&);
    static double inverse(const ARRAY&,ARRAY&);
    static double determinant(const ARRAY&);
    static double trace(const ARRAY&);
    static double innerProd(const ARRAY&,const ARRAY&);
    static void log(const ARRAY&,ARRAY&,ARRAY[] = 0,ARRAY[][3] = 0,
                     bool = false,bool = false);
    static void covariant(const ARRAY&,ARRAY&);
    static void contravariant(const ARRAY&,ARRAY&);
  };
  struct SymTensor4 {
    typedef SymTensor4_1D TYPE;
    static MATRIX identity();
    static MATRIX contravariantIdentity();
    static MATRIX covariantIdentity();
    static MATRIX baseJ();
    static MATRIX baseK();
    static void addIKJL(double,const ARRAY&,MATRIX&);
    static void outerProd(const ARRAY&,MATRIX&);
    static void outerProd2(const ARRAY&,const ARRAY&,MATRIX&);
    static void rightProd(const MATRIX&,const ARRAY&,ARRAY&);
  };
  struct Tensor {
    typedef Tensor1D TYPE;
    static const unsigned int MEMSIZE = 3;
    static ARRAY identity();
    static void cofactor(const ARRAY&,ARRAY&);
    static double inverse(const ARRAY&,ARRAY&);
    static double determinant(const ARRAY&);
    static double trace(const ARRAY&);
    static void prod(const ARRAY&,const ARRAY&,ARRAY&);
    static void log(const ARRAY&,ARRAY&,ARRAY[] = 0,ARRAY[][3] = 0,
                    bool = false,bool = false);
  };

  typedef Tensor4_1D Tensor4;
  typedef Vector1D   Vector;
  typedef Vector3D   LongVector;

  // operations
  static void RightCauchyGreen(const ARRAY&,ARRAY&);
  static void RUDecomposition(const ARRAY&,ARRAY&,ARRAY&);
  static void CauchyToPK1(const ARRAY&,const ARRAY&,ARRAY&);
  static void PK1ToCauchy(const ARRAY&,const ARRAY&,ARRAY&);
  static void PK2ToKirchhoff(const ARRAY&,const ARRAY&,ARRAY&);
  static void PK2ToPK1(const ARRAY&,const ARRAY&,ARRAY&);
  static void MaterialToLagrangian(const MATRIX&,const ARRAY&,const ARRAY&,
                                   MATRIX&);
  static void SpatialToLagrangian(const MATRIX&,const ARRAY&,const ARRAY&,
                                  MATRIX&);
};

// symmetrize
inline
SymTensor1D covariantSym(const Tensor1D& A) {return A.covariant();}
inline
SymTensor1D contravariantSym(const Tensor1D& A) {return A.contravariant();}
inline
SymTensor2D covariantSym(const Tensor2D& A) {return A.covariantSym();}
inline
SymTensor2D contravariantSym(const Tensor2D& A) {return A.contravariantSym();}
inline
SymTensor3D covariantSym(const Tensor3D& A) {return A.covariantSym();}
inline
SymTensor3D contravariantSym(const Tensor3D& A) {return A.contravariantSym();}

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
