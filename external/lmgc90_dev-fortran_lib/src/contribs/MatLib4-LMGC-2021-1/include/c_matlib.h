/*
 *  $Id: c_matlib.h 264 2019-11-15 15:36:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2019, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_INTERFACE_H
#define ZORGLIB_MATL_INTERFACE_H


/**
 * Declare interfaces useable in C.
 */

/*************
 * Functions *
 *************/

/* load new function */
extern void matlib_NewTabulatedFunction(int,char[],int,double[],double[]);

/* evaluate given function */
extern double matlib_EvalFunction(int,double);


/****************
 * MaterialData *
 ****************/

/* load new material data */
extern void matlib_NewMaterialData(int,char[]);
extern void matlib_NewMaterialDataFrom(int,char[]);

/* manipulate material properties */
extern void matlib_CopyMaterialDataName(int,char[]);

extern void matlib_AddIntegerProperty(int,char[],int);
extern void matlib_AddDoubleProperty(int,char[],double);
extern void matlib_AddFunctionProperty(int,char[],int);
extern int    matlib_GetIntegerProperty(int,char[]);
extern double matlib_GetDoubleProperty(int,char[]);
extern double matlib_GetFunctionProperty(int,char[],double);


/*********************
 * ConstitutiveModel *
 *********************/

#define MATLIB_TYPE_NONE       0
#define MATLIB_TYPE_SCALAR     1
#define MATLIB_TYPE_VECTOR     2
#define MATLIB_TYPE_SYM_TENSOR 3
#define MATLIB_TYPE_TENSOR     4

/* create new constitutive model */
extern void matlib_NewConstitutiveModel(int,char[],unsigned int);

/* manipulate constitutive model */
extern void matlib_CheckProperties(int,int,char[]);
extern void matlib_RotateProperties(int,int,double[],double[]);
extern void matlib_UpdateProperties(int,int,char*[],double[],unsigned int);

extern int matlib_NExtVar(int);
extern int matlib_NExtVarBundled(int);
extern int matlib_TypeExtVar(int,unsigned int);
extern int matlib_IndexExtVar(int,unsigned int);
extern void matlib_LabelExtVar(int,unsigned int,char[]);
extern void matlib_LabelExtForce(int,unsigned int,char[]);

extern int matlib_NIntVar(int);
extern int matlib_NIntVarBundled(int);
extern int matlib_TypeIntVar(int,unsigned int);
extern int matlib_IndexIntVar(int,unsigned int);
extern void matlib_LabelIntVar(int,unsigned int,char[]);

extern int matlib_IsStandard(int);

extern void matlib_InitState(int,int,double[],double[],double[]);
extern void matlib_UpdateState(int,int,char*[],double[],unsigned int,double[],double[],
                               double[],double[],double[],double[],double,
			                         double*,int);
extern double matlib_IncrementalPotential(int,int,char*[],double[],unsigned int,double[],
                                          double[],double[],double[],double[],
                                          double[],double,double*,int,int);
extern void matlib_ComputeTangent(int,int,char*[],double[],unsigned int,double[],double[],
                                  double[],double[],double[],double[],double,double*);
extern void matlib_ComputeNumericalTangent(int,int,char*[],double[],unsigned int,double[],double[],
                                           double[],double[],double[],double[],double,double*);


/*********************
 * MaterialCriterion *
 *********************/

/* create new material criterion */
extern void matlib_NewMaterialCriterion(int,char[],unsigned int);

/* manipulate material criterion */
extern void matlib_CheckCriterionProperties(int,int,char[]);
extern void matlib_RotateCriterionProperties(int,int,double[],double[]);
extern void matlib_UpdateCriterionProperties(int,int,char*[],double[],unsigned int);

extern int matlib_NExtVarCriterion(int);
extern int matlib_NIntVarCriterion(int);

extern double matlib_EvaluateCriterion(int,int,char*[],double[],unsigned int,double[],
                                       double[],double[],double*,double);

#endif
