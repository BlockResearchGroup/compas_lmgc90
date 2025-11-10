/*
 *  $Id: MatLibInterface.cpp 264 2019-11-15 15:36:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2019, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */

// config
#include <matlib_macros.h>

// std C library
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
// Visual Studio specific
#if defined(_WIN32) || defined(_WIN64)
#define strcasecmp _stricmp
#endif
// std C++ library
#include <fstream>
// STL
#ifdef HAVE_UNORDERED_MAP
#include <unordered_map>
#elif defined(__GNUG__) && defined(HAVE_TR1_UNORDERED_MAP)
#include <tr1/unordered_map>
#elif defined(HAVE_HASH_MAP)
#include <hash_map>
#elif defined(HAVE_EXT_HASH_MAP)
#include <ext/hash_map>
#else
#include <map>
#endif
// local
#include <data/Rotation3D.h>
#include <data/TabulatedFunction.h>
#include <matl/ModelDictionary.h>
#include <matl/CriterionDictionary.h>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/**
 * Tables of MaterialProperties and ConstitutiveModel objects.
 */
#ifdef HAVE_UNORDERED_MAP
typedef std::unordered_map<int,Function*> FunctionTable;
typedef std::unordered_map<int,MaterialProperties*> MaterialPropertiesTable;
typedef std::unordered_map<int,ConstitutiveModel*> ConstitutiveModelTable;
typedef std::unordered_map<int,MaterialCriterion*> MaterialCriterionTable;
#elif defined(__GNUG__) && defined(HAVE_TR1_UNORDERED_MAP)
typedef std::tr1::unordered_map<int,Function*> FunctionTable;
typedef std::tr1::unordered_map<int,MaterialProperties*> MaterialPropertiesTable;
typedef std::tr1::unordered_map<int,ConstitutiveModel*> ConstitutiveModelTable;
typedef std::tr1::unordered_map<int,MaterialCriterion*> MaterialCriterionTable;
#elif defined(HAVE_HASH_MAP)
typedef std::hash_map<int,Function*> FunctionTable;
typedef std::hash_map<int,MaterialProperties*> MaterialPropertiesTable;
typedef std::hash_map<int,ConstitutiveModel*> ConstitutiveModelTable;
typedef std::hash_map<int,MaterialCriterion*> MaterialCriterionTable;
#elif defined(__GNUG__) && defined(HAVE_EXT_HASH_MAP)
#if __GNUC__ == 3 && __GNUC_MINOR__ == 0
typedef std::hash_map<int,Function*> FunctionTable;
typedef std::hash_map<int,MaterialProperties*> MaterialPropertiesTable;
typedef std::hash_map<int,ConstitutiveModel*> ConstitutiveModelTable;
typedef std::hash_map<int,MaterialCriterion*> MaterialCriterionTable;
#else
typedef __gnu_cxx::hash_map<int,Function*> FunctionTable;
typedef __gnu_cxx::hash_map<int,MaterialProperties*> MaterialPropertiesTable;
typedef __gnu_cxx::hash_map<int,ConstitutiveModel*> ConstitutiveModelTable;
typedef __gnu_cxx::hash_map<int,MaterialCriterion*> MaterialCriterionTable;
#endif
#else
typedef std::map<int,Function*> FunctionTable;
typedef std::map<int,MaterialProperties*> MaterialPropertiesTable;
typedef std::map<int,ConstitutiveModel*> ConstitutiveModelTable;
typedef std::map<int,MaterialCriterion*> MaterialCriterionTable;
#endif

static FunctionTable functionTable;
static MaterialPropertiesTable materialDataTable;
static ConstitutiveModelTable constitutiveModelTable;
static MaterialCriterionTable materialCriterionTable;


/**
 * Declare interfaces useable in C.
 */
extern "C" {

  /*************
   * Functions *
   *************/

  // load new function
  void matlib_NewTabulatedFunction(int,char[],int,double[],double[]);

  // evaluate given function
  double matlib_EvalFunction(int,double);
  

  /**********************
   * MaterialProperties *
   **********************/

  // load new material data
  void matlib_NewMaterialData(int,char[]);
  void matlib_NewMaterialDataFrom(int,char[]);

  // manipulate material properties
  void matlib_CopyMaterialDataName(int,char[]);

  void matlib_AddIntegerProperty(int,char[],int);
  void matlib_AddDoubleProperty(int,char[],double);
  void matlib_AddFunctionProperty(int,char[],int);
  int    matlib_GetIntegerProperty(int,char[]);
  double matlib_GetDoubleProperty(int,char[]);
  double matlib_GetFunctionProperty(int,char[],double);


  /*********************
   * ConstitutiveModel *
   *********************/

  // create new constitutive model
  void matlib_NewConstitutiveModel(int,char[],unsigned int);

  // manipulate constitutive model
  void matlib_CheckProperties(int,int,char[]);
  void matlib_RotateProperties(int,int,double[],double[]);
  void matlib_UpdateProperties(int,int,char*[],double[],unsigned int);

  int matlib_NExtVar(int);
  int matlib_NExtVarBundled(int);
  int matlib_TypeExtVar(int,unsigned int);
  int matlib_IndexExtVar(int,unsigned int);
  void matlib_LabelExtVar(int,unsigned int,char[]);
  void matlib_LabelExtForce(int,unsigned int,char[]);

  int matlib_NIntVar(int);
  int matlib_NIntVarBundled(int);
  int matlib_TypeIntVar(int,unsigned int);
  int matlib_IndexIntVar(int,unsigned int);
  void matlib_LabelIntVar(int,unsigned int,char[]);
  
  int matlib_IsStandard(int);

  void matlib_InitState(int,int,double[],double[],double[]);
  void matlib_UpdateState(int,int,char*[],double[],unsigned int,
                          double[],double[],double[],
                          double[],double[],double[],double,double*,int);
  double matlib_IncrementalPotential(int,int,char*[],double[],unsigned int,
                                     double[],double[],double[],
                                     double[],double[],double[],double,
                                     double*,int,int);
  void matlib_ComputeTangent(int,int,char*[],double[],unsigned int,double[],double[],
                             double[],double[],double[],double[],double,double*);
  void matlib_ComputeNumericalTangent(int,int,char*[],double[],unsigned int,double[],double[],
                                      double[],double[],double[],double[],double,double*);


  /*********************
   * MaterialCriterion *
   *********************/

  /* create new material criterion */
  void matlib_NewMaterialCriterion(int,char[],unsigned int);

  /* manipulate constitutive model */
  void matlib_CheckCriterionProperties(int,int,char[]);
  void matlib_RotateCriterionProperties(int,int,double[],double[]);
  void matlib_UpdateCriterionProperties(int,int,char*[],double[],unsigned int);

  int matlib_NExtVarCriterion(int);
  int matlib_NIntVarCriterion(int);

  double matlib_EvaluateCriterion(int,int,char*[],double[],unsigned int,double[],
                                  double[],double[],double*,double);
}


/*************
 * Functions *
 *************/

// load new function
void matlib_NewTabulatedFunction(int ID,char fctName[],int n,double x[],double y[]) {
  // avoid memory leaks
  if (functionTable.count(ID)) delete functionTable[ID];

  // instanciate new function
  if (fctName)
    functionTable[ID] = new TabulatedFunction(fctName,n,x,y);
  else {
    char str[256];
    std::sprintf(str,"Function #%d",ID);
    functionTable[ID] = new TabulatedFunction(str,n,x,y);
  }
}

// evaluate given function
double matlib_EvalFunction(int ID,double x) {
  return functionTable[ID]->value(x);
}


/**********************
 * MaterialProperties *
 **********************/

// load new material data
void matlib_NewMaterialData(int ID,char materName[]) {
  // avoid memory leaks
  if (materialDataTable.count(ID)) delete materialDataTable[ID];

  // instanciate new material data
  if (materName)
    materialDataTable[ID] = new MaterialProperties(materName);
  else
    materialDataTable[ID] = new MaterialProperties();
}

// read from file
void matlib_NewMaterialDataFrom(int ID,char fileName[]) {
  matlib_NewMaterialData(ID,0);
  std::ifstream inputStream(fileName);
  materialDataTable[ID]->readFrom(inputStream);
}

// get material data name
void matlib_CopyMaterialDataName(int ID,char materName[]) {
  std::strcpy(materName,(materialDataTable[ID]->getName()).c_str());
}

// set or get properties
void matlib_AddIntegerProperty(int ID,char key[],int val) {
  materialDataTable[ID]->setProperty(key,val);
}
void matlib_AddDoubleProperty(int ID,char key[],double val) {
  materialDataTable[ID]->setProperty(key,val);
}
void matlib_AddFunctionProperty(int ID,char key[],int IDFct) {
  materialDataTable[ID]->setProperty(key,*(functionTable[IDFct]));
}
int matlib_GetIntegerProperty(int ID,char key[]) {
  return materialDataTable[ID]->getIntegerProperty(key);
}
double matlib_GetDoubleProperty(int ID,char key[]) {
  return materialDataTable[ID]->getDoubleProperty(key);
}
double matlib_GetFunctionProperty(int ID,char key[],double x) {
  return materialDataTable[ID]->getFunctionProperty(key).value(x);
}


/*********************
 * ConstitutiveModel *
 *********************/

// create new constitutive model
void matlib_NewConstitutiveModel(int ID,char key[],unsigned int dim) {
  // avoid memory leaks
  if (constitutiveModelTable.count(ID)) delete constitutiveModelTable[ID];

  // instanciate new constitutive model
  try {
    constitutiveModelTable[ID] = ModelDictionary::build(key,dim);
  }
  catch (NoSuchModelException) {
    std::fprintf(stderr,"ERROR - invalid model keyword.\n");
    return;
  }
}

// check material data vs. constitutive model
void matlib_CheckProperties(int IDMater,int IDModel,char outputFileName[]) {
  if (outputFileName) {
    if (strcasecmp(outputFileName,"null")) {
      std::ofstream outputStream(outputFileName);
      constitutiveModelTable[IDModel]->checkProperties(*(materialDataTable[IDMater]),
                                                       &outputStream);
      outputStream.flush();
    }
    else
      constitutiveModelTable[IDModel]->checkProperties(*(materialDataTable[IDMater]),(char*)0);
  } else {
    constitutiveModelTable[IDModel]->checkProperties(*(materialDataTable[IDMater]),
                                                     &(std::cout));
    std::cout.flush();
  }
}

// rotate material properties
void matlib_RotateProperties(int IDMater,int IDModel,double axis1[],double axis2[]) {
  // check orthogonality
  double prod = axis1[0]*axis2[0]+axis1[1]*axis2[1]+axis1[2]*axis2[2];
  if (std::fabs(prod) > 1.e-12) {
    std::fprintf(stderr,"ERROR - axes are not orthogonal.\n");
    return;
  }

  // compute rotation matrix
  double axis3[3];
  axis3[0] = axis1[1]*axis2[2]-axis1[2]*axis2[1];
  axis3[1] = axis1[2]*axis2[0]-axis1[0]*axis2[2];
  axis3[2] = axis1[0]*axis2[1]-axis1[1]*axis2[0];
  double val1 = 1.e0/std::sqrt(axis1[0]*axis1[0]+axis1[1]*axis1[1]+axis1[2]*axis1[2]);
  double val2 = 1.e0/std::sqrt(axis2[0]*axis2[0]+axis2[1]*axis2[1]+axis2[2]*axis2[2]);
  double val3 = 1.e0/std::sqrt(axis3[0]*axis3[0]+axis3[1]*axis3[1]+axis3[2]*axis3[2]);
  MatLibMatrix RMat(3);
  RMat[0][0] = axis1[0]*val1; RMat[0][1] = axis1[1]*val1; RMat[0][2] = axis1[2]*val1;
  RMat[1][0] = axis2[0]*val2; RMat[1][1] = axis2[1]*val2; RMat[1][2] = axis2[2]*val2;
  RMat[2][0] = axis3[0]*val3; RMat[2][1] = axis3[1]*val3; RMat[2][2] = axis3[2]*val3;

  // apply rotation
  Rotation3D R(RMat);
  constitutiveModelTable[IDModel]->rotateProperties(*(materialDataTable[IDMater]),R);
}

// update material properties
void matlib_UpdateProperties(int IDMater,int IDModel,
                             char *extPLbl[],double extPVal[],unsigned int nExtPar) {
  ParameterSet eP;
  for (unsigned int n=0; n < nExtPar; n++) eP[extPLbl[n]] = extPVal[n];

  constitutiveModelTable[IDModel]->updateProperties(*(materialDataTable[IDMater]),eP);
}

// size of external variables
int matlib_NExtVar(int ID) {
  return constitutiveModelTable[ID]->nExtVar();
}

// number of external variables
int matlib_NExtVarBundled(int ID) {
  return constitutiveModelTable[ID]->nExtVarBundled();
}

// get external variable type
int matlib_TypeExtVar(int ID,unsigned int n) {
  return constitutiveModelTable[ID]->typeExtVar(n);
}

// get index of external variable n
int matlib_IndexExtVar(int ID,unsigned int n) {
  return constitutiveModelTable[ID]->indexExtVar(n);
}

// get label of external variable n
void matlib_LabelExtVar(int ID,unsigned int n,char str[]) {
  std::strcpy(str,constitutiveModelTable[ID]->labelExtVar(n).c_str());
}

// get label of external force n
void matlib_LabelExtForce(int ID,unsigned int n,char str[]) {
  std::strcpy(str,constitutiveModelTable[ID]->labelExtForce(n).c_str());
}

// size of internal variables
int matlib_NIntVar(int ID) {
  return constitutiveModelTable[ID]->nIntVar();
}

// number of internal variables
int matlib_NIntVarBundled(int ID) {
  return constitutiveModelTable[ID]->nIntVarBundled();
}

// get internal variable type
int matlib_TypeIntVar(int ID,unsigned int n) {
  return constitutiveModelTable[ID]->typeIntVar(n);
}

// get index of internal variable n
int matlib_IndexIntVar(int ID,unsigned int n) {
  return constitutiveModelTable[ID]->indexIntVar(n);
}

// get label of internal variable n
void matlib_LabelIntVar(int ID,unsigned int n,char str[]) {
  std::strcpy(str,constitutiveModelTable[ID]->labelIntVar(n).c_str());
}

// is the model standard ?
int matlib_IsStandard(int ID) {
  if (constitutiveModelTable[ID]->isStandard())
    return 1;
  else
    return 0;
}

// initialize state arrays
void matlib_InitState(int IDMater,int IDModel,double grad[],double flux[],double intV[]) {
  ConstitutiveModel *model = constitutiveModelTable[IDModel];
  unsigned int nExt = model->nExtVar();
  unsigned int nInt = model->nIntVar();

  // fill material state
  MaterialState state;
  state.grad.wrap(grad,nExt);
  state.flux.wrap(flux,nExt);
  state.internal.wrap(intV,nInt);

  // initialize state
  model->initState(*(materialDataTable[IDMater]),state);
}

// update state arrays
void matlib_UpdateState(int IDMater,int IDModel,
                        char *extPLbl[],double extPVal[],unsigned int nExtPar,
                        double grad0[],double flux0[],double intV0[],
                        double grad1[],double flux1[],double intV1[],
                        double dTime,double *K,int tgt) {
  ConstitutiveModel *model = constitutiveModelTable[IDModel];
  unsigned int nExt = model->nExtVar();
  unsigned int nInt = model->nIntVar();

  ParameterSet eP;
  for (unsigned int n=0; n < nExtPar; n++) eP[extPLbl[n]] = extPVal[n];

  MatLibMatrix KMat(K,nExt); // WARNING: C-style ordering for 2D arrays !!!

  // fill material states
  MaterialState state0,state1;
  state0.grad.wrap(grad0,nExt);
  state0.flux.wrap(flux0,nExt);
  state0.internal.wrap(intV0,nInt);
  state1.grad.wrap(grad1,nExt);
  state1.flux.wrap(flux1,nExt);
  state1.internal.wrap(intV1,nInt);

  // update state
  model->updateState(*(materialDataTable[IDMater]),eP,state0,state1,
                     dTime,KMat,tgt);
}

// compute incremental potential
double matlib_IncrementalPotential(int IDMater,int IDModel,
                                   char *extPLbl[],double extPVal[],unsigned int nExtPar,
                                   double grad0[],double flux0[],double intV0[],
                                   double grad1[],double flux1[],double intV1[],
                                   double dTime,double *K,int upd,int tgt) {
  ConstitutiveModel *model = constitutiveModelTable[IDModel];
  StandardMaterial *stdModel = dynamic_cast<StandardMaterial*>(model);
  unsigned int nExt = stdModel->nExtVar();
  unsigned int nInt = stdModel->nIntVar();

  ParameterSet eP;
  for (unsigned int n=0; n < nExtPar; n++) {
    eP[extPLbl[n]] = extPVal[n];
  }
  
  MatLibMatrix KMat(K,nExt); // WARNING: C-style ordering for 2D arrays !!!

  // fill material states
  MaterialState state0,state1;
  state0.grad.wrap(grad0,nExt);
  state0.flux.wrap(flux0,nExt);
  state0.internal.wrap(intV0,nInt);
  state1.grad.wrap(grad1,nExt);
  state1.flux.wrap(flux1,nExt);
  state1.internal.wrap(intV1,nInt);

  // compute potential
  return stdModel->incrementalPotential(*(materialDataTable[IDMater]),eP,
                                        state0,state1,dTime,KMat,upd,tgt);
}

// compute material tangents (without updating)
void matlib_ComputeTangent(int IDMater,int IDModel,
                           char *extPLbl[],double extPVal[],unsigned int nExtPar,
                           double grad0[],double flux0[],double intV0[],
                           double grad1[],double flux1[],double intV1[],
                           double dTime,double *K) {
  ConstitutiveModel *model = constitutiveModelTable[IDModel];
  unsigned int nExt = model->nExtVar();
  unsigned int nInt = model->nIntVar();

  ParameterSet eP;
  for (unsigned int n=0; n < nExtPar; n++) eP[extPLbl[n]] = extPVal[n];

  MatLibMatrix KMat(K,nExt); // WARNING: C-style ordering for 2D arrays !!!

  // fill material states
  MaterialState state0,state1;
  state0.grad.wrap(grad0,nExt);
  state0.flux.wrap(flux0,nExt);
  state0.internal.wrap(intV0,nInt);
  state1.grad.wrap(grad1,nExt);
  state1.flux.wrap(flux1,nExt);
  state1.internal.wrap(intV1,nInt);

  // update state
  model->computeTangent(*(materialDataTable[IDMater]),eP,state0,state1,dTime,KMat);
}

// compute material tangents by numerical perturbation
void matlib_ComputeNumericalTangent(int IDMater,int IDModel,
                                    char *extPLbl[],double extPVal[],unsigned int nExtPar,
                                    double grad0[],double flux0[],double intV0[],
                                    double grad1[],double flux1[],double intV1[],
                                    double dTime,double *K) {
  ConstitutiveModel *model = constitutiveModelTable[IDModel];
  unsigned int nExt = model->nExtVar();
  unsigned int nInt = model->nIntVar();

  ParameterSet eP;
  for (unsigned int n=0; n < nExtPar; n++) eP[extPLbl[n]] = extPVal[n];

  MatLibMatrix KMat(K,nExt); // WARNING: C-style ordering for 2D arrays !!!

  // fill material states
  MaterialState state0,state1;
  state0.grad.wrap(grad0,nExt);
  state0.flux.wrap(flux0,nExt);
  state0.internal.wrap(intV0,nInt);
  state1.grad.wrap(grad1,nExt);
  state1.flux.wrap(flux1,nExt);
  state1.internal.wrap(intV1,nInt);

  // update state
  model->computeNumericalTangent(*(materialDataTable[IDMater]),eP,state0,state1,dTime,KMat);
}


/*********************
 * MaterialCriterion *
 *********************/

/* create new material criterion */
void matlib_NewMaterialCriterion(int ID,char key[],unsigned int dim) {
  // avoid memory leaks
  if (materialCriterionTable.count(ID)) delete materialCriterionTable[ID];

  // instanciate new material criterion
  try {
    materialCriterionTable[ID] = CriterionDictionary::build(key,dim);
  }
  catch (NoSuchCriterionException) {
    std::fprintf(stderr,"ERROR - invalid criterion keyword.\n");
    return;
  }
}

/* manipulate constitutive model */
void matlib_CheckCriterionProperties(int IDMater,int IDCriter,char outputFileName[]) {
  if (outputFileName) {
    if (strcasecmp(outputFileName,"null")) {
      std::ofstream outputStream(outputFileName);
      materialCriterionTable[IDCriter]->checkProperties(*(materialDataTable[IDMater]),
							&outputStream);
      outputStream.flush();
    }
    else
      materialCriterionTable[IDCriter]->checkProperties(*(materialDataTable[IDMater]),(char*)0);
  } else {
    materialCriterionTable[IDCriter]->checkProperties(*(materialDataTable[IDMater]),
						      &(std::cout));
    std::cout.flush();
  }
}
void matlib_RotateCriterionProperties(int IDMater,int IDCriter,double axis1[],double axis2[]) {
  // check orthogonality
  double prod = axis1[0]*axis2[0]+axis1[1]*axis2[1]+axis1[2]*axis2[2];
  if (std::fabs(prod) > 1.e-12) {
    std::fprintf(stderr,"ERROR - axes are not orthogonal.\n");
    return;
  }

  // compute rotation matrix
  double axis3[3];
  axis3[0] = axis1[1]*axis2[2]-axis1[2]*axis2[1];
  axis3[1] = axis1[2]*axis2[0]-axis1[0]*axis2[2];
  axis3[2] = axis1[0]*axis2[1]-axis1[1]*axis2[0];
  double val1 = 1.e0/std::sqrt(axis1[0]*axis1[0]+axis1[1]*axis1[1]+axis1[2]*axis1[2]);
  double val2 = 1.e0/std::sqrt(axis2[0]*axis2[0]+axis2[1]*axis2[1]+axis2[2]*axis2[2]);
  double val3 = 1.e0/std::sqrt(axis3[0]*axis3[0]+axis3[1]*axis3[1]+axis3[2]*axis3[2]);
  MatLibMatrix RMat(3);
  RMat[0][0] = axis1[0]*val1; RMat[0][1] = axis1[1]*val1; RMat[0][2] = axis1[2]*val1;
  RMat[1][0] = axis2[0]*val2; RMat[1][1] = axis2[1]*val2; RMat[1][2] = axis2[2]*val2;
  RMat[2][0] = axis3[0]*val3; RMat[2][1] = axis3[1]*val3; RMat[2][2] = axis3[2]*val3;

  // apply rotation
  Rotation3D R(RMat);
  materialCriterionTable[IDCriter]->rotateProperties(*(materialDataTable[IDMater]),R);
}
void matlib_UpdateCriterionProperties(int IDMater,int IDCriter,
                                      char *extPLbl[],double extPVal[],unsigned int nExtPar) {
  ParameterSet eP;
  for (unsigned int n=0; n < nExtPar; n++) eP[extPLbl[n]] = extPVal[n];

  materialCriterionTable[IDCriter]->updateProperties(*(materialDataTable[IDMater]),eP);
}

int matlib_NExtVarCriterion(int ID) {
  return materialCriterionTable[ID]->nExtVar();
}
int matlib_NIntVarCriterion(int ID) {
  return materialCriterionTable[ID]->nIntVar();
}

double matlib_EvaluateCriterion(int IDMater,int IDCriter,
                                char *extPLbl[],double extPVal[],unsigned int nExtPar,
                                double grad[],double flux[],double intV[],
                                double *K,double dTime) {
  MaterialCriterion *criter = materialCriterionTable[IDCriter];
  unsigned int nExt = criter->nExtVar();
  unsigned int nInt = criter->nIntVar();

  ParameterSet eP;
  for (unsigned int n=0; n < nExtPar; n++) {
    eP[extPLbl[n]] = extPVal[n];
  }
  
  MatLibMatrix KMat(K,nExt); // WARNING: C-style ordering for 2D arrays !!!

  // fill material state
  MaterialState state;
  state.grad.wrap(grad,nExt);
  state.flux.wrap(flux,nExt);
  state.internal.wrap(intV,nInt);

  // evaluate criterion
  return criter->evaluateCriterion(*(materialDataTable[IDMater]),eP,
                                   state,KMat,dTime);
}


/**
 * Declare interfaces useable in Fortran.
 */

#define N_EXT_PAR_MAX 100

extern "C" {

  /*************
   * Functions *
   *************/

  // load new function
  void FORTRAN(f_matlib_new_tabulated_function)(int *ID,char fctName[],int *len,int *n,
                                                double x[],double y[]) {
    fctName[*len] = '\0';
    matlib_NewTabulatedFunction(*ID,fctName,*n,x,y);
  }

  // evaluate given function
  void FORTRAN(f_matlib_eval_function)(int *ID,double *x,double *val) {
    (*val) = matlib_EvalFunction(*ID,*x);
  }

  
 /**********************
  * MaterialProperties *
  **********************/

  // load new material data
  void FORTRAN(f_matlib_new_material_data)(int *ID,char materName[],int *len) {
    materName[*len] = '\0';
    matlib_NewMaterialData(*ID,materName);
  }

  // read from file
  void FORTRAN(f_matlib_new_material_data_from)(int *ID,char fileName[],int *len) {
    fileName[*len] = '\0';
    matlib_NewMaterialDataFrom(*ID,fileName);
  }

  // get material data name
  void FORTRAN(f_matlib_cpy_material_data_name)(int *ID,char materName[],int *len) {
    materName[*len] = '\0';
    matlib_CopyMaterialDataName(*ID,materName);
  }

  // set or get properties
  void FORTRAN(f_matlib_add_integer_property)(int *ID,char key[],int *len,int *val) {
    key[*len] = '\0';
    matlib_AddIntegerProperty(*ID,key,*val);
  }
  void FORTRAN(f_matlib_add_double_property)(int *ID,char key[],int *len,double *val) {
    key[*len] = '\0';
    matlib_AddDoubleProperty(*ID,key,*val);
  }
  void FORTRAN(f_matlib_add_function_property)(int *ID,char key[],int *len,int *IDFct) {
    key[*len] = '\0';
    matlib_AddFunctionProperty(*ID,key,*IDFct);
  }
  void FORTRAN(f_matlib_get_integer_property)(int *ID,char key[],int *len,int *val) {
    key[*len] = '\0';
    (*val) = matlib_GetIntegerProperty(*ID,key);
  }
  void FORTRAN(f_matlib_get_double_property)(int *ID,char key[],int *len,double *val) {
    key[*len] = '\0';
    (*val) = matlib_GetDoubleProperty(*ID,key);
  }
  void FORTRAN(f_matlib_get_function_property)(int *ID,char key[],int *len,
					       double *x,double *val) {
    key[*len] = '\0';
    (*val) = matlib_GetFunctionProperty(*ID,key,*x);
  }


  /*********************
   * ConstitutiveModel *
   *********************/

  // create new constitutive model
  void FORTRAN(f_matlib_new_constitutive_model)(int *ID,char key[],int *len,int *dim) {
    key[*len] = '\0';
    matlib_NewConstitutiveModel(*ID,key,*dim);
  }

  // check material data vs. constitutive model
  void FORTRAN(f_matlib_check_properties)(int *IDMater,int *IDModel,
                                          char outputFileName[],int *len) {
    if (*len > 0) {
      outputFileName[*len] = '\0';
      matlib_CheckProperties(*IDMater,*IDModel,outputFileName);
    }
    else 
      matlib_CheckProperties(*IDMater,*IDModel,0);
  }

  // rotate material properties
  void FORTRAN(f_matlib_rotate_properties)(int *IDMater,int *IDModel,
                                           double axis1[],double axis2[]) {
    matlib_RotateProperties(*IDMater,*IDModel,axis1,axis2);
  }

  // update material properties
  void FORTRAN(f_update_properties)(int *IDMater,int *IDModel,
                                    char extPLbls[],int len[],int *ld,
                                    double extPVal[],int *nExtP) {
    int i; char *extPLbl[N_EXT_PAR_MAX],*p;
    for (i=0,p=extPLbls; i < (*nExtP); i++,p+=(*ld)) {
      p[len[i]] = '\0';
      extPLbl[i] = p;
    }
    matlib_UpdateProperties(*IDMater,*IDModel,extPLbl,extPVal,*nExtP);
  }
  
  // size of external variables
  void FORTRAN(f_matlib_n_external_variables)(int *ID,int *nExt) {
    (*nExt) = matlib_NExtVar(*ID);
  }

  // number of external variables
  void FORTRAN(f_matlib_n_ext_variables_bundled)(int *ID,int *nExt) {
    (*nExt) = matlib_NExtVarBundled(*ID);
  }

  // type of external variable
  void FORTRAN(f_matlib_type_external_variable)(int *ID,int *n,int *type) {
    (*type) = matlib_TypeExtVar(*ID,(*n)-1);
  }

  // index of external variable
  void FORTRAN(f_matlib_index_external_variable)(int *ID,int *n,int *idx) {
    (*idx) = matlib_IndexExtVar(*ID,(*n)-1)+1;
  }

  // label of external variable
  void FORTRAN(f_matlib_label_external_variable)(int *ID,int *n,char *str,int *len) {
    matlib_LabelExtVar(*ID,(*n)-1,str);
    (*len) = strlen(str)-1;
  }

  // label of external force
  void FORTRAN(f_matlib_label_external_force)(int *ID,int *n,char *str,int *len) {
    matlib_LabelExtForce(*ID,(*n)-1,str);
    (*len) = strlen(str)-1;
  }
  
  // size of internal variables
  void FORTRAN(f_matlib_n_internal_variables)(int *ID,int *nInt) {
    (*nInt) = matlib_NIntVar(*ID);
  }

  // number of internal variables
  void FORTRAN(f_matlib_n_int_variables_bundled)(int *ID,int *nInt) {
    (*nInt) = matlib_NIntVarBundled(*ID);
  }

  // type of internal variable
  void FORTRAN(f_matlib_type_internal_variable)(int *ID,int *n,int *type) {
    (*type) = matlib_TypeIntVar(*ID,(*n)-1);
  }

  // index of internal variable
  void FORTRAN(f_matlib_index_internal_variable)(int *ID,int *n,int *idx) {
    (*idx) = matlib_IndexIntVar(*ID,(*n)-1)+1;
  }

  // label of external variable
  void FORTRAN(f_matlib_label_internal_variable)(int *ID,int *n,char *str,int *len) {
    matlib_LabelIntVar(*ID,(*n)-1,str);
    (*len) = strlen(str)-1;
  }
  
  // is the model standard ?
  void FORTRAN(f_matlib_is_standard)(int *ID,int *flag) {
    (*flag) = matlib_IsStandard(*ID);
  }

  // initialize state arrays
  void FORTRAN(f_matlib_init_state)(int *IDMater,int *IDModel,
                                    double grad[],double flux[],double intV[]) {
    matlib_InitState(*IDMater,*IDModel,grad,flux,intV);
  }

  // update state arrays
  void FORTRAN(f_matlib_update_state)(int *IDMater,int *IDModel,
                                      char extPLbls[],int len[],int *ld,
                                      double extPVal[],int *nExtP,
                                      double grad0[],double flux0[],double intV0[],
                                      double grad1[],double flux1[],double intV1[],
                                      double *dTime,double *K,int *tgt) {
    int i; char *extPLbl[N_EXT_PAR_MAX],*p;
    for (i=0,p=extPLbls; i < (*nExtP); i++,p+=(*ld)) {
      p[len[i]] = '\0';
      extPLbl[i] = p;
    }
    matlib_UpdateState(*IDMater,*IDModel,extPLbl,extPVal,*nExtP,grad0,flux0,intV0,
                       grad1,flux1,intV1,*dTime,K,*tgt);
  }

  // compute incremental potential
  void FORTRAN(f_matlib_incremental_potential)(int *IDMater,int *IDModel,
                                               char extPLbls[],int len[],int *ld,
                                               double extPVal[],int *nExtP,
                                               double grad0[],double flux0[],double intV0[],
                                               double grad1[],double flux1[],double intV1[],
                                               double *dTime,double *K,int *upd,int *tgt,
                                               double *W) {
    int i; char *extPLbl[N_EXT_PAR_MAX],*p;
    for (i=0,p=extPLbls; i < (*nExtP); i++,p+=(*ld)) {
      p[len[i]] = '\0';
      extPLbl[i] = p;
    }
    (*W) = matlib_IncrementalPotential(*IDMater,*IDModel,extPLbl,extPVal,*nExtP,
                                       grad0,flux0,intV0,grad1,flux1,intV1,
                                       *dTime,K,*upd,*tgt);
  }

  // compute material tangents (without updating)
  void FORTRAN(f_matlib_compute_tangent)(int *IDMater,int *IDModel,
                                         char extPLbls[],int len[],int *ld,
                                         double extPVal[],int *nExtP,
                                         double grad0[],double flux0[],double intV0[],
                                         double grad1[],double flux1[],double intV1[],
                                         double *dTime,double *K) {
    int i; char *extPLbl[N_EXT_PAR_MAX],*p;
    for (i=0,p=extPLbls; i < (*nExtP); i++,p+=(*ld)) {
      p[len[i]] = '\0';
      extPLbl[i] = p;
    }
    matlib_ComputeTangent(*IDMater,*IDModel,extPLbl,extPVal,*nExtP,grad0,flux0,intV0,
                          grad1,flux1,intV1,*dTime,K);
  }

  // compute material tangents by numerical perturbation
  void FORTRAN(f_matlib_compute_numerical_tangent)(int *IDMater,int *IDModel,
                                                   char extPLbls[],int len[],int *ld,
                                                   double extPVal[],int *nExtP,
                                                   double grad0[],double flux0[],double intV0[],
                                                   double grad1[],double flux1[],double intV1[],
                                                   double *dTime,double *K) {
    int i; char *extPLbl[N_EXT_PAR_MAX],*p;
    for (i=0,p=extPLbls; i < (*nExtP); i++,p+=(*ld)) {
      p[len[i]] = '\0';
      extPLbl[i] = p;
    }
    matlib_ComputeNumericalTangent(*IDMater,*IDModel,extPLbl,extPVal,*nExtP,grad0,flux0,intV0,
                                   grad1,flux1,intV1,*dTime,K);
  }


  /*********************
   * MaterialCriterion *
   *********************/

  // create new material criterion
  void FORTRAN(f_matlib_new_material_criterion)(int *ID,char key[],int *len,int *dim) {
    key[*len] = '\0';
    matlib_NewMaterialCriterion(*ID,key,*dim);
  }

  // check material data vs. criterion
  void FORTRAN(f_matlib_check_criterion_properties)(int *IDMater,int *IDCriter,
                                                    char outputFileName[],int *len) {
    if (*len > 0) {
      outputFileName[*len] = '\0';
      matlib_CheckCriterionProperties(*IDMater,*IDCriter,outputFileName);
    }
    else 
      matlib_CheckCriterionProperties(*IDMater,*IDCriter,0);
  }

  // rotate material properties
  void FORTRAN(f_matlib__rotate_criterion_properties)(int *IDMater,int *IDCriter,
                                                      double axis1[],double axis2[]) {
    matlib_RotateCriterionProperties(*IDMater,*IDCriter,axis1,axis2);
  }

  // update material properties
  void FORTRAN(f_matlib_update_criterion_properties)(int *IDMater,int *IDCriter,
                                                     char extPLbls[],int len[],int *ld,
                                                     double extPVal[],int *nExtP) {
    int i; char *extPLbl[N_EXT_PAR_MAX],*p;
    for (i=0,p=extPLbls; i < (*nExtP); i++,p+=(*ld)) {
      p[len[i]] = '\0';
      extPLbl[i] = p;
    }
    matlib_UpdateCriterionProperties(*IDMater,*IDCriter,extPLbl,extPVal,*nExtP);
  }

  // size of external variables
  void FORTRAN(f_matlib_n_external_variables_criterion)(int *ID,int *nExt) {
    (*nExt) = matlib_NExtVarCriterion(*ID);
  }

  // size of internal variables
  void FORTRAN(f_matlib_n_internal_variables_criterion)(int *ID,int *nInt) {
    (*nInt) = matlib_NIntVarCriterion(*ID);
  }

  // evaluate material criterion
  void FORTRAN(f_matlib_evaluate_criterion)(int *IDMater,int *IDCriter,
                                            char extPLbls[],int len[],int *ld,
                                            double extPVal[],int *nExtP,
                                            double grad[],double flux[],double intV[],
                                            double *K,double *dTime,double *value) {
    int i; char *extPLbl[N_EXT_PAR_MAX],*p;
    for (i=0,p=extPLbls; i < (*nExtP); i++,p+=(*ld)) {
      p[len[i]] = '\0';
      extPLbl[i] = p;
    }
    (*value) = matlib_EvaluateCriterion(*IDMater,*IDCriter,extPLbl,extPVal,*nExtP,
                                        grad,flux,intV,K,*dTime);
  }
}

