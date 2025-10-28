/*
 *  $Id: MatlConsistencyTest.cpp 174 2015-08-25 19:44:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */

// std C library
#include <cmath>
// std C++ library
#include <fstream>
// local
#include <data/Rotation3D.h>
#include <math/Vector3D.h>
#include <matl/ModelDictionary.h>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Tests the consistency of a material model.
 */
int main(int argc,char *argv[]) {
  
  // read input file name
  std::istream *input;
  if (argc < 2)
    input = &std::cin;
  else {
    input = new std::ifstream(argv[1]);
    if (!dynamic_cast<std::ifstream*>(input)->is_open()) {
      std::cerr << "ERROR: could not open file ";
      std::cerr << argv[1] << "." << std::endl;
      return -1;
    }
  }
  
  // setup environment
  unsigned int dimSpace;
  (*input) >> dimSpace;
  
  // read material
#ifdef FULL_DEBUG
  ModelDictionary::list(std::cout);
#endif
  std::string key;
  (*input) >> key;
  ConstitutiveModel* material;
  try {
    material = ModelDictionary::get(key).build(dimSpace);
  }
  catch (NoSuchModelException e) {
    std::cerr << "ERROR: invalid model keyword (" << e.mesg() << ")" << std::endl;
    return -1;
  }
  StandardMaterial *stdMaterial = 0;
  if (material->isStandard()) stdMaterial = dynamic_cast<StandardMaterial*>(material);
    
  // read material properties
  MaterialProperties data;
  (*input) >> key;
  data.readFrom(key.c_str());
  material->checkProperties(data,&std::cout);

  // allocate work arrays
  unsigned int nExtVar = material->nExtVar();
  MaterialState state0,state;
  state0.grad.resize(nExtVar);
  state.grad.resize(nExtVar);
  state0.flux.resize(nExtVar);
  state.flux.resize(nExtVar);
  
  MatLibMatrix M(nExtVar);
  
  unsigned int nIntVar = material->nIntVar();
  state0.internal.resize(nIntVar);
  state.internal.resize(nIntVar);

  // initialize state
  material->initState(data,state0);
  
  // read time step size
  double dTime;
  (*input) >> dTime;
  
  // read external parameters
  ParameterSet external;
  unsigned int nExtPar;
  (*input) >> nExtPar;
  double value;
  for (unsigned int i=0; i < nExtPar; i++) {
    (*input) >> key >> value;
    external[key.c_str()] = value;
  }
  
  // read gradient increment
  for (unsigned int i=0; i < nExtVar; i++) (*input) >> state.grad[i];
    
  // orientation
  if ((*input) >> value) {
    Vector3D e1,e2,e3;
    e1[0] = value;
    (*input) >> e1[1] >> e1[2];
    (*input) >> e2[0] >> e2[1] >> e2[2];
    if (std::fabs(e1*e2) >= 1.e-12) {
      std::cerr << "ERROR: orientation vectors are not orthogonal" << std::endl;
      return -1;
    }
    e3 = crossProd(e1,e2);
    double norm;
    norm = normL2(e1);
    e1 /= norm;
    norm = normL2(e2);
    e2 /= norm;
    norm = normL2(e3);
    e3 /= norm;
    MatLibMatrix RMat(3);
    for (unsigned int i=0; i < 3; i++) {
      RMat[0][i] = e1[i];
      RMat[1][i] = e2[i];
      RMat[2][i] = e3[i];
    }
    Rotation3D R(RMat);
    material->rotateProperties(data,R);
  }

  // compute new state
  double W;
  if (stdMaterial) {
    W = stdMaterial->incrementalPotential(data,external,state0,state,
                                          dTime,M,true,true);
    std::cout << "\nIncremental energy = " << W << std::endl;
  }
  else
    material->updateState(data,external,state0,state,dTime,M,true);
  
  std::cout << "\nStress tensor = \n\t" << state.flux << std::endl;
  
  std::cout << "\nTangent matrix = \n";
  for (unsigned int i=0; i < nExtVar; i++) {
    std::cout << "\t";
    for (unsigned int j=0; j < nExtVar; j++) std::cout << " " << M[i][j];
    std::cout << std::endl;
  }
  
  if (nIntVar > 0)
    std::cout << "\nInternal variables =\n" << state.internal << std::endl;
  else
    std::cout << "\nNo internal variables" << std::endl;
  
  double pertu = 1.0e-8;
  double coef0 = 0.5/pertu;
  double Wp=0.0,Wm=0.0;
  MatLibArray sigp(nExtVar),sigm(nExtVar),sigdiff(nExtVar);
  MatLibMatrix Mdiff(nExtVar);
  std::cout << "\nComputing: ";
  for (unsigned int k=0; k < nExtVar; k++) {
    
    std::cout << k;
    
    // positive perturbation
    double valRef = std::fabs(state.grad[k]);
    if (valRef > 1.e-8)
      state.grad[k] += valRef*pertu;
    else
      state.grad[k] += valRef;
    if (stdMaterial)
      Wp = stdMaterial->incrementalPotential(data,external,state0,state,
                                             dTime,M,true,false);
    else
      material->updateState(data,external,state0,state,dTime,M,false);
    sigp = state.flux;
    std::cout << ".";
    
    // negative perturbation
    if (valRef > 1.e-8)
      state.grad[k] -= 2*valRef*pertu;
    else
      state.grad[k] -= 2*pertu;
    if (stdMaterial)
      Wm = stdMaterial->incrementalPotential(data,external,state0,state,
                                             dTime,M,true,false);
    else
      material->updateState(data,external,state0,state,dTime,M,false);
    sigm = state.flux;
    std::cout << ".";
    
    // compute derivatives
    double coef;
    if (valRef > 1.e-8)
      coef = coef0/valRef;
    else
      coef = coef0;
    
    if (stdMaterial) sigdiff[k] = coef*(Wp-Wm);
    
    for (unsigned int l=0; l < nExtVar; l++) 
      Mdiff[l][k] = coef*(sigp[l]-sigm[l]);
    
    // restore F2
    if (valRef > 1.e-8)
      state.grad[k] += valRef*pertu;
    else
      state.grad[k] += pertu;
  }
  std::cout << std::endl << std::endl;
  
  if (stdMaterial)
    std::cout << "\nStress tensor (numerical) =\n\t" << sigdiff << std::endl;
  
  std::cout << "\nTangent matrix (numerical)= " << std::endl;
  for (unsigned int i=0; i < nExtVar; i++) {
    std::cout << "\t";
    for (unsigned int j=0; j < nExtVar; j++) std::cout << " " << Mdiff[i][j];
    std::cout << std::endl;
  }
  
  return 0;
}
