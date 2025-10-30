/*
 *  $Id: MatlUniaxialTest.cpp 197 2016-02-26 10:45:52Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */

// std C library
#include <cmath>
// std C++ library
#include <cstring>
#include <fstream>
// STL
#include <vector>
// local
#include <data/Rotation3D.h>
#include <math/TensorAlgebra.h>
#include <math/Vector3D.h>
#include <matl/ModelDictionary.h>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/* Utility */
void cauchy(MatLibArray& sig,const MatLibArray& P,const MatLibArray& F,unsigned int d) {
  if (d == 1)
    TensorAlgebra1D::PK1ToCauchy(P,F,sig);
  else if (d == 2)
    TensorAlgebra2D::PK1ToCauchy(P,F,sig);
  else if (d == 3)
    TensorAlgebra3D::PK1ToCauchy(P,F,sig);
}


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
  
  // create output file
  std::ostream *output;
  if (argc < 2)
    output = &std::cout;
  else {
    char oFile[256];
    std::strcpy(oFile,argv[1]);
    char* ext = strrchr(oFile,'.');
    if (ext)
      std::strncpy(ext,".plt",4);
    else
      std::strcat(oFile,".plt");
    output = new std::ofstream(oFile,std::ofstream::out|std::ofstream::trunc);
    if (!dynamic_cast<std::ofstream*>(output)->is_open()) {
      std::cerr << "ERROR: could not open file ";
      std::cerr << oFile << "." << std::endl;
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
  MaterialState state0,state1;
  state0.grad.resize(nExtVar);
  state1.grad.resize(nExtVar);
  state0.flux.resize(nExtVar);
  state1.flux.resize(nExtVar);
  
  MatLibMatrix M(nExtVar);
  
  MatLibArray tau;
  if (material->typeExtVar(0) == ConstitutiveModel::TYPE_TENSOR)
    tau.resize(dimSpace*dimSpace+3-dimSpace);

  unsigned int nIntVar = material->nIntVar();
  state0.internal.resize(nIntVar);
  state1.internal.resize(nIntVar);

  // initialize state
  material->initState(data,state0);
  material->initState(data,state1);
  
  // read external parameters
  ParameterSet external;
  unsigned int nExtPar;
  (*input) >> nExtPar;
  double value;
  for (unsigned int i=0; i < nExtPar; i++) {
    (*input) >> key >> value;
    external[key.c_str()] = value;
  }
  
  // read time stepping
  unsigned int nSeg;
  (*input) >> nSeg;
  MatLibArray tSeg(nSeg+1),tStp(nSeg),eSeg(nSeg);
  tSeg[0] = 0.0;
  for (unsigned int i=0; i < nSeg; i++)
    (*input) >> tSeg[i+1] >> tStp[i] >> eSeg[i];
  
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
  
  // write initial state
  unsigned int nExtVarBundled = material->nExtVarBundled();
  unsigned int nIntVarBundled = material->nIntVarBundled();
  (*output) << 0 << "\t" << 0. << "\t" << state0.grad[0] << "\t" << state0.flux[0];
  if (material->typeExtVar(0) == ConstitutiveModel::TYPE_TENSOR) {
    cauchy(tau,state0.flux,state0.grad,dimSpace);
    (*output) << "\t" << tau[0];
  }
  if (nExtVarBundled > 1 && material->typeExtVar(nExtVarBundled-1) == ConstitutiveModel::TYPE_SCALAR)
    (*output) << "\t" << state0.grad[nExtVar-1];
  if (material->getIntVar("EPLS") < nIntVarBundled)
    (*output) << "\t" << state0.internal[material->indexIntVar(material->getIntVar("EPLS"))];
  if (material->getIntVar("DAMG") < nIntVarBundled)
    (*output) << "\t" << state0.internal[material->indexIntVar(material->getIntVar("DAMG"))];
  if (material->getIntVar("TEMP") < nIntVarBundled)
    (*output) << "\t" << state0.internal[material->indexIntVar(material->getIntVar("TEMP"))];
  if (material->getIntVar("VFRC") < nIntVarBundled)
    (*output) << "\t" << state0.internal[material->indexIntVar(material->getIntVar("VFRC"))];
  (*output) << std::endl;
  
  // time loop
  unsigned int iStep = 0;
  for (unsigned int n=0; n < nSeg; n++) {
    double tStep = tStp[n];
    double time1 = tSeg[n];
    while (time1 < tSeg[n+1]) {
      
      // compute new time
      double time0 = time1;
      time1 = time0+tStep;
      if (time1 > tSeg[n+1]) {
        time1 = tSeg[n+1];
        tStep = time1-time0;
      }
      if (tStep < 1.e-8*(tSeg[n+1]-tSeg[n])) break;
      std::cout << "Step " << ++iStep << " - Time=" << time1 << " - dTime=" << tStep << std::endl;
      
      // compute new axial strain
      state0 = state1;
      double eps11 = state1.grad[0]+eSeg[n]*tStep;
      state1.grad[0] = eps11;
      
      MatLibArray eps(state1.grad,nExtVar-1,1);
      MatLibArray sig(state1.flux,nExtVar-1,1);
      MatLibMatrix Mred(M,nExtVar-1,1);
      
      // solve transverse equilibrium
      static const unsigned int ITMAX=25;
      static const double PRECISION=1.e-16;
      static const double TOLERANCE=1.e-8;
      double norm0 = 0.0;
      for (unsigned int it = 0; it < ITMAX; it++) {
        
        // update state
        double W;
        if (stdMaterial)
          W = stdMaterial->incrementalPotential(data,external,state0,state1,
                                                tStep,M,true,true);
        else
          material->updateState(data,external,state0,state1,tStep,M,true);
                            
        // check norm
        double norm = normL2(sig);
        std::cout << "\tIter " << it << " - Norm=" << norm << std::endl;
        if (norm0 == 0.0) norm0 = norm;
        if (norm < TOLERANCE*norm0 || norm < PRECISION) break;
                                
        // compute correction
        MatLibArray dEps(nExtVar-1);
        try {
          Mred.solve(dEps,sig,material->isStandard());
        }
        catch (SingularMatrixException) {
          std::cerr << "ERROR: singular matrix" << std::endl;
          return -1;
        }

        eps -= dEps;
      }
                                  
      // write solution
      (*output) << iStep << "\t" << time1 << "\t" << eps11 << "\t" << state1.flux[0];
      if (material->typeExtVar(0) == ConstitutiveModel::TYPE_TENSOR) {
        cauchy(tau,state1.flux,state1.grad,dimSpace);
        (*output) << "\t" << tau[0];
      }
      if (nExtVarBundled > 1 && material->typeExtVar(nExtVarBundled-1) == ConstitutiveModel::TYPE_SCALAR)
        (*output) << "\t" << state1.grad[nExtVar-1];
      if (material->getIntVar("EPLS") < nIntVarBundled)
        (*output) << "\t" << state1.internal[material->indexIntVar(material->getIntVar("EPLS"))];
      if (material->getIntVar("DAMG") < nIntVarBundled)
        (*output) << "\t" << state1.internal[material->indexIntVar(material->getIntVar("DAMG"))];
      if (material->getIntVar("TEMP") < nIntVarBundled)
        (*output) << "\t" << state1.internal[material->indexIntVar(material->getIntVar("TEMP"))];
      if (material->getIntVar("VFRC") < nIntVarBundled)
        (*output) << "\t" << state1.internal[material->indexIntVar(material->getIntVar("VFRC"))];
      (*output) << std::endl;
    }
  }
  
  return 0;
}
