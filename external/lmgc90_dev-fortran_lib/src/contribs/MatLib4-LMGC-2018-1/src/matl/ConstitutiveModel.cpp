/*
 *  $Id: ConstitutiveModel.cpp 194 2015-12-16 21:13:27Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "ConstitutiveModel.h"

// std C library
#include <cmath>
#include <cstring>
// Visual Studio specific
#if defined(_WIN32) || defined(_WIN64)
#define strcasecmp _stricmp
#endif
// std C++ library
#include <fstream>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


// check consistency of material properties
void ConstitutiveModel::checkProperties(MaterialProperties& material,
                                        const char* oFileName) 
 throw (FileException, InvalidPropertyException, NoSuchPropertyException) {
   
  // open output file
  if (oFileName) {
    if (strcasecmp(oFileName,"null")) {
      std::ofstream file(oFileName,std::ofstream::out | std::ofstream::app);
      if (!file.is_open()) {
        std::string msg("cannot open output file: ");
        msg += oFileName;
        throw FileException(msg);
      }
      checkProperties(material,&file);
    }
    else
      checkProperties(material,(std::ostream*)0);
  }
  else
    checkProperties(material,&std::cout);
}

// utility function
unsigned int ConstitutiveModel::dimension(VariableType type,unsigned int sz) {
  switch (type) {
    case TYPE_VECTOR:
      return sz;
      break;
    case TYPE_SYM_TENSOR:
    case TYPE_TENSOR:
      return 3;
      break;
    case TYPE_STD_SYM_TENSOR:
      if (sz == 1)
        return 1;
      else if (sz == 3)
        return 2;
      else if (sz == 6)
        return 3;
      break;
    case TYPE_STD_TENSOR:
      if (sz == 1)
        return 1;
      else if (sz == 4)
        return 2;
      else if (sz == 9)
        return 3;
      break;
    default:
      return 0;
      break;
  }
  return 0;
}

// compute material tangents by numerical perturbation
void ConstitutiveModel::computeNumericalTangent(const MaterialProperties& mater,
                                                const ParameterSet& extPar,
                                                const MaterialState& state0,
                                                const MaterialState& state1,
                                                double dTime,MatLibMatrix& tgt) {
  // create buffer material state
  MaterialState state;
  initState(mater,state);

  // perturbation parameters
  const double PERTU = 1.0e-8;
  double coef0 = 0.5/PERTU;
  
  unsigned int sz = nExtVar();
  MatLibArray sigp(sz),sigm(sz);
  MatLibMatrix dummy(sz);

  // loop on gradient array components
  state = state1;
  for (unsigned n=0; n < sz; n++) {

    // positive perturbation
    double valRef = std::fabs(state.grad[n]);
    if (valRef > 1.e-8)
      state.grad[n] += valRef*PERTU;
    else
      state.grad[n] += valRef;
    updateState(mater,extPar,state0,state,dTime,dummy,false);
    sigp = state.flux;
    
    // negative perturbation
    if (valRef > 1.e-8)
      state.grad[n] -= 2*valRef*PERTU;
    else
      state.grad[n] -= 2*PERTU;
    updateState(mater,extPar,state0,state,dTime,dummy,false);
    sigm = state.flux;
    
    // compute derivatives
    double coef;
    if (valRef > 1.e-8)
      coef = coef0/valRef;
    else
      coef = coef0;
    
    for (unsigned int m=0; m < sz; m++)
      tgt[m][n] = coef*(sigp[m]-sigm[m]);
    
    // restore state
    if (valRef > 1.e-8)
      state.grad[n] += valRef*PERTU;
    else
      state.grad[n] += PERTU;
  }
}
