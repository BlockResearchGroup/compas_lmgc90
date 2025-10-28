/*
 *  $Id: KirchhoffShell3D.cpp 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "KirchhoffShell3D.h"

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif

/*
 * Methods for class KirchhoffShell3D
 */

// check consistency of material properties
void KirchhoffShell3D::checkProperties(MaterialProperties& material,std::ostream* os) 
 throw (InvalidPropertyException, NoSuchPropertyException) {
  if (os) (*os) << "\nKirchhoff shell material (homogeneous linear elastic) in resultants:\n";
   
  // density
  try {
    double rho = material.getDoubleProperty("MASS_DENSITY");
    if (os) (*os) << "\n\tmass density = " << rho << std::endl;
  }
  catch (NoSuchPropertyException) {
    if (os) (*os) << "\n\tmass density is not defined" << std::endl;
  }
   
  // check potential
  if (potential) potential->checkProperties(material,os);
   
  // shell thickness
  try {
    double t = material.getDoubleProperty("THICKNESS");
    if (t <= 0.e0) {
      if (os) (*os) << "ERROR: shell thickness must be positive." << std::endl;
      throw InvalidPropertyException("shell thickness");
    }
    if (os) (*os) << "\n\tshell thickness = " << t << std::endl;
  }
  catch (NoSuchPropertyException) {
    if (os) (*os) << "\n\tshell thickness is not defined" << std::endl;
  }
}

// self-documenting utilities
ConstitutiveModel::VariableType KirchhoffShell3D::typeExtVar(unsigned int i) const {
  switch (i) {
    case 0:
      return ConstitutiveModel::TYPE_STD_SYM_TENSOR;
      break;
    case 1:
      return ConstitutiveModel::TYPE_STD_SYM_TENSOR;
      break;
    default:
      return ConstitutiveModel::TYPE_NONE;
      break;
  }
}
unsigned int KirchhoffShell3D::indexExtVar(unsigned int i) const {
  switch (i) {
    case 0:
      return 0;
      break;
    case 1:
      return SYM_TENSOR::MEMSIZE;
      break;
    default:
      return 2*SYM_TENSOR::MEMSIZE;
      break;
  }
}
std::string KirchhoffShell3D::labelExtVar(unsigned int i) const {
  switch (i) {
    case 0:
      return "membrane strains";
      break;
    case 1:
      return "curvatures";
      break;
    default:
      return "";
      break;
  }
}
std::string KirchhoffShell3D::labelExtForce(unsigned int i) const {
  switch (i) {
    case 0:
      return "membrane tractions";
      break;
    case 1:
      return "bending moments";
      break;
    default:
      return "";
      break;
  }
}

// self-documenting utilities
unsigned int KirchhoffShell3D::getIntVar(const std::string& str) const {
  if (str == "MNRG")
    return 0;
  else if (str == "BNRG")
    return 1;
  else
    return 2;
}
ConstitutiveModel::VariableType KirchhoffShell3D::typeIntVar(unsigned int i) const {
  switch (i) {
    case 0:
      return ConstitutiveModel::TYPE_SCALAR;
      break;
    case 1:
      return ConstitutiveModel::TYPE_SCALAR;
      break;
    default:
      return ConstitutiveModel::TYPE_NONE;
      break;
  }
}
unsigned int KirchhoffShell3D::indexIntVar(unsigned int i) const {
  switch (i) {
    case 0:
      return 0;
      break;
    case 1:
      return 1;
      break;
    default:
      return 2;
      break;
  }
}
std::string KirchhoffShell3D::labelIntVar(unsigned int i) const {
  switch (i) {
    case 0:
      return "membrane energy";
      break;
    case 1:
      return "flexural energy";
      break;
    default:
      return "";
      break;
  }
}

// compute the incremental potential
double KirchhoffShell3D::incrementalPotential(const MaterialProperties& material,
                                              const ParameterSet& extPar,
                                              const MaterialState& state0,MaterialState& state,
                                              double dTime,MatLibMatrix& M,
                                              bool update,bool tangent) 
 throw (UpdateFailedException) {
   
  // "cast" external variables to membrane and bending parts
  SYM_TENSOR eps(state.grad,0);
  SYM_TENSOR sig(state.flux,0);
   
  SYM_TENSOR chi(state.grad,SYM_TENSOR::MEMSIZE);
  SYM_TENSOR mnt(state.flux,SYM_TENSOR::MEMSIZE);
   
  // get thickness
  double t = material.getDoubleProperty("THICKNESS");
   
  // get material stiffness
  SymTensor4_3D H;
  potential->computeStiffness(material,extPar,H);
   
  // compute plane stress stiffness tensor
  MatLibMatrix HRed(SYM_TENSOR::MEMSIZE);
  double coef = 1.e0/H(2,2,2,2);
  unsigned int i,j,k,l,ij,kl;
  for (i=0, ij=0; i < 2; i++)
    for (j=0; j <= i; j++, ij++)
      for (k=0, kl=0; k < 2; k++)
        for (l=0; l <= k; l++, kl++)
          HRed[ij][kl] = H(i,j,k,l)-H(i,j,2,2)*coef*H(2,2,k,l);
   
  // membrane energy
  MatLibMatrix Hm(SYM_TENSOR::MEMSIZE);
  double Wm = membraneEnergy(HRed,t,eps,sig,Hm,update,tangent);
  state.internal[0] = Wm;
   
  // bending energy
  MatLibMatrix Hb(3);
  double Wb = bendingEnergy(HRed,t,chi,mnt,Hb,update,tangent);
  state.internal[1] = Wb;
   
  // assemble tangents
  if (tangent) {
    M = 0.e0;
     
    // membrane part
    for (unsigned int i=0; i < SYM_TENSOR::MEMSIZE; i++)
      for (unsigned int j=0; j < SYM_TENSOR::MEMSIZE; j++) 
        M(i,j) = Hm(i,j);

    // membrane part
    for (unsigned int i=0; i < SYM_TENSOR::MEMSIZE; i++)
      for (unsigned int j=0; j < SYM_TENSOR::MEMSIZE; j++) 
        M(SYM_TENSOR::MEMSIZE+i,SYM_TENSOR::MEMSIZE+j) = Hb(i,j);
  }
   
  return Wm+Wb-state0.internal[0]-state0.internal[1];
} 

// compute membrane stored energy
double KirchhoffShell3D::membraneEnergy(const MatLibMatrix& H,double t,
                                        const SYM_TENSOR& eps,SYM_TENSOR& sig,
                                        MatLibMatrix& M,bool computeStress,
                                        bool computeTangent) {
  double W;
  MatLibMatrix Hm(SYM_TENSOR::MEMSIZE);
  Hm = t*H;
  
  if (computeStress) {
    sig = Hm*eps;
    W = 0.5*innerProd(sig,eps);
  }
  else {
    W = 0.5*innerProd(eps,Hm*eps);
  }
  
  if (computeTangent) {
    M = Hm;
  }
  
  return W;
}

// compute bending stored energy
double KirchhoffShell3D::bendingEnergy(const MatLibMatrix& H,double t,
                                       const SYM_TENSOR& chi,SYM_TENSOR& mnt,
                                       MatLibMatrix& M,bool computeStress,
                                       bool computeTangent) {
  double W;
  MatLibMatrix Hb(3);
  Hb  = (t*t*t/12)*H;
  
  if (computeStress) {
    mnt = Hb*chi;
    W = 0.5*innerProd(mnt,chi);
  }
  else {
    W = 0.5*innerProd(chi,Hb*chi);
  }
  
  if (computeTangent) {
    M = Hb;
  }
  
  return W;
}


/*
 * Methods for class IsotropicKirchhoffShellBuilder.
 */

// the instance
IsotropicKirchhoffShellBuilder const* IsotropicKirchhoffShellBuilder::BUILDER 
= new IsotropicKirchhoffShellBuilder();

// constructor
IsotropicKirchhoffShellBuilder::IsotropicKirchhoffShellBuilder() {
  ModelDictionary::add("ISOTROPIC_KIRCHHOFF_SHELL",*this);
}

// build model
ConstitutiveModel* IsotropicKirchhoffShellBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new IsotropicKirchhoffShell3D();
      break;
    default:
      return 0;
      break;
  }
}


/*
 * Methods for class OrthotropicKirchhoffShellBuilder.
 */

// the instance
OrthotropicKirchhoffShellBuilder const* OrthotropicKirchhoffShellBuilder::BUILDER 
= new OrthotropicKirchhoffShellBuilder();

// constructor
OrthotropicKirchhoffShellBuilder::OrthotropicKirchhoffShellBuilder() {
  ModelDictionary::add("ORTHOTROPIC_KIRCHHOFF_SHELL",*this);
}

// build model
ConstitutiveModel* OrthotropicKirchhoffShellBuilder::build(unsigned int d) const {
  switch(d) {
    case 3:
      return new OrthotropicKirchhoffShell3D();
      break;
    default:
      return 0;
      break;
  }
}

