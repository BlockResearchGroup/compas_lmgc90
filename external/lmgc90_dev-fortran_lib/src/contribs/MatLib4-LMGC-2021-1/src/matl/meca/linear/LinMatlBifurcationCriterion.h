/*
 *  $Id: LinMatlBifurcationCriterion.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_BIFURCATION_H
#define ZORGLIB_MATL_MECA_LINEAR_BIFURCATION_H

// config
#include <matlib_macros.h>

// local
#ifdef USE_LAPACK
#include <math/lapack.h>
#else
#include <math/eigsym.h>
#endif
#include <matl/CriterionDictionary.h>
#include <matl/meca/linear/Elasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Bifurcation criterion for linearized (small-strain) mechanical models.
 */
template <class ALG>
class LinMatlBifurcationCriterion : virtual public MaterialCriterion {

 protected:

  // instance counter
  unsigned int *count;
  
 public:

  // constructor
  LinMatlBifurcationCriterion() {
    count = new unsigned int(1);
  }

  // copy constructor
  LinMatlBifurcationCriterion(const LinMatlBifurcationCriterion& src) {
    count = src.count;
    (*count)++;
  }
  
  // destructor
  virtual ~LinMatlBifurcationCriterion() {
    if (--(*count) > 0) return;
    delete count;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nLinear material bifurcation criterion." << std::endl;
    // no properties to check
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    // no properties to rotate
  }
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties& material,const ParameterSet& extPar) {
    // no properties to update
  }
  
  // how many external variables ?
  unsigned int nExtVar() const {return Elasticity<ALG>::SYM_TENSOR::MEMSIZE;}
  
  // how many internal variables ?
  unsigned int nIntVar() const {return 0;}
  
  // evaluate criterion
  double evaluateCriterion(const MaterialProperties& material,const ParameterSet& extPar,
                           const MaterialState& state,const MatLibMatrix& M,double time) {

    // store material tangent in column-wise upper triangular (or row-wise lower triangular)
    static const unsigned int memsize = Elasticity<ALG>::SYM_TENSOR::MEMSIZE
                                       *(Elasticity<ALG>::SYM_TENSOR::MEMSIZE+1)/2;
    unsigned int i,j,ij;
    double A[memsize];
    for (i=0, ij=0; i < Elasticity<ALG>::SYM_TENSOR::MEMSIZE; i++)
      for (j=0; j <= i; j++, ij++)
        A[ij] = M[i][j];

    // compute eigenvalues of material tangent M
    double eigVal[Elasticity<ALG>::SYM_TENSOR::MEMSIZE];
    double V[Elasticity<ALG>::SYM_TENSOR::MEMSIZE*Elasticity<ALG>::SYM_TENSOR::MEMSIZE];
    int test;
#ifndef USE_LAPACK
    double wrk1[Elasticity<ALG>::SYM_TENSOR::MEMSIZE],wrk2[Elasticity<ALG>::SYM_TENSOR::MEMSIZE];
    test = jacobi(Elasticity<ALG>::SYM_TENSOR::MEMSIZE,Elasticity<ALG>::SYM_TENSOR::MEMSIZE,
                  A,eigVal,V,wrk1,wrk2);
#else
    char jobz='V',uplo='U';
    LAPACK_INTEGER size=Elasticity<ALG>::SYM_TENSOR::MEMSIZE,ierr;
    LAPACK_DOUBLE work[Elasticity<ALG>::SYM_TENSOR::MEMSIZE*Elasticity<ALG>::SYM_TENSOR::MEMSIZE];
    FORTRAN(dspev)(&jobz,&uplo,&size,A,eigVal,V,&size,work,&ierr);
    test = !ierr;
#endif
    if (!test) throw ZError("eigenvalue analysis failed in LinMatlBifurcationCriterion");
    
    // output results
    double vMin = eigVal[0];
    for (i=1; i < Elasticity<ALG>::SYM_TENSOR::MEMSIZE; i++)
      if (eigVal[i] < vMin) vMin = eigVal[i];
    return vMin;
  }
};


/**
 * The associated criterion builder.
 */
class LinMatlBifurcationCriterionBuilder : public CriterionBuilder {

 private:
  
  // constructor
  LinMatlBifurcationCriterionBuilder();
  
  // the instance
  static LinMatlBifurcationCriterionBuilder const* BUILDER;
  
 public:
 
  // destructor
  virtual ~LinMatlBifurcationCriterionBuilder() {}
  
  // build model
  MaterialCriterion* build(unsigned int) const;
};


#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
