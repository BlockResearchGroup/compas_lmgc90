/*
 *  $Id: StdMatlBifurcationCriterion.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_BIFURCATION_H
#define ZORGLIB_MATL_MECA_HYPER_BIFURCATION_H

// config
#include <matlib_macros.h>

// local
#ifdef USE_LAPACK
#include <math/lapack.h>
#else
#include <math/eigsym.h>
#endif
#include <matl/CriterionDictionary.h>
#include <matl/meca/hyper/HyperElasticity.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Bifurcation criterion for standard (finite strain) mechanical models.
 */
template <class ALG>
class StdMatlBifurcationCriterion : virtual public MaterialCriterion {

 protected:

  // instance counter
  unsigned int *count;
  
 public:

  // constructor
  StdMatlBifurcationCriterion() {
    count = new unsigned int(1);
  }

  // copy constructor
  StdMatlBifurcationCriterion(const StdMatlBifurcationCriterion& src) {
    count = src.count;
    (*count)++;
  }
  
  // destructor
  virtual ~StdMatlBifurcationCriterion() {
    if (--(*count) > 0) return;
    delete count;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\nStandard material bifurcation criterion." << std::endl;
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
  unsigned int nExtVar() const {return HyperElasticity<ALG>::TENSOR::MEMSIZE;}
  
  // how many internal variables ?
  unsigned int nIntVar() const {return 0;}
  
  // evaluate criterion
  double evaluateCriterion(const MaterialProperties& material,const ParameterSet& extPar,
                           const MaterialState& state,const MatLibMatrix& M,double time) {

    // store material tangent in column-wise upper triangular (or row-wise lower triangular)
    static const unsigned int memsize = HyperElasticity<ALG>::TENSOR::MEMSIZE
                                       *(HyperElasticity<ALG>::TENSOR::MEMSIZE+1)/2;
    unsigned int i,j,ij;
    double A[memsize];
    for (i=0, ij=0; i < HyperElasticity<ALG>::TENSOR::MEMSIZE; i++)
      for (j=0; j <= i; j++, ij++)
        A[ij] = M[i][j];

    // compute eigenvalues of material tangent M
    double eigVal[HyperElasticity<ALG>::TENSOR::MEMSIZE];
    double V[HyperElasticity<ALG>::TENSOR::MEMSIZE*HyperElasticity<ALG>::TENSOR::MEMSIZE];
    int test;
#ifndef USE_LAPACK
    double wrk1[HyperElasticity<ALG>::TENSOR::MEMSIZE],wrk2[HyperElasticity<ALG>::TENSOR::MEMSIZE];
    test = jacobi(HyperElasticity<ALG>::TENSOR::MEMSIZE,HyperElasticity<ALG>::TENSOR::MEMSIZE,
                  A,eigVal,V,wrk1,wrk2);
#else
    char jobz='V',uplo='U';
    LAPACK_INTEGER size=HyperElasticity<ALG>::TENSOR::MEMSIZE,ierr;
    LAPACK_DOUBLE work[HyperElasticity<ALG>::TENSOR::MEMSIZE*HyperElasticity<ALG>::TENSOR::MEMSIZE];
    FORTRAN(dspev)(&jobz,&uplo,&size,A,eigVal,V,&size,work,&ierr);
    test = !ierr;
#endif
    if (!test) throw ZError("eigenvalue analysis failed in StdMatlBifurcationCriterion");
    
    // output results
    double vMin = eigVal[0];
    for (i=1; i < HyperElasticity<ALG>::TENSOR::MEMSIZE; i++)
      if (eigVal[i] < vMin) vMin = eigVal[i];
    return vMin;
  }
};


/**
 * The associated criterion builder.
 */
class StdMatlBifurcationCriterionBuilder : public CriterionBuilder {

 private:
  
  // constructor
  StdMatlBifurcationCriterionBuilder();
  
  // the instance
  static StdMatlBifurcationCriterionBuilder const* BUILDER;
  
 public:
 
  // destructor
  virtual ~StdMatlBifurcationCriterionBuilder() {}
  
  // build model
  MaterialCriterion* build(unsigned int) const;
};


#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
