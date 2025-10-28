/*
 *  $Id: GeneralHenckyPotential.h 237 2017-06-06 09:13:56Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_HYPER_GENERAL_HENCKY_POTENTIAL_H
#define ZORGLIB_MATL_MECA_HYPER_GENERAL_HENCKY_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/hyper/HyperElasticity.h>
#include <matl/meca/linear/CubicElasticPotential.h>
#include <matl/meca/linear/IsotropicElasticPotential.h>
#include <matl/meca/linear/OrthotropicElasticPotential.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class describing general hyperelastic Hencky potentials.
 */
template <class ALG>
class GeneralHenckyPotential : virtual public HyperElasticity<ALG>::Potential {
  
 public:

  typedef typename ALG::SymTensor::TYPE  SYM_TENSOR;
  typedef typename ALG::SymTensor4::TYPE SYM_TENSOR4;
  
 protected:
    
  // counter
  unsigned int *count;
  
  // linear elastic potential
  typename Elasticity<ALG>::Potential *potential;
  
  // flag for linearization of strains
  bool linearize;
  
 public:

  // constructor
  GeneralHenckyPotential(typename Elasticity<ALG>::Potential& p,bool l = false) {
    count = new unsigned int(1);
    potential = &p;
    linearize = l;
  }
  
  // copy constructor
  GeneralHenckyPotential(const GeneralHenckyPotential& src) {
    count = src.count;
    ++(*count);
    potential = src.potential;
    linearize = src.linearize;
  }

  // destructor
  virtual ~GeneralHenckyPotential() {
    if (--(*count) != 0) return;
    delete count;
    if (potential) delete potential;
  }
  
  // check if strains are linearized
  bool isLinearized() const {return linearize;}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException) {
    if (os) (*os) << "\n\t***General Hencky potential***" << std::endl;
    if (os && linearize) (*os) << "\t (linearized strains)" << std::endl;
    potential->checkProperties(material,os);
  }
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties& material,const Rotation& R) {
    potential->rotateProperties(material,R);
  }
  
  // compute stored energy
  double storedEnergy(const MaterialProperties& material,
                      const ParameterSet& extPar,
                      const SYM_TENSOR& C,SYM_TENSOR& S,
                      SYM_TENSOR4& M,bool first,bool second) {

    // compute logarithmic strain tensor
    SYM_TENSOR eps,dLogC[SYM_TENSOR::MEMSIZE];
    SYM_TENSOR d2LogC[SYM_TENSOR::MEMSIZE][SYM_TENSOR::MEMSIZE];
    if (!linearize)
      eps = 0.5*covariant(log(C,dLogC,d2LogC,
                              first || second,second));
    else
      eps = 0.5*(covariant(C)-SYM_TENSOR::identity());

    // compute elastic energy and stresses
    SYM_TENSOR sig;
    SYM_TENSOR4 K;
    double W = potential->storedEnergy(material,extPar,eps,sig,K,
                                       first || (second && !linearize),second);

    if (!linearize) {
      
      // take into account geometrical terms
      unsigned int ij,kl,sz = SYM_TENSOR::MEMSIZE;
      
      if (first) {
        for (ij=0; ij < sz; ij++) S[ij] = innerProd2(sig,dLogC[ij]);
      }
      
      if (second) {
        SYM_TENSOR *p = *d2LogC;
        for (ij=0; ij < sz; ij++)
          for (kl=0; kl < sz; kl++, p++)
            M[ij][kl] = innerProd2(dLogC[ij],innerProd2(K,dLogC[kl]))
                       +2*innerProd2(sig,*p);
      }
    }
    else {
      if (first) S = sig;
      if (second) M = K;
    }
    
    return W;
  }
};


/**
 * Implementations of the model: isotropic hyperelasticity.
 */
class IsotropicHyperElasticity3D : public HyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicHyperElasticity3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
          new GeneralHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicElasticPotential<TensorAlgebra3D>())),
          eos) {}
  
  // copy constructor
  IsotropicHyperElasticity3D(const IsotropicHyperElasticity3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicHyperElasticity3D() {}
};
class IsotropicHyperElasticity2D : public HyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicHyperElasticity2D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra2D>(
          new GeneralHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicElasticPotential<TensorAlgebra2D>())),
          eos) {}
  
  // copy constructor
  IsotropicHyperElasticity2D(const IsotropicHyperElasticity2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicHyperElasticity2D() {}
};
class IsotropicHyperElasticity1D : public HyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicHyperElasticity1D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra1D>(
          new GeneralHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicElasticPotential<TensorAlgebra1D>())),
          eos) {}
  
  // copy constructor
  IsotropicHyperElasticity1D(const IsotropicHyperElasticity1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicHyperElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicHyperElasticityBuilder : public ModelBuilder {

 private:
  
  // constructor
  IsotropicHyperElasticityBuilder();

  // the instance
  static IsotropicHyperElasticityBuilder const* BUILDER;

 public:
  
  // destructor
  virtual ~IsotropicHyperElasticityBuilder() {}

  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementations of the model: St-Venant--Kirchhoff hyperelasticity.
 */
class StVenantKirchhoffHyperElasticity3D : public HyperElasticity<TensorAlgebra3D> {
  
public:
  
  // constructor
  StVenantKirchhoffHyperElasticity3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
          new GeneralHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicElasticPotential<TensorAlgebra3D>()),true),
          eos) {}
  
  // copy constructor
  StVenantKirchhoffHyperElasticity3D(const StVenantKirchhoffHyperElasticity3D& src)
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~StVenantKirchhoffHyperElasticity3D() {}
};
class StVenantKirchhoffHyperElasticity2D : public HyperElasticity<TensorAlgebra2D> {
  
public:
  
  // constructor
  StVenantKirchhoffHyperElasticity2D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra2D>(
          new GeneralHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicElasticPotential<TensorAlgebra2D>()),true),
          eos) {}
  
  // copy constructor
  StVenantKirchhoffHyperElasticity2D(const StVenantKirchhoffHyperElasticity2D& src)
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~StVenantKirchhoffHyperElasticity2D() {}
};
class StVenantKirchhoffHyperElasticity1D : public HyperElasticity<TensorAlgebra1D> {
  
public:
  
  // constructor
  StVenantKirchhoffHyperElasticity1D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra1D>(
          new GeneralHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicElasticPotential<TensorAlgebra1D>()),true),
          eos) {}
  
  // copy constructor
  StVenantKirchhoffHyperElasticity1D(const StVenantKirchhoffHyperElasticity1D& src)
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~StVenantKirchhoffHyperElasticity1D() {}
};

/**
 * The associated model builder
 */
class StVenantKirchhoffHyperElasticityBuilder : public ModelBuilder {
  
private:
  
  // constructor
  StVenantKirchhoffHyperElasticityBuilder();
  
  // the instance
  static StVenantKirchhoffHyperElasticityBuilder const* BUILDER;
  
public:
  
  // destructor
  virtual ~StVenantKirchhoffHyperElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementations of the model: cubic hyperelasticity.
 */
class CubicHyperElasticity3D : public HyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  CubicHyperElasticity3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
          new GeneralHenckyPotential<TensorAlgebra3D>(
                    *(new CubicElasticPotential<TensorAlgebra3D>())),eos) {}
  
  // copy constructor
  CubicHyperElasticity3D(const CubicHyperElasticity3D& src) 
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CubicHyperElasticity3D() {}
};
class CubicHyperElasticity2D : public HyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  CubicHyperElasticity2D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra2D>(
          new GeneralHenckyPotential<TensorAlgebra2D>(
                    *(new CubicElasticPotential<TensorAlgebra2D>())),eos) {}
  
  // copy constructor
  CubicHyperElasticity2D(const CubicHyperElasticity2D& src) 
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CubicHyperElasticity2D() {}
};
class CubicHyperElasticity1D : public HyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  CubicHyperElasticity1D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra1D>(
          new GeneralHenckyPotential<TensorAlgebra1D>(
                    *(new CubicElasticPotential<TensorAlgebra1D>())),eos) {}
  
  // copy constructor
  CubicHyperElasticity1D(const CubicHyperElasticity1D& src) 
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CubicHyperElasticity1D() {}
};

/**
 * The associated model builder
 */
class CubicHyperElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CubicHyperElasticityBuilder();
  
  // the instance
  static CubicHyperElasticityBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~CubicHyperElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

/**
 * Implementations of the model: orthotropic hyperelasticity.
 */
class OrthotropicHyperElasticity3D : public HyperElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  OrthotropicHyperElasticity3D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra3D>(
          new GeneralHenckyPotential<TensorAlgebra3D>(
                    *(new OrthotropicElasticPotential<TensorAlgebra3D>())),eos) {}
  
  // copy constructor
  OrthotropicHyperElasticity3D(const OrthotropicHyperElasticity3D& src)
  : HyperElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~OrthotropicHyperElasticity3D() {}
};
class OrthotropicHyperElasticity2D : public HyperElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  OrthotropicHyperElasticity2D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra2D>(
          new GeneralHenckyPotential<TensorAlgebra2D>(
                    *(new OrthotropicElasticPotential<TensorAlgebra2D>())),eos) {}
  
  // copy constructor
  OrthotropicHyperElasticity2D(const OrthotropicHyperElasticity2D& src)
  : HyperElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~OrthotropicHyperElasticity2D() {}
};
class OrthotropicHyperElasticity1D : public HyperElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  OrthotropicHyperElasticity1D(EOS *eos = 0)
  : HyperElasticity<TensorAlgebra1D>(
          new GeneralHenckyPotential<TensorAlgebra1D>(
                    *(new OrthotropicElasticPotential<TensorAlgebra1D>())),eos) {}
  
  // copy constructor
  OrthotropicHyperElasticity1D(const OrthotropicHyperElasticity1D& src)
  : HyperElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~OrthotropicHyperElasticity1D() {}
};

/**
 * The associated model builder
 */
class OrthotropicHyperElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  OrthotropicHyperElasticityBuilder();
  
  // the instance
  static OrthotropicHyperElasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~OrthotropicHyperElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
