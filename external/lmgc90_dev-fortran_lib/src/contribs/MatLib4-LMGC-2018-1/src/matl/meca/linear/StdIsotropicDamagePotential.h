/*
 *  $Id: StdIsotropicDamagePotential.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_STD_ISOTROPIC_DAMAGE_POTENTIAL_H
#define ZORGLIB_MATL_MECA_LINEAR_STD_ISOTROPIC_DAMAGE_POTENTIAL_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/meca/linear/ElasticIsotropicDamage.h>
#include <matl/meca/linear/IsotropicElasticPotential.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for "standard" isotropic damage pseudo-potentials,
 * based on a power-law.
 */
class StdIsotropicDamagePotential : virtual public IsotropicDamagePotential {
  
 public:
  
  // default constructor
  StdIsotropicDamagePotential() {}
  
  // copy constructor
  StdIsotropicDamagePotential(const StdIsotropicDamagePotential&) {}

  // destructor
  virtual ~StdIsotropicDamagePotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of internal parameters
  unsigned int nIntPar() const {return 0;}
  
  // dissipated energy
  double dissipatedEnergy(const MaterialProperties&,const ParameterSet&,
                          const MatLibArray&,MatLibArray&,double,double,
                          double&,double&,double&,double&,double&,
                          bool,bool);
};


/**
 * Implementations of the model.
 */
class IsotropicElasticDamage3D : public ElasticIsotropicDamage<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicElasticDamage3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>()),
    ElasticIsotropicDamage<TensorAlgebra3D>(new StdIsotropicDamagePotential()) {}
  
  // copy constructor
  IsotropicElasticDamage3D(const IsotropicElasticDamage3D& src) 
  : Elasticity<TensorAlgebra3D>(src),
    ElasticIsotropicDamage<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicElasticDamage3D() {}
};
class IsotropicElasticDamage2D : public ElasticIsotropicDamage<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicElasticDamage2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>()),
    ElasticIsotropicDamage<TensorAlgebra2D>(new StdIsotropicDamagePotential()) {}
  
  // copy constructor
  IsotropicElasticDamage2D(const IsotropicElasticDamage2D& src) 
  : Elasticity<TensorAlgebra2D>(src),
    ElasticIsotropicDamage<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicElasticDamage2D() {}
};
class IsotropicElasticDamage1D : public ElasticIsotropicDamage<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicElasticDamage1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>()),
    ElasticIsotropicDamage<TensorAlgebra1D>(new StdIsotropicDamagePotential()) {}
  
  // copy constructor
  IsotropicElasticDamage1D(const IsotropicElasticDamage1D& src) 
  : Elasticity<TensorAlgebra1D>(src),
    ElasticIsotropicDamage<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicElasticDamage1D() {}
};

/**
 * The associated model builder
 */
class IsotropicElasticDamageBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicElasticDamageBuilder();
  
  // the instance
  static IsotropicElasticDamageBuilder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~IsotropicElasticDamageBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
