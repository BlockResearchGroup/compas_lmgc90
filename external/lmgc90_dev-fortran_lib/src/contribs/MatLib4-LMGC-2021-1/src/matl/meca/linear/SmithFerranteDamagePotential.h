/*
 *  $Id: SmithFerranteDamagePotential.h 245 2017-07-20 12:49:41Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2017, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_SMITH_FERRANTE_DAMAGE_POTENTIAL_H
#define ZORGLIB_MATL_MECA_LINEAR_SMITH_FERRANTE_DAMAGE_POTENTIAL_H

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
 * Class for Smith-Ferrante isotropic damage pseudo-potential.
 */
class SmithFerranteDamagePotential : virtual public IsotropicDamagePotential {
  
 public:
  
  // default constructor
  SmithFerranteDamagePotential() {}
  
  // copy constructor
  SmithFerranteDamagePotential(const SmithFerranteDamagePotential&) {}
  
  // destructor
  virtual ~SmithFerranteDamagePotential() {}
  
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
class SmithFerranteElasticDamage3D : public ElasticIsotropicDamage<TensorAlgebra3D> {
  
 public:
  
  // constructor
  SmithFerranteElasticDamage3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>()),
    ElasticIsotropicDamage<TensorAlgebra3D>(new SmithFerranteDamagePotential()) {}
  
  // copy constructor
  SmithFerranteElasticDamage3D(const SmithFerranteElasticDamage3D& src)
  : Elasticity<TensorAlgebra3D>(src),
    ElasticIsotropicDamage<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~SmithFerranteElasticDamage3D() {}
};
class SmithFerranteElasticDamage2D : public ElasticIsotropicDamage<TensorAlgebra2D> {
  
 public:
  
  // constructor
  SmithFerranteElasticDamage2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>()),
    ElasticIsotropicDamage<TensorAlgebra2D>(new SmithFerranteDamagePotential()) {}
  
  // copy constructor
  SmithFerranteElasticDamage2D(const SmithFerranteElasticDamage2D& src)
  : Elasticity<TensorAlgebra2D>(src),
    ElasticIsotropicDamage<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~SmithFerranteElasticDamage2D() {}
};
class SmithFerranteElasticDamage1D : public ElasticIsotropicDamage<TensorAlgebra1D> {
  
 public:
  
  // constructor
  SmithFerranteElasticDamage1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>()),
    ElasticIsotropicDamage<TensorAlgebra1D>(new SmithFerranteDamagePotential()) {}
  
  // copy constructor
  SmithFerranteElasticDamage1D(const SmithFerranteElasticDamage1D& src)
  : Elasticity<TensorAlgebra1D>(src),
    ElasticIsotropicDamage<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~SmithFerranteElasticDamage1D() {}
};

/**
 * The associated model builder
 */
class SmithFerranteElasticDamageBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  SmithFerranteElasticDamageBuilder();
  
  // the instance
  static SmithFerranteElasticDamageBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~SmithFerranteElasticDamageBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
