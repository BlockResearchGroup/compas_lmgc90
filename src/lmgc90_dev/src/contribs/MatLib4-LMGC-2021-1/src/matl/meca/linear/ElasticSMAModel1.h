/*
 *  $Id: ElasticSMAModel1.h 139 2013-08-30 15:33:21Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ELASTIC_SMA_MODEL_1_H
#define ZORGLIB_MATL_MECA_LINEAR_ELASTIC_SMA_MODEL_1_H

// config
#include <matlib_macros.h>

// local
#include <matl/meca/linear/IsotropicLinThermalDilatancy.h>
#include <matl/meca/linear/ElasticSMA.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Standard latent heat potential.
 */
class StdSMALatentHeatPotential : virtual public LatentHeatPotential {
  
 public:
  
  // constructor
  StdSMALatentHeatPotential() {}
  
  // copy constructor
  StdSMALatentHeatPotential(const StdSMALatentHeatPotential&) {}
  
  // destructor
  virtual ~StdSMALatentHeatPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0) 
   throw (InvalidPropertyException, NoSuchPropertyException);
  
  // compute latent heat energy
  double latentHeat(const MaterialProperties&,const ParameterSet&,
                    double,double&,double&,bool,bool);
};


/**
 * Implementations of the model.
 */
class ElasticSMAModel1_3D : public ElasticSMA<TensorAlgebra3D> {
  
 public:
  
  // constructor
  ElasticSMAModel1_3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicElasticPotential<TensorAlgebra3D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra3D>()),
    ElasticSMA<TensorAlgebra3D>(new StdSMALatentHeatPotential()) {}
  
  // copy constructor
  ElasticSMAModel1_3D(const ElasticSMAModel1_3D& src) 
  : Elasticity<TensorAlgebra3D>(src),ElasticSMA<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~ElasticSMAModel1_3D() {}
};
class ElasticSMAModel1_2D : public ElasticSMA<TensorAlgebra2D> {
  
 public:
  
  // constructor
  ElasticSMAModel1_2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicElasticPotential<TensorAlgebra2D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra2D>()),
    ElasticSMA<TensorAlgebra2D>(new StdSMALatentHeatPotential()) {}
  
  // copy constructor
  ElasticSMAModel1_2D(const ElasticSMAModel1_2D& src) 
  : Elasticity<TensorAlgebra2D>(src),ElasticSMA<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~ElasticSMAModel1_2D() {}
};
class ElasticSMAModel1_1D : public ElasticSMA<TensorAlgebra1D> {
  
 public:
  
  // constructor
  ElasticSMAModel1_1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicElasticPotential<TensorAlgebra1D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra1D>()),
  ElasticSMA<TensorAlgebra1D>(new StdSMALatentHeatPotential()) {}
  
  // copy constructor
  ElasticSMAModel1_1D(const ElasticSMAModel1_1D& src) 
  : Elasticity<TensorAlgebra1D>(src),ElasticSMA<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~ElasticSMAModel1_1D() {}
};

/**
 * The associated model builder
 */
class ElasticSMAModel1Builder : public ModelBuilder {
  
 private:
  
  // constructor
  ElasticSMAModel1Builder();
  
  // the instance
  static ElasticSMAModel1Builder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~ElasticSMAModel1Builder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
