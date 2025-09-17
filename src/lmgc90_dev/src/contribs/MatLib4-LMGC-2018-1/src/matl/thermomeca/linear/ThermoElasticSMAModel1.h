/*
 *  $Id: ThermoElasticSMAModel1.h 142 2014-02-07 12:51:54Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_LINEAR_ELASTIC_SMA_MODEL_1_H
#define ZORGLIB_MATL_MECA_THERMO_LINEAR_ELASTIC_SMA_MODEL_1_H

// config
#include <matlib_macros.h>

// local
#include <matl/thermomeca/linear/IsotropicThermoElasticity.h>
#include <matl/thermomeca/linear/ThermoElasticSMA.h>
#include <matl/meca/linear/ElasticSMAModel1.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Standard latent heat potential.
 */
class StdSMALatentHeatThermalPotential : virtual public LatentHeatThermalPotential,
                                         virtual public StdSMALatentHeatPotential {
  
 public:
  
  // constructor
  StdSMALatentHeatThermalPotential() {}
  
  // copy constructor
  StdSMALatentHeatThermalPotential(const StdSMALatentHeatThermalPotential&) {}
  
  // destructor
  virtual ~StdSMALatentHeatThermalPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0) 
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // compute latent heat energy
  using LatentHeatThermalPotential::latentHeat;
  double latentHeat(const MaterialProperties&,const ParameterSet&,
                    double,double,double&,double&,
                    double&,double&,double&,bool,bool);
};


/**
 * Implementations of the model.
 */
class ThermoElasticSMAModel1_3D : public ThermoElasticSMA<TensorAlgebra3D> {
  
 public:
  
  // constructor
  ThermoElasticSMAModel1_3D()
  : ThermoElasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra3D>()),
    ThermoElasticSMA<TensorAlgebra3D>(new StdSMALatentHeatThermalPotential()) {}
  
  // copy constructor
  ThermoElasticSMAModel1_3D(const ThermoElasticSMAModel1_3D& src) 
  : ThermoElasticity<TensorAlgebra3D>(src),
    ThermoElasticSMA<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~ThermoElasticSMAModel1_3D() {}
};
class ThermoElasticSMAModel1_2D : public ThermoElasticSMA<TensorAlgebra2D> {
  
 public:
  
  // constructor
  ThermoElasticSMAModel1_2D()
  : ThermoElasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra2D>()),
    ThermoElasticSMA<TensorAlgebra2D>(new StdSMALatentHeatThermalPotential()) {}
  
  // copy constructor
  ThermoElasticSMAModel1_2D(const ThermoElasticSMAModel1_2D& src) 
  : ThermoElasticity<TensorAlgebra2D>(src),
    ThermoElasticSMA<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~ThermoElasticSMAModel1_2D() {}
};
class ThermoElasticSMAModel1_1D : public ThermoElasticSMA<TensorAlgebra1D> {
  
 public:
  
  // constructor
  ThermoElasticSMAModel1_1D()
  : ThermoElasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra1D>()),
  ThermoElasticSMA<TensorAlgebra1D>(new StdSMALatentHeatThermalPotential()) {}
  
  // copy constructor
  ThermoElasticSMAModel1_1D(const ThermoElasticSMAModel1_1D& src) 
  : ThermoElasticity<TensorAlgebra1D>(src),
  ThermoElasticSMA<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~ThermoElasticSMAModel1_1D() {}
};

/**
 * The associated model builder
 */
class ThermoElasticSMAModel1Builder : public ModelBuilder {
  
 private:
  
  // constructor
  ThermoElasticSMAModel1Builder();
  
  // the instance
  static ThermoElasticSMAModel1Builder const* BUILDER;
  
 public:
    
  // destructor
  virtual ~ThermoElasticSMAModel1Builder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
