/*
 *  $Id: JohnsonCookModel.h 150 2014-08-25 19:41:10Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2014, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_THERMO_JOHNSON_COOK_MODEL_H
#define ZORGLIB_MATL_MECA_THERMO_JOHNSON_COOK_MODEL_H

// config
#include <matlib_macros.h>

// local
#include <math/TensorAlgebra.h>
#include <matl/thermomeca/coupled/AdiabaticLinThermoMechanics.h>
#include <matl/thermomeca/coupled/AdiabaticStdThermoMechanics.h>
#include <matl/thermomeca/coupled/CoupledLinThermoMechanics.h>
#include <matl/thermomeca/coupled/IsotropicThMConductionPotential.h>
#include <matl/thermomeca/linear/ThermoViscoPlasticitySimple.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for describing a J2 thermo-visco-plasticity model with Johson-Cook flow stress.
 */
class JohnsonCookModel : public ThermoViscoPlasticitySimple {
  
 protected:
  
  // instance counter
  unsigned int *count;
  
  // (plastic) stored energy
  double storedEnergy(const MaterialProperties&,const ParameterSet&,double,double,
                      double&,double&,double&,double&,double&,bool,bool);
  
  // dissipation potential
  double dissipatedEnergy(const MaterialProperties&,const ParameterSet&,double,double,double,
                          double&,double&,double&,double&,double&,double&,
                          double&,double&,double&,bool,bool);

 public:
  
  // constructor
  JohnsonCookModel() {count = new unsigned int(1);}
  
  // copy constructor
  JohnsonCookModel(const JohnsonCookModel& src) {
    count = src.count;
    (*count)++;
  }
  
  // destructor
  virtual ~JohnsonCookModel() {
    if (--(*count) > 0) return;
    delete count;
  }
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
    throw (InvalidPropertyException, NoSuchPropertyException);
  
  // number of internal parameters
  unsigned int nIntPar() const {
    return 2; // plastic stored energy + heat fraction
  }
  
  // compute irreversible energy and derivatives
  double irreversibleEnergy(const MaterialProperties&,const ParameterSet&,
                            const MatLibArray&,MatLibArray&,double,double,
                            double,double,double,double,double&,double&,
                            double&,double&,double&,double&,double,bool,bool);
};


/*
 * Implementations of the model.
 */

/**
 * J2 thermoplasticity with Johnson-Cook flow stress (small strains).
 */
class JohnsonCookJ2ThermoPlasticity3D : public J2ThermoPlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  JohnsonCookJ2ThermoPlasticity3D()
  : ThermoElasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra3D>()),
    J2ThermoPlasticitySimple<TensorAlgebra3D>(new JohnsonCookModel()) {}
  
  // copy constructor
  JohnsonCookJ2ThermoPlasticity3D(const JohnsonCookJ2ThermoPlasticity3D& src)
  : ThermoElasticity<TensorAlgebra3D>(src), ThermoElastoPlasticity<TensorAlgebra3D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~JohnsonCookJ2ThermoPlasticity3D() {}
};
class JohnsonCookJ2ThermoPlasticity2D : public J2ThermoPlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  JohnsonCookJ2ThermoPlasticity2D()
  : ThermoElasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra2D>()),
    J2ThermoPlasticitySimple<TensorAlgebra2D>(new JohnsonCookModel()) {}
  
  // copy constructor
  JohnsonCookJ2ThermoPlasticity2D(const JohnsonCookJ2ThermoPlasticity2D& src)
  : ThermoElasticity<TensorAlgebra2D>(src), ThermoElastoPlasticity<TensorAlgebra2D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~JohnsonCookJ2ThermoPlasticity2D() {}
};
class JohnsonCookJ2ThermoPlasticity1D : public J2ThermoPlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  JohnsonCookJ2ThermoPlasticity1D()
  : ThermoElasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                      new StdLinThermalCapacity(),
                                      new IsotropicThermoElasticDilatancy<TensorAlgebra1D>()),
    J2ThermoPlasticitySimple<TensorAlgebra1D>(new JohnsonCookModel()) {}
  
  // copy constructor
  JohnsonCookJ2ThermoPlasticity1D(const JohnsonCookJ2ThermoPlasticity1D& src)
  : ThermoElasticity<TensorAlgebra1D>(src), ThermoElastoPlasticity<TensorAlgebra1D>(src),
    J2ThermoPlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~JohnsonCookJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class JohnsonCookJ2ThermoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  JohnsonCookJ2ThermoPlasticityBuilder();
  
  // the instance
  static JohnsonCookJ2ThermoPlasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~JohnsonCookJ2ThermoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Adiabatic small strains Johnson-Cook J2 thermo-plasticity
 */
class AdiabaticJohnsonCookJ2ThermoPlasticity3D
: public AdiabaticLinThermoMechanics<TensorAlgebra3D> {
  
 public:
  
  // constructor
  AdiabaticJohnsonCookJ2ThermoPlasticity3D()
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(new JohnsonCookJ2ThermoPlasticity3D()) {}
  
  // copy constructor
  AdiabaticJohnsonCookJ2ThermoPlasticity3D(const AdiabaticJohnsonCookJ2ThermoPlasticity3D& src)
  : AdiabaticLinThermoMechanics<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~AdiabaticJohnsonCookJ2ThermoPlasticity3D() {}
};
class AdiabaticJohnsonCookJ2ThermoPlasticity2D
: public AdiabaticLinThermoMechanics<TensorAlgebra2D> {
  
 public:
  
  // constructor
  AdiabaticJohnsonCookJ2ThermoPlasticity2D()
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(new JohnsonCookJ2ThermoPlasticity2D()) {}
  
  // copy constructor
  AdiabaticJohnsonCookJ2ThermoPlasticity2D(const AdiabaticJohnsonCookJ2ThermoPlasticity2D& src)
  : AdiabaticLinThermoMechanics<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~AdiabaticJohnsonCookJ2ThermoPlasticity2D() {}
};
class AdiabaticJohnsonCookJ2ThermoPlasticity1D
: public AdiabaticLinThermoMechanics<TensorAlgebra1D> {
  
 public:
  
  // constructor
  AdiabaticJohnsonCookJ2ThermoPlasticity1D()
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(new JohnsonCookJ2ThermoPlasticity1D()) {}
  
  // copy constructor
  AdiabaticJohnsonCookJ2ThermoPlasticity1D(const AdiabaticJohnsonCookJ2ThermoPlasticity1D& src)
  : AdiabaticLinThermoMechanics<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~AdiabaticJohnsonCookJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class AdiabaticJohnsonCookJ2ThermoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  AdiabaticJohnsonCookJ2ThermoPlasticityBuilder();
  
  // the instance
  static AdiabaticJohnsonCookJ2ThermoPlasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~AdiabaticJohnsonCookJ2ThermoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Coupled small-strains Johnson-Cook J2 thermo-plasticity
 */
class CoupledJohnsonCookJ2ThermoPlasticity3D
: public CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledJohnsonCookJ2ThermoPlasticity3D()
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new JohnsonCookJ2ThermoPlasticity3D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledJohnsonCookJ2ThermoPlasticity3D(const CoupledJohnsonCookJ2ThermoPlasticity3D& src)
  : CoupledLinThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledJohnsonCookJ2ThermoPlasticity3D() {}
};
class CoupledJohnsonCookJ2ThermoPlasticity2D
: public CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledJohnsonCookJ2ThermoPlasticity2D()
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new JohnsonCookJ2ThermoPlasticity2D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledJohnsonCookJ2ThermoPlasticity2D(const CoupledJohnsonCookJ2ThermoPlasticity2D& src)
  : CoupledLinThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledJohnsonCookJ2ThermoPlasticity2D() {}
};
class CoupledJohnsonCookJ2ThermoPlasticity1D
: public CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledJohnsonCookJ2ThermoPlasticity1D()
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new JohnsonCookJ2ThermoPlasticity1D(),
                new IsotropicLinConductionPotential<StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledJohnsonCookJ2ThermoPlasticity1D(const CoupledJohnsonCookJ2ThermoPlasticity1D& src)
  : CoupledLinThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledJohnsonCookJ2ThermoPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledJohnsonCookJ2ThermoPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledJohnsonCookJ2ThermoPlasticityBuilder();
  
  // the instance
  static CoupledJohnsonCookJ2ThermoPlasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~CoupledJohnsonCookJ2ThermoPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * J2 thermoplasticity with Johnson-Cook flow stress (finite strains).
 */
class JohnsonCookJ2ThermoHEPlasticity3D : public J2ThermoHEPlasticitySimple<TensorAlgebra3D> {
  
 public:
  
  // constructor
  JohnsonCookJ2ThermoHEPlasticity3D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra3D>(
          new GeneralThermalHenckyPotential<TensorAlgebra3D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra3D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra3D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra3D>(new JohnsonCookModel()) {}
  
  // copy constructor
  JohnsonCookJ2ThermoHEPlasticity3D(const JohnsonCookJ2ThermoHEPlasticity3D& src)
  : ThermoHyperElasticity<TensorAlgebra3D>(src), ThermoHyperElastoPlasticity<TensorAlgebra3D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~JohnsonCookJ2ThermoHEPlasticity3D() {}
};
class JohnsonCookJ2ThermoHEPlasticity2D : public J2ThermoHEPlasticitySimple<TensorAlgebra2D> {
  
 public:
  
  // constructor
  JohnsonCookJ2ThermoHEPlasticity2D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra2D>(
          new GeneralThermalHenckyPotential<TensorAlgebra2D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra2D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra2D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra2D>(new JohnsonCookModel()) {}
  
  // copy constructor
  JohnsonCookJ2ThermoHEPlasticity2D(const JohnsonCookJ2ThermoHEPlasticity2D& src)
  : ThermoHyperElasticity<TensorAlgebra2D>(src), ThermoHyperElastoPlasticity<TensorAlgebra2D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~JohnsonCookJ2ThermoHEPlasticity2D() {}
};
class JohnsonCookJ2ThermoHEPlasticity1D : public J2ThermoHEPlasticitySimple<TensorAlgebra1D> {
  
 public:
  
  // constructor
  JohnsonCookJ2ThermoHEPlasticity1D(ThermalEOS *eos = 0)
  : ThermoHyperElasticity<TensorAlgebra1D>(
          new GeneralThermalHenckyPotential<TensorAlgebra1D>(
                    *(new IsotropicThermoElasticPotential<TensorAlgebra1D>())),
          eos,new StdThermalCapacity(),
          new IsotropicThermoHyperElasticDilatancy<TensorAlgebra1D>()),
    J2ThermoHEPlasticitySimple<TensorAlgebra1D>(new JohnsonCookModel()) {}
  
  // copy constructor
  JohnsonCookJ2ThermoHEPlasticity1D(const JohnsonCookJ2ThermoHEPlasticity1D& src)
  : ThermoHyperElasticity<TensorAlgebra1D>(src), ThermoHyperElastoPlasticity<TensorAlgebra1D>(src),
    J2ThermoHEPlasticitySimple<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~JohnsonCookJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class JohnsonCookJ2ThermoHEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  JohnsonCookJ2ThermoHEPlasticityBuilder();
  
  // the instance
  static JohnsonCookJ2ThermoHEPlasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~JohnsonCookJ2ThermoHEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Adiabatic finite strains Johnson-Cook J2 thermo-plasticity
 */
class AdiabaticJohnsonCookJ2ThermoHEPlasticity3D
: public AdiabaticStdThermoMechanics<TensorAlgebra3D> {
  
 public:
  
  // constructor
  AdiabaticJohnsonCookJ2ThermoHEPlasticity3D()
  : AdiabaticStdThermoMechanics<TensorAlgebra3D>(new JohnsonCookJ2ThermoHEPlasticity3D()) {}
  
  // copy constructor
  AdiabaticJohnsonCookJ2ThermoHEPlasticity3D(const AdiabaticJohnsonCookJ2ThermoHEPlasticity3D& src)
  : AdiabaticStdThermoMechanics<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~AdiabaticJohnsonCookJ2ThermoHEPlasticity3D() {}
};
class AdiabaticJohnsonCookJ2ThermoHEPlasticity2D
: public AdiabaticStdThermoMechanics<TensorAlgebra2D> {
  
 public:
  
  // constructor
  AdiabaticJohnsonCookJ2ThermoHEPlasticity2D()
  : AdiabaticStdThermoMechanics<TensorAlgebra2D>(new JohnsonCookJ2ThermoHEPlasticity2D()) {}
  
  // copy constructor
  AdiabaticJohnsonCookJ2ThermoHEPlasticity2D(const AdiabaticJohnsonCookJ2ThermoHEPlasticity2D& src)
  : AdiabaticStdThermoMechanics<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~AdiabaticJohnsonCookJ2ThermoHEPlasticity2D() {}
};
class AdiabaticJohnsonCookJ2ThermoHEPlasticity1D
: public AdiabaticStdThermoMechanics<TensorAlgebra1D> {
  
 public:
  
  // constructor
  AdiabaticJohnsonCookJ2ThermoHEPlasticity1D()
  : AdiabaticStdThermoMechanics<TensorAlgebra1D>(new JohnsonCookJ2ThermoHEPlasticity1D()) {}
  
  // copy constructor
  AdiabaticJohnsonCookJ2ThermoHEPlasticity1D(const AdiabaticJohnsonCookJ2ThermoHEPlasticity1D& src)
  : AdiabaticStdThermoMechanics<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~AdiabaticJohnsonCookJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class AdiabaticJohnsonCookJ2ThermoHEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  AdiabaticJohnsonCookJ2ThermoHEPlasticityBuilder();
  
  // the instance
  static AdiabaticJohnsonCookJ2ThermoHEPlasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~AdiabaticJohnsonCookJ2ThermoHEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Coupled finite strains JohnsonCook J2 thermo-plasticity
 */
class CoupledJohnsonCookJ2ThermoHEPlasticity3D
: public CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D> {
  
 public:
  
  // constructor
  CoupledJohnsonCookJ2ThermoHEPlasticity3D()
  : CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(
                new JohnsonCookJ2ThermoHEPlasticity3D(),
                new IsotropicThMConductionPotential<TensorAlgebra3D,StdTensorAlgebra3D>()) {}
  
  // copy constructor
  CoupledJohnsonCookJ2ThermoHEPlasticity3D(const CoupledJohnsonCookJ2ThermoHEPlasticity3D& src)
  : CoupledStdThermoMechanics<TensorAlgebra3D,StdTensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~CoupledJohnsonCookJ2ThermoHEPlasticity3D() {}
};
class CoupledJohnsonCookJ2ThermoHEPlasticity2D
: public CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D> {
  
 public:
  
  // constructor
  CoupledJohnsonCookJ2ThermoHEPlasticity2D()
  : CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(
                new JohnsonCookJ2ThermoHEPlasticity2D(),
                new IsotropicThMConductionPotential<TensorAlgebra2D,StdTensorAlgebra2D>()) {}
  
  // copy constructor
  CoupledJohnsonCookJ2ThermoHEPlasticity2D(const CoupledJohnsonCookJ2ThermoHEPlasticity2D& src)
  : CoupledStdThermoMechanics<TensorAlgebra2D,StdTensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~CoupledJohnsonCookJ2ThermoHEPlasticity2D() {}
};
class CoupledJohnsonCookJ2ThermoHEPlasticity1D
: public CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D> {
  
 public:
  
  // constructor
  CoupledJohnsonCookJ2ThermoHEPlasticity1D()
  : CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(
                new JohnsonCookJ2ThermoHEPlasticity1D(),
                new IsotropicThMConductionPotential<TensorAlgebra1D,StdTensorAlgebra1D>()) {}
  
  // copy constructor
  CoupledJohnsonCookJ2ThermoHEPlasticity1D(const CoupledJohnsonCookJ2ThermoHEPlasticity1D& src)
  : CoupledStdThermoMechanics<TensorAlgebra1D,StdTensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~CoupledJohnsonCookJ2ThermoHEPlasticity1D() {}
};

/**
 * The associated model builder
 */
class CoupledJohnsonCookJ2ThermoHEPlasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  CoupledJohnsonCookJ2ThermoHEPlasticityBuilder();
  
  // the instance
  static CoupledJohnsonCookJ2ThermoHEPlasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~CoupledJohnsonCookJ2ThermoHEPlasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
