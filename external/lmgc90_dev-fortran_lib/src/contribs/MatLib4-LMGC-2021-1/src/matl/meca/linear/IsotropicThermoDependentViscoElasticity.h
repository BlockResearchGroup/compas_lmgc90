/*
 *  $Id$
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2020, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_THERMO_DEPENDENT_ELASTICITY_H
#define ZORGLIB_MATL_MECA_LINEAR_ISOTROPIC_THERMO_DEPENDENT_ELASTICITY_H

// config
#include <matlib_macros.h>

// local
#include <matl/thermomeca/linear/IsotropicThermoViscoElasticMultiPotential.h>

#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif


/**
 * Implementations of the model: Kelvin-Voigt.
 */
class IsotropicThermoDependentKelvinViscoElasticity3D : public ViscoElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicThermoDependentKelvinViscoElasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra3D>()),
    ViscoElasticity<TensorAlgebra3D>(new IsotropicThermalViscousPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicThermoDependentKelvinViscoElasticity3D(const IsotropicThermoDependentKelvinViscoElasticity3D& src)
  : Elasticity<TensorAlgebra3D>(src), ViscoElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDependentKelvinViscoElasticity3D() {}
};
class IsotropicThermoDependentKelvinViscoElasticity2D : public ViscoElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicThermoDependentKelvinViscoElasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra2D>()),
    ViscoElasticity<TensorAlgebra2D>(new IsotropicThermalViscousPotential<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicThermoDependentKelvinViscoElasticity2D(const IsotropicThermoDependentKelvinViscoElasticity2D& src)
  : Elasticity<TensorAlgebra2D>(src), ViscoElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDependentKelvinViscoElasticity2D() {}
};
class IsotropicThermoDependentKelvinViscoElasticity1D : public ViscoElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicThermoDependentKelvinViscoElasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                new IsotropicLinThermalDilatancy<TensorAlgebra1D>()),
    ViscoElasticity<TensorAlgebra1D>(new IsotropicThermalViscousPotential<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicThermoDependentKelvinViscoElasticity1D(const IsotropicThermoDependentKelvinViscoElasticity1D& src)
  : Elasticity<TensorAlgebra1D>(src), ViscoElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDependentKelvinViscoElasticity1D() {}
};

/**
 * The associated model builder
 */
class IsotropicThermoDependentKelvinViscoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicThermoDependentKelvinViscoElasticityBuilder();
  
  // the instance
  static IsotropicThermoDependentKelvinViscoElasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~IsotropicThermoDependentKelvinViscoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};



/**
 * Class for standard (linear) isotropic thermo-viscoelastic model (Maxwell branch).
 */
template <class ALG>
class IsotropicMaxwellThermoDependentViscoElasticity
: virtual public StdMaxwellViscoElasticity<ALG> {
  
 public:
  
  // constructor
  IsotropicMaxwellThermoDependentViscoElasticity(unsigned int r,bool i = false)
  : StdMaxwellViscoElasticity<ALG>(*(new IsotropicThermoElasticMultiPotential<ALG>(r,i)),
                                   *(new IsotropicThermoViscousMultiPotential<ALG>(r,i)),
                                   i) {}
  
  // copy constructor
  IsotropicMaxwellThermoDependentViscoElasticity(const IsotropicMaxwellThermoDependentViscoElasticity& src)
  : StdMaxwellViscoElasticity<ALG>(src) {}
  
  // destructor
  virtual ~IsotropicMaxwellThermoDependentViscoElasticity() {}
};


/**
 * Implementations of the model : Maxwell.
 */
class IsotropicMaxwellThermoDependentViscoElasticity3D : public ViscoElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicMaxwellThermoDependentViscoElasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                new IsotropicThermoElasticDilatancy<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicMaxwellThermoDependentViscoElasticity3D(const IsotropicMaxwellThermoDependentViscoElasticity3D& src)
  : Elasticity<TensorAlgebra3D>(src), ViscoElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicMaxwellThermoDependentViscoElasticity3D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellThermoDependentViscoElasticity<TensorAlgebra3D>(i+1)));
        }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellThermoDependentViscoElasticity<TensorAlgebra3D>(1)));
    }
    
    // check properties
    ViscoElasticity<TensorAlgebra3D>::checkProperties(material,os);
  }
};
class IsotropicMaxwellThermoDependentViscoElasticity2D : public ViscoElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicMaxwellThermoDependentViscoElasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                new IsotropicThermoElasticDilatancy<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicMaxwellThermoDependentViscoElasticity2D(const IsotropicMaxwellThermoDependentViscoElasticity2D& src)
  : Elasticity<TensorAlgebra2D>(src), ViscoElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicMaxwellThermoDependentViscoElasticity2D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellThermoDependentViscoElasticity<TensorAlgebra2D>(i+1)));
        }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellThermoDependentViscoElasticity<TensorAlgebra2D>(1)));
    }
    
    // check properties
    ViscoElasticity<TensorAlgebra2D>::checkProperties(material,os);
  }
};
class IsotropicMaxwellThermoDependentViscoElasticity1D : public ViscoElasticity<TensorAlgebra1D> {
  
public:
  
  // constructor
  IsotropicMaxwellThermoDependentViscoElasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                new IsotropicThermoElasticDilatancy<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicMaxwellThermoDependentViscoElasticity1D(const IsotropicMaxwellThermoDependentViscoElasticity1D& src)
  : Elasticity<TensorAlgebra1D>(src), ViscoElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicMaxwellThermoDependentViscoElasticity1D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellThermoDependentViscoElasticity<TensorAlgebra1D>(i+1)));
        }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellThermoDependentViscoElasticity<TensorAlgebra1D>(1)));
    }
    
    // check properties
    ViscoElasticity<TensorAlgebra1D>::checkProperties(material,os);
  }
};

/**
 * The associated model builder
 */
class IsotropicMaxwellThermoDependentViscoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicMaxwellThermoDependentViscoElasticityBuilder();
  
  // the instance
  static IsotropicMaxwellThermoDependentViscoElasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~IsotropicMaxwellThermoDependentViscoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};


/**
 * Implementations of the model : general viscoelasticity model (Kelvin+Maxwell).
 */
class IsotropicThermoDependentViscoElasticity3D : public ViscoElasticity<TensorAlgebra3D> {
  
 public:
  
  // constructor
  IsotropicThermoDependentViscoElasticity3D()
  : Elasticity<TensorAlgebra3D>(new IsotropicThermoElasticPotential<TensorAlgebra3D>(),
                                new IsotropicThermoElasticDilatancy<TensorAlgebra3D>()),
    ViscoElasticity<TensorAlgebra3D>(new IsotropicThermalViscousPotential<TensorAlgebra3D>()) {}
  
  // copy constructor
  IsotropicThermoDependentViscoElasticity3D(const IsotropicThermoDependentViscoElasticity3D& src)
  : Elasticity<TensorAlgebra3D>(src), ViscoElasticity<TensorAlgebra3D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDependentViscoElasticity3D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellThermoDependentViscoElasticity<TensorAlgebra3D>(i+1)));
        }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellThermoDependentViscoElasticity<TensorAlgebra3D>(1)));
    }
    
    // check properties
    ViscoElasticity<TensorAlgebra3D>::checkProperties(material,os);
  }
};
class IsotropicThermoDependentViscoElasticity2D : public ViscoElasticity<TensorAlgebra2D> {
  
 public:
  
  // constructor
  IsotropicThermoDependentViscoElasticity2D()
  : Elasticity<TensorAlgebra2D>(new IsotropicThermoElasticPotential<TensorAlgebra2D>(),
                                new IsotropicThermoElasticDilatancy<TensorAlgebra2D>()),
    ViscoElasticity<TensorAlgebra2D>(new IsotropicThermalViscousPotential<TensorAlgebra2D>()) {}
  
  // copy constructor
  IsotropicThermoDependentViscoElasticity2D(const IsotropicThermoDependentViscoElasticity2D& src)
  : Elasticity<TensorAlgebra2D>(src), ViscoElasticity<TensorAlgebra2D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDependentViscoElasticity2D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellThermoDependentViscoElasticity<TensorAlgebra2D>(i+1)));
        }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellThermoDependentViscoElasticity<TensorAlgebra2D>(1)));
    }
    
    // check properties
    ViscoElasticity<TensorAlgebra2D>::checkProperties(material,os);
  }
};
class IsotropicThermoDependentViscoElasticity1D : public ViscoElasticity<TensorAlgebra1D> {
  
 public:
  
  // constructor
  IsotropicThermoDependentViscoElasticity1D()
  : Elasticity<TensorAlgebra1D>(new IsotropicThermoElasticPotential<TensorAlgebra1D>(),
                                new IsotropicThermoElasticDilatancy<TensorAlgebra1D>()),
    ViscoElasticity<TensorAlgebra1D>(new IsotropicThermalViscousPotential<TensorAlgebra1D>()) {}
  
  // copy constructor
  IsotropicThermoDependentViscoElasticity1D(const IsotropicThermoDependentViscoElasticity1D& src)
  : Elasticity<TensorAlgebra1D>(src), ViscoElasticity<TensorAlgebra1D>(src) {}
  
  // destructor
  virtual ~IsotropicThermoDependentViscoElasticity1D() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties& material,std::ostream* os = 0)
   throw (InvalidPropertyException) {
    
    // initialize maxwell branches
    try {
      unsigned int nBranches = material.getIntegerProperty("NUMBER_OF_MAXWELL_BRANCHES");
      for (unsigned int i=0; i < nBranches; i++)
        addMaxwellBranch(*(new IsotropicMaxwellThermoDependentViscoElasticity<TensorAlgebra1D>(i+1)));
        }
    catch (NoSuchPropertyException) {
      addMaxwellBranch(*(new IsotropicMaxwellThermoDependentViscoElasticity<TensorAlgebra1D>(1)));
    }
    
    // check properties
    ViscoElasticity<TensorAlgebra1D>::checkProperties(material,os);
  }
};

/**
 * The associated model builder
 */
class IsotropicThermoDependentViscoElasticityBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  IsotropicThermoDependentViscoElasticityBuilder();
  
  // the instance
  static IsotropicThermoDependentViscoElasticityBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~IsotropicThermoDependentViscoElasticityBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
