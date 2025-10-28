/*
 *  $Id: MaterialCriterion.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MATERIAL_CRITERION_H
#define ZORGLIB_MATL_MATERIAL_CRITERION_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include <matl/ConstitutiveModel.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Virtual base class for material criteria
 */
class MaterialCriterion {

 protected:

  // constructor
  MaterialCriterion() {}

 public:

  // virtual destructor
  virtual ~MaterialCriterion() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream*) 
    throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  void checkProperties(MaterialProperties&,const char* = 0) 
    throw (FileException, InvalidPropertyException, NoSuchPropertyException);
  
  // apply rotation to material properties
  virtual void rotateProperties(MaterialProperties&,const Rotation&) {}
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // how many external variables ?
  virtual unsigned int nExtVar() const = 0;
  
  // how many internal variables ?
  virtual unsigned int nIntVar() const = 0;
  
  // evaluate criterion
  virtual double evaluateCriterion(const MaterialProperties&,const ParameterSet&,
                                   const MaterialState&,const MatLibMatrix&,double) = 0;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
