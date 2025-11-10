/*
 *  $Id: MaterialModel.h 138 2013-08-30 15:25:50Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MATERIAL_MODEL_H
#define ZORGLIB_MATL_MATERIAL_MODEL_H

// config
#include <matlib_macros.h>

// local
#ifndef WITH_MATLIB_H
#include "matl/ConstitutiveModel.h"
#include "matl/MaterialProperties.h"
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class constituting a complete material model:
 *   constitutive model + material properties
 */
class MaterialModel {

 private:

  // constitutive model
  ConstitutiveModel* modl;

  // material properties
  MaterialProperties* prop;

 public:

  // constructor
  MaterialModel(ConstitutiveModel&,MaterialProperties&);

  // copy constructor
  MaterialModel(const MaterialModel&);

  // destructor
  ~MaterialModel();

  // assignment operator
  MaterialModel& operator=(const MaterialModel&);

  // get constitutive model
  ConstitutiveModel& model() const {return *modl;}

  // set constitutive model
  void setModel(ConstitutiveModel& m) {modl = &m;}

  // get material properties
  MaterialProperties& properties() const {return *prop;}

  // set material properties
  void setProperties(MaterialProperties& p);

  // initialize material model
  void initialize(std::ostream*) throw (InvalidPropertyException, NoSuchPropertyException);
  void initialize(const char* = 0) throw (InvalidPropertyException, NoSuchPropertyException);

  // rotate material properties
  void rotateProperties(const Rotation&);
  
  // update material properties
  void updateProperties(const ParameterSet&);
  
  // initialize the state of the material
  void initState(MaterialState&);
  
  // update the state of the material (with the ability to compute tangents)
  void updateState(const ParameterSet&,const MaterialState&,MaterialState&,
                   double,MatLibMatrix&,bool) throw (UpdateFailedException);
  
  // compute material tangents (without updating)
  void computeTangent(const ParameterSet&,const MaterialState&,MaterialState&,
                      double,MatLibMatrix&);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
