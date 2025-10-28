/*
 *  $Id: ContactPenalty.h 180 2015-09-17 18:40:22Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2015, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATL_MECA_CONTACT_PENALTY_H
#define ZORGLIB_MATL_MECA_CONTACT_PENALTY_H

// config
#include <matlib_macros.h>

// local
#include <matl/ConstitutiveModel.h>
#include <matl/ModelDictionary.h>


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Base class for contact treatment with a penalty approach.
 */
class ContactPenalty : virtual public StandardMaterial {
  
 public:

  // nested class
  class ContactPenaltyPotential;

 protected:

  // associated penalty potential
  ContactPenaltyPotential *penalty;
  
  // instance counter
  unsigned int *count;
  
  // empty constructor
  ContactPenalty(ContactPenaltyPotential* p = 0) {
    count = new unsigned int(1);
    penalty = p;
  }

 public:

  // constructor
  ContactPenalty(ContactPenaltyPotential& p) {
    count = new unsigned int(1);
    penalty = &p;
  }
  
  // copy constructor
  ContactPenalty(const ContactPenalty& src) {
    count = src.count;
    (*count)++;
    penalty = src.penalty;

  }
  
  // destructor
  virtual ~ContactPenalty();
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException);
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties&,const ParameterSet&);
  
  // how many external variables ?
  unsigned int nExtVar() const {return 2;}
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const {return 2;}
  ConstitutiveModel::VariableType typeExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      case 1:
        return ConstitutiveModel::TYPE_SCALAR;
        break;
      default:
        return ConstitutiveModel::TYPE_NONE;
        break;
    }
  }
  unsigned int indexExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return 0;
        break;
      case 1:
        return 1;
        break;
      default:
        return 2;
        break;
    }
  }
  std::string labelExtVar(unsigned int i) const {
    switch (i) {
      case 0:
        return "surface jacobian";
        break;
      case 1:
        return "normal gap";
        break;
      default:
        return "";
        break;
    }
  }
  std::string labelExtForce(unsigned int i) const {
    switch (i) {
      case 0:
        return "penalty energy density";
        break;
      case 1:
        return "normal contact force";
        break;
      default:
        return "";
        break;
    }
  }
  
  // how many internal variables ?
  unsigned int nIntVar() const {return 0;}
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const {return 0;}
  unsigned int getIntVar(const std::string&) const {return 0;}
  ConstitutiveModel::VariableType typeIntVar(unsigned int) const {
    return ConstitutiveModel::TYPE_NONE;
  }
  unsigned int indexIntVar(unsigned int) const {return 0;}
  std::string labelIntVar(unsigned int) const {return "";}
  
  // initialize the state of the material
  void initState(const MaterialProperties&,MaterialState&);
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties&,const ParameterSet&,
                              const MaterialState&,MaterialState&,double,
                              MatLibMatrix&,bool,bool)
   throw (UpdateFailedException);
};


/**
 * Base class for contact penalty potentials.
 */
class ContactPenalty::ContactPenaltyPotential {

 protected:

  // default constructor
  ContactPenaltyPotential() {}

 public:

  // destructor
  virtual ~ContactPenaltyPotential() {}
  
  // check consistency of material properties
  virtual void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException) = 0;
  
  // update properties in function of external parameters
  virtual void updateProperties(MaterialProperties&,const ParameterSet&) {}
  
  // compute penalty potential
  virtual double penaltyEnergy(const MaterialProperties&,const ParameterSet&,
                               double,double&,double&,bool,bool) = 0;
};


/**
 * Standard penalty formulation.
 */
class StdContactPenaltyPotential : public ContactPenalty::ContactPenaltyPotential {
  
 public:
  
  // default constructor
  StdContactPenaltyPotential() {}
 
  // destructor
  virtual ~StdContactPenaltyPotential() {}
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,std::ostream* = 0)
   throw (InvalidPropertyException, NoSuchPropertyException);
  
  // compute penalty potential
  double penaltyEnergy(const MaterialProperties&,const ParameterSet&,
                       double,double&,double&,bool,bool);
};


/**
 * Implementation of the model.
 */
class StdContactPenalty : public ContactPenalty {
  
 public:
  
  // constructor
  StdContactPenalty()
  : ContactPenalty(new StdContactPenaltyPotential()) {}
  
  // copy constructor
  StdContactPenalty(const StdContactPenalty& src)
  : ContactPenalty(src) {}
  
  // destructor
  virtual ~StdContactPenalty() {}
};


/**
 * The associated model builder
 */
class StdContactPenaltyBuilder : public ModelBuilder {
  
 private:
  
  // constructor
  StdContactPenaltyBuilder();
  
  // the instance
  static StdContactPenaltyBuilder const* BUILDER;
  
 public:
  
  // destructor
  virtual ~StdContactPenaltyBuilder() {}
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
