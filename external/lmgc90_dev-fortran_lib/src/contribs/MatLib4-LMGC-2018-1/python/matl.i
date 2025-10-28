/*
 *  $Id: matl.i 201 2016-03-25 13:21:09Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
%{
#include <typeinfo>

#include <matl/CriterionDictionary.h>
#include <matl/MaterialModel.h>
#include <matl/ModelDictionary.h>
  
#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif
%}


/*************
   Materials
 *************/

/**
 * Exception thrown when a property is not found in the table.
 */
class NoSuchPropertyException : public ZException {
  
 public:
  
  // default constructor
  NoSuchPropertyException(const std::string& = "no such property");
  
  // copy constructor
  NoSuchPropertyException(const NoSuchPropertyException&);
};


/**
 * Class containing a set of material properties.
 */
class MaterialProperties {
  
 public:
  
  // define iterators
  //typedef PropertyTable::iterator Iterator;
  //typedef PropertyTable::const_iterator ConstIterator;
    
  // constructor
  MaterialProperties(const std::string& = "no name");
  
  // copy constructor
  MaterialProperties(const MaterialProperties&);
  
  // assignment operator
  %ignore operator=;
  MaterialProperties& operator=(const MaterialProperties&);
  %extend {
    void copy(const MaterialProperties& src) {self->operator=(src);}
  }
  
  // clear data
  void clear();

  // get material's name
  std::string getName() const;
  
  // check if property exists
  bool checkProperty(const std::string&) const;
  
  // get property associated to keyword
  Property& getProperty(const std::string&) const
    throw (NoSuchPropertyException);
  int       getIntegerProperty(const std::string&) const
    throw (NoSuchPropertyException);
  double    getDoubleProperty(const std::string&) const
    throw (NoSuchPropertyException);
  std::string getStringProperty(const std::string&) const
    throw (NoSuchPropertyException);
  Function& getFunctionProperty(const std::string&) const
    throw (NoSuchPropertyException);
  
  // set property associated to keyword
  void setProperty(const std::string&,Property&);
  void setProperty(const std::string&,int);
  void setProperty(const std::string&,double);
  void setProperty(const std::string&,const char*);
  void setProperty(const std::string&,Function&);
    
  // iterators
  //ConstIterator begin() const;
  //ConstIterator end() const;

  %extend{
    PyObject* asDict() const {
      PyObject* d = PyDict_New();
      MaterialProperties::ConstIterator iter;
      for (iter = self->begin(); iter != self->end(); iter++) {
        Property& p = *(iter->second);
        if (typeid(p) == typeid(DoubleProperty)) {
          double val = dynamic_cast<DoubleProperty*>(iter->second)->value();
          PyDict_SetItemString(d,iter->first.c_str(),PyFloat_FromDouble(val));
        }
        else if (typeid(p) == typeid(IntegerProperty)) {
          int val = dynamic_cast<IntegerProperty*>(iter->second)->value();
          PyDict_SetItemString(d,iter->first.c_str(),PyInt_FromLong(val));
        }
        else if (typeid(p) == typeid(StringProperty)) {
          std::string val = dynamic_cast<StringProperty*>(iter->second)->value();
          PyDict_SetItemString(d,iter->first.c_str(),PyString_FromString(val.c_str()));
        }
        else if (typeid(p) == typeid(FunctionProperty)) {
          Function& val = dynamic_cast<FunctionProperty*>(iter->second)->function();
	  if (typeid(val) == typeid(TabulatedFunction)) {
            PyObject *fct = SWIG_NewPointerObj(dynamic_cast<TabulatedFunction*>(&val),SWIGTYPE_p_TabulatedFunction,0);
            PyDict_SetItemString(d,iter->first.c_str(),fct);
	  }
	  else if (typeid(val) == typeid(TabulatedFunction2)) {
            PyObject *fct = SWIG_NewPointerObj(dynamic_cast<TabulatedFunction2*>(&val),SWIGTYPE_p_TabulatedFunction2,0);
            PyDict_SetItemString(d,iter->first.c_str(),fct);
	  }
	  else {
            PyObject *fct = SWIG_NewPointerObj(&val,SWIGTYPE_p_Function,0);
            PyDict_SetItemString(d,iter->first.c_str(),fct);
	  }
        }
        else
          PySys_WriteStdout("WARNING: key %s not exported\n",iter->first.c_str());
      }
      return d;
    }
  }

  // read from an input stream
  void readFrom(const char* = 0) throw (FileException, SyntaxError);
};

/*
 * Define MatLib type (for interface).
 */
typedef ShortArray MatLibArray;
typedef ShortSqrMatrix MatLibMatrix;

/**
 * Class describing the state of a material point.
 */
class MaterialState {
  
 public:
  
  // constructor
  MaterialState();
  
  // assignement operator
  %ignore operator=;
  MaterialState& operator=(const MaterialState&);
  %extend {
    void copy(const MaterialState& src) {self->operator=(src);}
  }
  
  // external variables
  MatLibArray grad;
  
  // associated forces
  MatLibArray flux;
  
  // internal variables
  MatLibArray internal;
  
  // extra data
  Copiable* extra;
};

/**
 * Exception thrown when a property has been given an invalid value.
 */
class InvalidPropertyException : public ZException {
  
 public:
  
  // default constructor
  InvalidPropertyException(const std::string& = "invalid property");
  
  // copy constructor
  InvalidPropertyException(const InvalidPropertyException&);
};

/**
 * Exception thrown when the constitutive update fails.
 */
class UpdateFailedException : public ZException {
  
 public:
  
  // default constructor
  UpdateFailedException(const std::string& = "update failed");
  
  // copy constructor
  UpdateFailedException(const UpdateFailedException&);
};

/**
 * Define new type ParameterSet
 */
typedef StringMap<double>::Type ParameterSet;
%typemap (in) ParameterSet& {
  if (PyDict_Check($input)) {
    Py_ssize_t size = PyDict_Size($input);
    $1 = new ParameterSet(size);
    PyObject *key,*value;
    Py_ssize_t pos = 0;
    while (PyDict_Next($input, &pos, &key, &value)) {
      if (PyString_Check(key) && PyFloat_Check(value)) {
        (*$1)[PyString_AsString(key)] = PyFloat_AsDouble(value);
      }
      else {
        PyErr_SetString(PyExc_TypeError,"dictionary items must be string-float pairs");
        delete $1;
        return 0;
      }
    }
  }
  else {
    PyErr_SetString(PyExc_TypeError,"not a dictionary");
    return 0;
  }
}
%typemap (freearg) ParameterSet& {
  delete $1;
}

/**
 * Virtual base class for constitutive models.
 */
%exception ConstitutiveModel::updateState {
  try {
    $action
  }
  catch (UpdateFailedException& e) {
    PyErr_SetString(PyExc_RuntimeError,e.mesg().c_str());
    return 0;
  }
}
class ConstitutiveModel {
  
 public:
  
  // define variable types handled by const. models
  enum VariableType {
    TYPE_NONE,
    TYPE_SCALAR,
    TYPE_VECTOR,
    TYPE_SYM_TENSOR,
    TYPE_TENSOR,
    TYPE_STD_SYM_TENSOR,
    TYPE_STD_TENSOR
  };
  
 protected:
  
  // constructor
  ConstitutiveModel();
  
 public:
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,const char* = 0) 
    throw (FileException, InvalidPropertyException, NoSuchPropertyException);
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties&,const Rotation&);
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties&,const ParameterSet&);
  
  // how many external variables ?
  unsigned int nExtVar() const;
  
  // self-documenting utilities
  unsigned int nExtVarBundled() const;
  VariableType typeExtVar(unsigned int) const;
  unsigned int indexExtVar(unsigned int) const;
  std::string labelExtVar(unsigned int) const;
  std::string labelExtForce(unsigned int) const;
  
  // how many internal variables ?
  unsigned int nIntVar() const;
  
  // self-documenting utilities
  unsigned int nIntVarBundled() const;
  unsigned int getIntVar(const std::string&) const;
  VariableType typeIntVar(unsigned int) const;
  unsigned int indexIntVar(unsigned int) const;
  std::string labelIntVar(unsigned int) const;
  
  // utility function
  static unsigned int dimension(VariableType,unsigned int);
  
  // check if the material behaviour is linear ?
  bool isLinear() const;
  
  // check if the material is "standard" ?
  bool isStandard() const;
  %extend {
    StandardMaterial* toStandardMaterial() {
      return dynamic_cast<StandardMaterial*>(self);
    }
  }
  
  // initialize the state of the material
  void initState(const MaterialProperties&,MaterialState&);
  
  // update the state of the material (with the ability to compute tangents)
  void updateState(const MaterialProperties&,const ParameterSet&,
                   const MaterialState&,MaterialState&,double,MatLibMatrix&,bool);
  
  // compute material tangents (without updating)
  void computeTangent(const MaterialProperties&,const ParameterSet&,
                      const MaterialState&,const MaterialState&,
                      double,MatLibMatrix&);
  
  // compute material tangents by numerical perturbation
  void computeNumericalTangent(const MaterialProperties&,const ParameterSet&,
                               const MaterialState&,const MaterialState&,double,
                               MatLibMatrix&);
};

/**
 * Additional interface for standard materials.
 */
%exception StandardMaterial::incrementalPotential {
  try {
    $action
  }
  catch (UpdateFailedException& e) {
    PyErr_SetString(PyExc_RuntimeError,e.mesg().c_str());
    return 0;
  }
}
%exception StandardMaterial::updateState {
  try {
    $action
  }
  catch (UpdateFailedException& e) {
    PyErr_SetString(PyExc_RuntimeError,e.mesg().c_str());
    return 0;
  }
}
class StandardMaterial : virtual public ConstitutiveModel {
  
 public:
  
  // check if the material is "standard" ?
  bool isStandard() const;
  
  // compute the incremental potential
  double incrementalPotential(const MaterialProperties&,
                              const ParameterSet&,
                              const MaterialState&,MaterialState&,
                              double,MatLibMatrix&,bool,bool) = 0;
  
  // update the state of the material
  void updateState(const MaterialProperties&,const ParameterSet&,
                   const MaterialState&,MaterialState&,
                   double,MatLibMatrix&,bool);
  
  // compute material tangents (without updating)
  void computeTangent(const MaterialProperties&,const ParameterSet&,
                      const MaterialState&,const MaterialState&,
                      double,MatLibMatrix&);
};

/**
 * Class constituting a complete material model:
 *   constitutive model + material properties
 */
%exception MaterialModel::updateState {
  try {
    $action
  }
  catch (UpdateFailedException& e) {
    PyErr_SetString(PyExc_RuntimeError,e.mesg().c_str());
    return 0;
  }
}
class MaterialModel {
  
 public:
  
  // constructor
  MaterialModel(ConstitutiveModel&,MaterialProperties&);
  
  // copy constructor
  MaterialModel(const MaterialModel&);
  
  // assignment operator
  %ignore operator=;
  MaterialModel& operator=(const MaterialModel&);
  %extend {
    void copy(const MaterialModel& src) {self->operator=(src);}
  }
  
  // get constitutive model
  ConstitutiveModel& model() const;
  
  // set constitutive model
  void setModel(ConstitutiveModel&);
  
  // get material properties
  MaterialProperties& properties() const;
  
  // set material properties
  void setProperties(MaterialProperties&);
  
  // initialize material model
  void initialize(const char* = 0) throw (InvalidPropertyException, NoSuchPropertyException);
  
  // rotate material properties
  void rotateProperties(const Rotation&);
  
  // update material properties
  void updateProperties(const ParameterSet&);
  
  // initialize the state of the material
  void initState(MaterialState&);
  
  // update the state of the material (with the ability to compute tangents)
  void updateState(const ParameterSet&,const MaterialState&,MaterialState&,
                   double,MatLibMatrix&,bool);
  
  // compute material tangents (without updating)
  void computeTangent(const ParameterSet&,const MaterialState&,MaterialState&,
                      double,MatLibMatrix&);
};

/**
 * Interface for constitutive model builders
 */
class ModelBuilder {
  
 protected:
  
  // constructor
  ModelBuilder();
  
 public:
  
  // build model
  ConstitutiveModel* build(unsigned int) const;
};
%newobject ModelBuilder::build(unsigned int) const;

/**
 * Exception thrown when a model is not found in the dictionary.
 */
class NoSuchModelException : public ZException {
  
 public:
  
  // default constructor
  NoSuchModelException(const std::string& = "no such model");
  
  // copy constructor
  NoSuchModelException(const NoSuchModelException&);
};

/**
 * Constitutive model dictionary.
 */
class ModelDictionary {
  
 private:
  
  // private constructor
  ModelDictionary();
  
 public:
    
  // add model associated to keyword
  static void add(const std::string&,ModelBuilder&);
  
  // build model associated to keyword
  static ConstitutiveModel* build(const std::string&,unsigned int = 3)
    throw (NoSuchModelException);
  
  // get model associated to keyword
  static ModelBuilder& get(const std::string&)
    throw (NoSuchModelException);
  
  // list all models
  static void list(std::ostream&);
  %extend {
    static void list() {ModelDictionary::list(std::cout);}
  }
};
%newobject ModelDictionary::build(const std::string&,unsigned int = 3);


/**
 * Virtual base class for material criteria
 */
class MaterialCriterion {
  
 protected:
  
  // constructor
  MaterialCriterion() {}
  
 public:
  
  // check consistency of material properties
  void checkProperties(MaterialProperties&,const char* = 0) 
    throw (FileException, InvalidPropertyException, NoSuchPropertyException);
  
  // apply rotation to material properties
  void rotateProperties(MaterialProperties&,const Rotation&);
  
  // update properties in function of external parameters
  void updateProperties(MaterialProperties&,const ParameterSet&);
  
  // how many external variables ?
  unsigned int nExtVar() const;
  
  // how many internal variables ?
  unsigned int nIntVar() const;
  
  // evaluate criterion
  double evaluateCriterion(const MaterialProperties&,const ParameterSet&,
                           const MaterialState&,const MatLibMatrix&,double);
};

/**
 * Interface for material criterion builders
 */
class CriterionBuilder {
  
 protected:
  
  // constructor
  CriterionBuilder();
  
 public:
  
  // build model
  MaterialCriterion* build(unsigned int) const;
};
%newobject CriterionBuilder::build(unsigned int) const;


/**
 * Exception thrown when a criterion is not found in the dictionary.
 */
class NoSuchCriterionException : public ZException {
  
 public:

  // default constructor
  NoSuchCriterionException(const std::string& msg = "no such criterion");
  
  // copy constructor
  NoSuchCriterionException(const NoSuchCriterionException&);
};

/**
 * Material criterion dictionary.
 */
class CriterionDictionary {
  
 private:
  
  // private constructor
  CriterionDictionary();
  
 public:
  
  // add criterion associated to keyword
  static void add(const std::string&,CriterionBuilder&);
  
  // build criterion associated to keyword
  static MaterialCriterion* build(const std::string& key,unsigned int d = 3)
    throw (NoSuchCriterionException);
  
  // get model associated to keyword
  static CriterionBuilder& get(const std::string&)
    throw (NoSuchCriterionException);
  
  // list all models
  static void list(std::ostream&);
  %extend {
    static void list() {CriterionDictionary::list(std::cout);}
  }
};
%newobject CriterionDictionary::build(const std::string&,unsigned int = 3);
