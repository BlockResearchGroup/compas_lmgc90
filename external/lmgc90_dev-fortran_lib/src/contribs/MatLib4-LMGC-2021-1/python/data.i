/*
 *  $Id: data.i 231 2017-03-16 21:15:11Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
%{
#include <data/Chronometer.h>
#include <data/Copiable.h>
#include <data/FileException.h>
#include <data/Property.h>
#include <data/Rotation2D.h>
#include <data/Rotation3D.h>
#include <data/ShortSqrMatrix.h>
#include <data/ConstantFunction.h>
#include <data/TabulatedFunction2.h>
#include <data/SyntaxError.h>
  
#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif
%}


/***************
   Chronometer
 ***************/

/**
 * Class for CPU chronometer.
 */
class Chronometer {

 public:

  // constructor
  Chronometer();

  // copy constructor
  Chronometer(const Chronometer&);

  // destructor
  ~Chronometer() {}

  // start chrono
  void start();

  // stop chrono
  void stop();

  // reset chrono
  void reset();

  // elapsed time
  unsigned long long elapsed();

  // format elapsed time
  static std::string toString(unsigned long long);
};

#ifdef _OPENMP
/**
 * Class for OpenMP-compatible chronometer.
 */
class OMPChronometer {
  
 public:
  
  // constructor
  OMPChronometer();
  
  // copy constructor
  OMPChronometer(const OMPChronometer&);
  
  // destructor
  ~OMPChronometer() {}
  
  // start chrono
  void start();
  
  // stop chrono
  void stop();
  
  // reset chrono
  void reset();
  
  // elapsed time
  double elapsed();
  
  // format elapsed time
  static std::string toString(double);
};
#endif


/*************************
   Exceptions and errors
 *************************/

/**
 * Base class for Zorglib exceptions.
 */
class ZException {
  
 public:
  
  // constructor
  ZException(const std::string&);
  
  // copy constructor
  ZException(const ZException&);
  
  // get message
  std::string mesg() const;
};


/**
 * Base class for Zorglib errors.
 */
class ZError {
  
 public:
  
  // constructor
  ZError(const std::string&);
  
  // copy constructor
  ZError(const ZError&);
  
  // get message
  std::string mesg() const;
};

/**
 * Thrown when a file is not found or cannot be opened.
 */
class FileException : public ZException {
  
 public:
  
  // default constructor
  FileException(const std::string& = "");
  
  // copy constructor
  FileException(const FileException&);
};

/**
 * Syntax error in an input stream we are parsing.
 */
class SyntaxError : public ZError {
  
 public:
  
  // default constructor
  SyntaxError(const std::string& = "");
  
  // copy constructor
  SyntaxError(const SyntaxError&);
};


/***********************
   Cloneable interface
 ***********************

/**
 * Interface for cloneable objects.
 */
class Cloneable {
  
 public:
  
  // clone operation
  Cloneable* clone() const = 0;
};


/**********************
   Copiable interface
 **********************

/**
 * Interface for copiable objects.
 */
class Copiable {
  
 public:
  
  // copy operation
  void copy(const Copiable&) = 0;
};


/*************
   Functions
 *************/

/**
 * Base class for functions.
 */
class Function : virtual public Cloneable {
  
 public:
  
  // constructor
  Function(const std::string& = "no name");
  
  // copy constructor
  Function(const Function&);
  
  // duplicate object
  Function* clone() const = 0;
  
  // get the function's name
  std::string getName() const;
  
  // get value
  double value(double) = 0;
  
  // get derivative
  double slope(double) = 0;
  
  // get value and derivative
  double value(double,double&) = 0;
  
  // print-out
  std::string toString() const = 0;
  %extend {
    char* __str__() {
      static char tmp[1024];
      std::strcpy(tmp,self->toString().c_str());
      return tmp;
    }
  }
};

/**
 * Base class for functions for which a curvature can be computed.
 * It normally implies C1 continuity.
 */
class Function2 : virtual public Function {
  
 public:
  
  // duplicate object
  Function2* clone() const = 0;
  
  // get value
  double value(double) = 0;
  
  // get derivative
  double slope(double) = 0;
  
  // get curvature
  double curvature(double) = 0;
  
  // get value and derivative
  double value(double,double&) = 0;
  
  // get value and derivatives
  double value(double,double&,double&) = 0;
};

/**
 * Constant functions.
 */
class ConstantFunction : virtual public Function2 {
  
 public:
  
  // default constructor
  ConstantFunction(const std::string& = "no name",double = 0.0);
  
  // copy constructor
  ConstantFunction(const ConstantFunction&);
  
  // duplicate object
  ConstantFunction* clone() const;
  
  // get value
  double value(double);
  
  // get derivative
  double slope(double);
  
  // get curvature
  double curvature(double);
  
  // get value and derivative
  double value(double,double&);
  
  // get value and derivatives
  double value(double,double&,double&);
  
  // print-out
  std::string toString() const;
};

/**
 * Tabulated functions.
 */
class TabulatedFunction : virtual public Function {
  
 public:
    
  // default constructor
  TabulatedFunction(const std::string& = "no name",unsigned int = 0);
  
  // constructor
  TabulatedFunction(const std::string&,unsigned int,double*,double*);
  
  // copy constructor
  TabulatedFunction(const TabulatedFunction&);
  
  // duplicate object
  TabulatedFunction* clone() const;

  // resize
  void resize(unsigned int);
  
  // get number of points
  unsigned int nPoints() const;
  
  // get point of given index
  std::pair<double,double> getPoint(unsigned int) const;

  // set value
  void setPoint(unsigned int,double,double);
  
  // get value
  double value(double);
  
  // get derivative
  double slope(double);
  
  // get value and derivative
  double value(double,double&);
  
  // print-out
  std::string toString() const;
};

/**
 * Tabulated functions (with C1-continuity).
 */
class TabulatedFunction2 : virtual public TabulatedFunction,
                           virtual public Function2 {
  
 public:
    
  // default constructor
  TabulatedFunction2(const std::string& = "no name",unsigned int = 0);
  
  // constructor
  TabulatedFunction2(const std::string&,unsigned int,double*,double*);
  
  // copy constructor
  TabulatedFunction2(const TabulatedFunction2&);
  
  // duplicate object
  TabulatedFunction2* clone() const;
  
  // resize
  void resize(unsigned int);
  
  // set value
  void setPoint(unsigned int,double,double);
  
  // get value
  double value(double);
  
  // get derivative
  double slope(double);
  
  // get curvature
  double curvature(double);
  
  // get value and derivative
  double value(double,double&);
  
  // get value and derivatives
  double value(double,double&,double&);
};


// allow exception handling in python
%catches(ZError) PythonFunction::PythonFunction();
%catches(ZError) PythonFunction::value(double);
%catches(ZError) PythonFunction::slope(double);

%exception {
  try {
    $action
  }
  catch (ZError e) {
    PyErr_SetString(PyExc_TypeError,e.mesg().c_str());
    return 0;
  }
  catch (ZException) {
    return 0;
  }
}

%inline %{
  
/**
 * Base class for functions defined in python.
 */
class PythonFunction : virtual public Function2 {
  
 private:
  
  PyObject* pyfct;
  
 public:
  
  // constructor
  PythonFunction(PyObject* f,const std::string& name = "undefined") 
  : Function(name) {
    if (!PyCallable_Check(f))
      throw ZError("first argument must be a callable python object.");
    pyfct = f; Py_INCREF(f);
  }
  
  // copy constructor
  PythonFunction(const PythonFunction& src)
  : Function(src) {
    pyfct = src.pyfct; Py_INCREF(src.pyfct);
  }
  
  // destructor
  virtual ~PythonFunction() {Py_DECREF(pyfct);}
  
  // duplicate object
  PythonFunction* clone() const {
    return new PythonFunction(*this);
  }
  
  // get value
  double value(double x) {
    PyObject *arglist;
    arglist = Py_BuildValue("(d)",x);
    PyObject *result;
    result = PyEval_CallObject(pyfct,arglist);
    Py_DECREF(arglist);
    if (!result) throw ZException("invalid python function call");
    double val = 0.0e0;
    if (PyFloat_Check(result))
      val = PyFloat_AsDouble(result);
    else if (PyTuple_Check(result) && PyTuple_GET_SIZE(result) > 0)
      val = PyFloat_AsDouble(PyTuple_GET_ITEM(result,0));
    else
      throw ZError("python function must return a float or float tuple");
    Py_DECREF(result);
    return val;
  }
  
  // get derivative
  double slope(double x) {
    PyObject *arglist;
    arglist = Py_BuildValue("(d)",x);
    PyObject *result;
    result = PyEval_CallObject(pyfct,arglist);
    Py_DECREF(arglist);
    if (!result) throw ZException("invalid python function call");
    double val = 0.0e0;
    if (PyTuple_Check(result) && PyTuple_GET_SIZE(result) > 1)
      val = PyFloat_AsDouble(PyTuple_GET_ITEM(result,1));
    else
      throw ZError("python function must return a float tuple of size 2");
    Py_DECREF(result);
    return val;
  }
  
  // get curvature
  double curvature(double x) {
    PyObject *arglist;
    arglist = Py_BuildValue("(d)",x);
    PyObject *result;
    result = PyEval_CallObject(pyfct,arglist);
    Py_DECREF(arglist);
    if (!result) throw ZException("invalid python function call");
    double val = 0.0e0;
    if (PyTuple_Check(result) && PyTuple_GET_SIZE(result) > 2)
      val = PyFloat_AsDouble(PyTuple_GET_ITEM(result,2));
    else
      throw ZError("python function must return a float tuple of size 3");
    Py_DECREF(result);
    return val;
  }
  
  // get value and derivative
#ifdef SWIG
  %ignore PythonFunction::value(double,double&);
#endif
  double value(double x,double& df) {
    PyObject *arglist;
    arglist = Py_BuildValue("(d)",x);
    PyObject *result;
    result = PyEval_CallObject(pyfct,arglist);
    Py_DECREF(arglist);
    if (!result) throw ZException("invalid python function call");
    double val = 0.0e0;
    df = 0.0e0;
    if (PyFloat_Check(result)) {
      val = PyFloat_AsDouble(result);
      df  = 0.0e0;
    }
    else if (PyTuple_Check(result) && PyTuple_GET_SIZE(result) > 1) {
      val = PyFloat_AsDouble(PyTuple_GET_ITEM(result,0));
      df  = PyFloat_AsDouble(PyTuple_GET_ITEM(result,1));
    }
    else
      throw ZError("python function must return a float or float tuple of size 2");
    Py_DECREF(result);
    return val;
  }
  
  // get value and derivatives
#ifdef SWIG
  %ignore PythonFunction::value(double,double&,double&);
#endif
  double value(double x,double& df,double& d2f) {
    PyObject *arglist;
    arglist = Py_BuildValue("(d)",x);
    PyObject *result;
    result = PyEval_CallObject(pyfct,arglist);
    Py_DECREF(arglist);
    if (!result) throw ZException("invalid python function call");
    double val = 0.0e0;
    df = 0.0e0;
    if (PyFloat_Check(result)) {
      val = PyFloat_AsDouble(result);
      df  = 0.0e0;
      d2f  = 0.0e0;
    }
    else if (PyTuple_Check(result) && PyTuple_GET_SIZE(result) > 1) {
      val  = PyFloat_AsDouble(PyTuple_GetItem(result,0));
      df   = PyFloat_AsDouble(PyTuple_GetItem(result,1));
      if (PyTuple_GET_SIZE(result) > 2)
        d2f  = PyFloat_AsDouble(PyTuple_GetItem(result,2));
      else
        d2f = 0.0e0;
    }
    else
      throw ZError("python function must return a float or float tuple");
    Py_DECREF(result);
    return val;
  }
  
  // print-out
  std::string toString() const {return getName();}
};

%}

%exception;


/**************
   Properties
 **************/

/**
 * Base class for multi-type properties.
 */
class Property : virtual public Cloneable {
  
 public:
  
  // duplicate object
  Property* clone() const = 0;
  
  // output as a string
  std::string toString() const = 0;
  %extend {
    char* __str__() {
      static char tmp[1024];
      std::strcpy(tmp,self->toString().c_str());
      return tmp;
    }
  }
};

/**
 * Integer-valued property.
 */
class IntegerProperty : public Property {
  
 public:
  
  // default constructor
  IntegerProperty(int = 0);
  
  // copy constructor
  IntegerProperty(const IntegerProperty&);
  
  // get value
  int value() const;
  
  // set value
  void setValue(int);
  
  // duplicate object
  IntegerProperty* clone() const;
  
  // output as a string
  std::string toString() const;
};

/**
 * Real-valued (double precision) property.
 */
class DoubleProperty : public Property {
  
 public:
  
  // default constructor
  DoubleProperty(double = 0.e0);
  
  // copy constructor
  DoubleProperty(const DoubleProperty&);
  
  // get value
  double value() const;
  
  // set value
  void setValue(double);
  
  // duplicate object
  DoubleProperty* clone() const;
  
  // output as a string
  std::string toString() const;
};

/**
 * String-valued (character string) property.
 */
class StringProperty : public Property {
  
 public:
  
  // default constructor
  StringProperty(const char*);
  
  // copy constructor
  StringProperty(const StringProperty&);
  
  // get value
  std::string value() const;
  
  // set value
  void setValue(const char*);
  
  // duplicate object
  StringProperty* clone() const;
  
  // output as a string
  std::string toString() const;
};

/**
 * Function-valued property.
 */
class FunctionProperty : public Property {
  
 public:
  
  // default constructor
  FunctionProperty(Function&);
  
  // copy constructor
  FunctionProperty(const FunctionProperty&);
  
  // get value
  Function& function() const;
  
  // set value
  void setFunction(Function&);
  
  // duplicate object
  FunctionProperty* clone() const;
  
  // output as a string
  std::string toString() const;
};


/**
 * Define new type PropertyTable
 */
typedef StringMap<Property*>::Type PropertyTable;


/***********************
   Arrays and Matrices
 ***********************/

/**
 * Base class for expressions returning (short) arrays.
 */
class ShortArrayExpression {
  
 public:
  
  // size
  unsigned int size() const = 0;
    
  // expression operator
  %ignore operator[];
  double operator[](unsigned int) const = 0;
  %extend {
    double operator()(unsigned int i) const {return self->operator[](i);}
  }

  // print to string object
  std::string toString() const;
  %extend {
    char* __str__() {
      static char tmp[1024];
      std::strcpy(tmp,self->toString().c_str());
      return tmp;
    }
  }
};

/**
 * Class for short arrays.
 */
class ShortArray {
  
 public:
    
  // constructor
  ShortArray(unsigned int = 0);
  
  // subarray constructor
  ShortArray(ShortArray&,unsigned int,unsigned int = 0);
  
  // copy constructor
  ShortArray(const ShortArray&);
  ShortArray(const ShortArrayExpression&);

  // assignment operator
  %ignore operator=;
  ShortArray& operator=(const ShortArray&) throw (std::range_error);
  %extend {
    void copy(const ShortArray& src) throw (std::range_error) {self->operator=(src);}
  }
  ShortArray& operator=(const ShortArrayExpression&) throw (std::range_error);
  %extend {
    void copy(const ShortArrayExpression& src) throw (std::range_error) {self->operator=(src);}
  }
  ShortArray& operator=(double);
  %extend {
    void set(double val) {self->operator=(val);}
  }
  
  // unary operators
  ShortArray& operator+=(const ShortArray&) throw (std::range_error);
  ShortArray& operator+=(const ShortArrayExpression&) throw (std::range_error);
  ShortArray& operator-=(const ShortArray&) throw (std::range_error);
  ShortArray& operator-=(const ShortArrayExpression&) throw (std::range_error);
  ShortArray& operator*=(double);
  ShortArray& operator/=(double);
  
  // size
  unsigned int size() const;
  
  // resize
  void resize(unsigned int);
  
  // access operators
  double operator()(unsigned int) const throw (std::out_of_range);
  %extend {
    void set(unsigned int i,double val) throw (std::out_of_range) {self->operator()(i) = val;}
  }
  
  // print to string object
  std::string toString() const;
  %extend {
    char* __str__() {
      static char tmp[1024];
      std::strcpy(tmp,self->toString().c_str());
      return tmp;
    }
  }
};

/**
 * ShortArray sums and differences.
 */
class ShortArraySum : public ShortArrayExpression {
  
 public:
  
  // constructor
  ShortArraySum(const ShortArray&,const ShortArray&);
  
  // copy constructor
  ShortArraySum(const ShortArraySum&);
  
  // size
  unsigned int size() const;
  
  // expression operator
  %ignore operator[];
  double operator[](unsigned int) const;
  %extend {
    double operator()(unsigned int i) const {return self->operator[](i);}
  }
};

class ShortArraySum1 : public ShortArrayExpression {
  
 public:
    
  // constructor
  ShortArraySum1(const ShortArray&,const ShortArrayExpression&);
  
  // copy constructor
  ShortArraySum1(const ShortArraySum1&);
  
  // size
  unsigned int size() const;
  
  // expression operator
  %ignore operator[];
  double operator[](unsigned int) const;
  %extend {
    double operator()(unsigned int i) const {return self->operator[](i);}
  }
};

class ShortArraySum2 : public ShortArrayExpression {
  
 public:
  
  // constructor
  ShortArraySum2(const ShortArrayExpression&,const ShortArrayExpression&);
  
  // copy constructor
  ShortArraySum2(const ShortArraySum2&);
  
  // size
  unsigned int size() const;
  
  // expression operator
  %ignore operator[];
  double operator[](unsigned int) const;
  %extend {
    double operator()(unsigned int i) const {return self->operator[](i);}
  }
};

%extend ShortArray {
  ShortArraySum __add__(const ShortArray& other) const {
    return (*self)+other;
  }
  ShortArraySum1 __add__(const ShortArrayExpression& other) const {
    return (*self)+other;
  }
};

%extend ShortArrayExpression {
  ShortArraySum1 __add__(const ShortArray& other) const {
    return (*self)+other;
  }
  ShortArraySum2 __add__(const ShortArrayExpression& other) const {
    return (*self)+other;
  }
};

class ShortArrayDifference : public ShortArrayExpression {
  
 public:
  
  // constructor
  ShortArrayDifference(const ShortArray&,const ShortArray&);
  
  // copy constructor
  ShortArrayDifference(const ShortArrayDifference&);
  
  // size
  unsigned int size() const;
  
  // expression operator
  %ignore operator[];
  double operator[](unsigned int) const;
  %extend {
    double operator()(unsigned int i) const {return self->operator[](i);}
  }
};

class ShortArrayDifference1 : public ShortArrayExpression {
  
 public:
    
  // constructor
  ShortArrayDifference1(const ShortArray&,const ShortArrayExpression&);
  
  // copy constructor
  ShortArrayDifference1(const ShortArrayDifference1&);
  
  // size
  unsigned int size() const;
  
  // expression operator
  %ignore operator[];
  double operator[](unsigned int) const;
  %extend {
    double operator()(unsigned int i) const {return self->operator[](i);}
  }
};

class ShortArrayDifference2 : public ShortArrayExpression {
  
 public:
    
  // constructor
  ShortArrayDifference2(const ShortArrayExpression&,const ShortArray&);
  
  // copy constructor
  ShortArrayDifference2(const ShortArrayDifference2&);
  
  // size
  unsigned int size() const;
  
  // expression operator
  %ignore operator[];
  double operator[](unsigned int) const;
  %extend {
    double operator()(unsigned int i) const {return self->operator[](i);}
  }
};

class ShortArrayDifference3 : public ShortArrayExpression {
  
 public:
    
  // constructor
  ShortArrayDifference3(const ShortArrayExpression&,const ShortArrayExpression&);
  
  // copy constructor
  ShortArrayDifference3(const ShortArrayDifference3&);
  
  // size
  unsigned int size() const;
  
  // expression operator
  %ignore operator[];
  double operator[](unsigned int) const;
  %extend {
    double operator()(unsigned int i) const {return self->operator[](i);}
  }
};

%extend ShortArray {
  ShortArrayDifference __sub__(const ShortArray& other) const {
    return (*self)-other;
  }
  ShortArrayDifference1 __sub__(const ShortArrayExpression& other) const {
    return (*self)-other;
  }
};

%extend ShortArrayExpression {
  ShortArrayDifference2 __sub__(const ShortArray& other) const {
    return (*self)-other;
  }
  ShortArrayDifference3 __sub__(const ShortArrayExpression& other) const {
    return (*self)-other;
  }
};

/**
 * Array products.
 */
class ShortArrayScalarProduct : public ShortArrayExpression {
  
 public:
    
  // constructor
  ShortArrayScalarProduct(const ShortArray&,double);
  
  // copy constructor
  ShortArrayScalarProduct(const ShortArrayScalarProduct&);
  
  // size
  unsigned int size() const;
  
  // expression operator
  %ignore operator[];
  double operator[](unsigned int) const;
  %extend {
    double operator()(unsigned int i) const {return self->operator[](i);}
  }
};

class ShortArrayScalarProduct1 : public ShortArrayExpression {
  
 public:
    
  // constructor
  ShortArrayScalarProduct1(const ShortArrayExpression&,double);
  
  // copy constructor
  ShortArrayScalarProduct1(const ShortArrayScalarProduct1&);
  
  // size
  unsigned int size() const;
  
  // expression operator
  %ignore operator[];
  double operator[](unsigned int) const;
  %extend {
    double operator()(unsigned int i) const {return self->operator[](i);}
  }
};

%extend ShortArray {
  ShortArrayScalarProduct __div__(double f) const {
    return (*self)/f;
  }  
  ShortArrayScalarProduct __mul__(double f) const {
    return f*(*self);
  }
  ShortArrayScalarProduct __neg__() {
    return -1.*(*self);
  }
};
%extend ShortArrayExpression {
  ShortArrayScalarProduct1 __div__(double f) const {
    return (*self)/f;
  }
  ShortArrayScalarProduct1 __mul__(double f) const {
    return f*(*self);
  }
  ShortArrayScalarProduct1 __neg__() const {
    return -1.*(*self);
  }
};
%inline %{
  ShortArrayScalarProduct mul(double f,const ShortArray& a) {
    return f*a;
  }
  ShortArrayScalarProduct1 mul(double f,const ShortArrayExpression& a) {
    return f*a;
  }
%}

// define inner product
double innerProd(const ShortArray&,const ShortArray&) throw (std::range_error);

// compute norm of an array
double normL1(const ShortArray&);
double normL2(const ShortArray&);

// compute max absolute value
double normLInf(const ShortArray&);


/**
 * Base class for expressions returning (short) matrices.
 */
class ShortMatrixExpression {
  
 public:
  
  // matrix size
  unsigned int nCols() const = 0;
  unsigned int nRows() const = 0;
    
  // expression operator
  double operator()(unsigned int,unsigned int) const = 0;

  // print to string object
  std::string toString() const;
  %extend {
    char* __str__() {
      static char tmp[16384];
      std::strcpy(tmp,self->toString().c_str());
      return tmp;
    }
  }
};

/**
 * Class for small, full matrices.
 */
class ShortMatrix {

 public:
  
  // constructor
  ShortMatrix(unsigned int = 0,unsigned int = 1);
  
  // submatrix constructor
  ShortMatrix(const ShortMatrix&,unsigned int,unsigned int,unsigned int = 0,unsigned int = 0);
  
  // copy constructor
  ShortMatrix(const ShortMatrix&);
  ShortMatrix(const ShortMatrixExpression&);
  
  // assignment operator
  %ignore operator=;
  ShortMatrix& operator=(const ShortMatrix&) throw (std::range_error);
  %extend {
    void copy(const ShortMatrix& src) throw (std::range_error) {self->operator=(src);}
  }
  ShortMatrix& operator=(const ShortMatrixExpression&) throw (std::range_error);
  %extend {
    void copy(const ShortMatrixExpression& src) throw (std::range_error) {
      self->operator=(src);
    }
  }
  ShortMatrix& operator=(double val);
  %extend {
    void set(double val) {self->operator=(val);}
  }

  // unary operators
  ShortMatrix& operator+=(const ShortMatrix&) throw (std::range_error);
  ShortMatrix& operator+=(const ShortMatrixExpression&) throw (std::range_error);
  ShortMatrix& operator-=(const ShortMatrix&) throw (std::range_error);
  ShortMatrix& operator-=(const ShortMatrixExpression&) throw (std::range_error);
  ShortMatrix& operator*=(double);
  ShortMatrix& operator/=(double);
  
  // size
  unsigned int nRows() const;
  unsigned int nCols() const;
  
  // access operators
  double operator()(unsigned int i,unsigned int j) const throw (std::out_of_range);
  %extend {
    void set(unsigned int i,unsigned int j,double val) throw (std::out_of_range) {
      self->operator()(i,j) = val;
    }
  }
  
  // print to string object
  std::string toString() const;
  %extend {
    char* __str__() {
      static char tmp[16384];
      std::strcpy(tmp,self->toString().c_str());
      return tmp;
    }
  }
  
  // build matrix by vector outer product
  static void outerProd(const ShortArray&,const ShortArray&,ShortMatrix&);
};

/**
 * ShortMatrix sums and differences.
 */
class ShortMatrixSum : public ShortMatrixExpression {
  
 public:
  
  // constructor
  ShortMatrixSum(const ShortMatrix&,const ShortMatrix&);
  
  // copy constructor
  ShortMatrixSum(const ShortMatrixSum&);
  
  // matrix size
  unsigned int nCols() const;
  unsigned int nRows() const;
  
  // expression operator
  double operator()(unsigned int,unsigned int) const;
};

class ShortMatrixSum1 : public ShortMatrixExpression {
  
 public:
    
  // constructor
  ShortMatrixSum1(const ShortMatrix&,const ShortMatrixExpression&);
  
  // copy constructor
  ShortMatrixSum1(const ShortMatrixSum1&);
  
  // matrix size
  unsigned int nCols() const;
  unsigned int nRows() const;
  
  // expression operator
  double operator()(unsigned int,unsigned int) const;
};

class ShortMatrixSum2 : public ShortMatrixExpression {
  
 public:
    
  // constructor
  ShortMatrixSum2(const ShortMatrixExpression&,const ShortMatrixExpression&);
  
  // copy constructor
  ShortMatrixSum2(const ShortMatrixSum2&);
  
  // matrix size
  unsigned int nCols() const;
  unsigned int nRows() const;
  
  // expression operator
  double operator()(unsigned int,unsigned int) const;
};

%extend ShortMatrix {
  ShortMatrixSum __add__(const ShortMatrix& other) const {
    return (*self)+other;
  }
  ShortMatrixSum1 __add__(const ShortMatrixExpression& other) const {
    return (*self)+other;
  }
};
%extend ShortMatrixExpression {
  ShortMatrixSum1 __add__(const ShortMatrix& other) const {
    return (*self)+other;
  }
  ShortMatrixSum2 __add__(const ShortMatrixExpression& other) const {
    return (*self)+other;
  }
};

class ShortMatrixDifference : public ShortMatrixExpression {
  
 public:
  
  // constructor
  ShortMatrixDifference(const ShortMatrix&,const ShortMatrix&);
  
  // copy constructor
  ShortMatrixDifference(const ShortMatrixDifference&);
  
  // matrix size
  unsigned int nCols() const;
  unsigned int nRows() const;
  
  // expression operator
  double operator()(unsigned int,unsigned int) const;
};

class ShortMatrixDifference1 : public ShortMatrixExpression {
  
 public:
    
  // constructor
  ShortMatrixDifference1(const ShortMatrix&,const ShortMatrixExpression&);
  
  // copy constructor
  ShortMatrixDifference1(const ShortMatrixDifference1&);
  
  // matrix size
  unsigned int nCols() const;
  unsigned int nRows() const;
  
  // expression operator
  double operator()(unsigned int,unsigned int) const;
};

class ShortMatrixDifference2 : public ShortMatrixExpression {
  
 public:
    
  // constructor
  ShortMatrixDifference2(const ShortMatrixExpression&,const ShortMatrix&);
  
  // copy constructor
  ShortMatrixDifference2(const ShortMatrixDifference2&);
  
  // matrix size
  unsigned int nCols() const;
  unsigned int nRows() const;
  
  // expression operator
  double operator()(unsigned int,unsigned int) const;
};

class ShortMatrixDifference3 : public ShortMatrixExpression {
  
 public:
  
  // constructor
  ShortMatrixDifference3(const ShortMatrixExpression&,const ShortMatrixExpression&);
  
  // copy constructor
  ShortMatrixDifference3(const ShortMatrixDifference3&);
  
  // matrix size
  unsigned int nCols() const;
  unsigned int nRows() const;
  
  // expression operator
  double operator()(unsigned int,unsigned int) const;
};

%extend ShortMatrix {
  ShortMatrixDifference __sub__(const ShortMatrix& other) const {
    return (*self)-other;
  }
  ShortMatrixDifference1 __sub__(const ShortMatrixExpression& other) const {
    return (*self)-other;
  }
};
%extend ShortMatrixExpression {
  ShortMatrixDifference2 __sub__(const ShortMatrix& other) const {
    return (*self)-other;
  }
  ShortMatrixDifference3 __sub__(const ShortMatrixExpression& other) const {
    return (*self)-other;
  }
};

/**
 * Matrix products.
 */
class ShortMatrixScalarProduct : public ShortMatrixExpression {
  
 public:
    
  // constructor
  ShortMatrixScalarProduct(const ShortMatrix&,double);
  
  // copy constructor
  ShortMatrixScalarProduct(const ShortMatrixScalarProduct&);
  
  // matrix size
  unsigned int nCols() const;
  unsigned int nRows() const;
  
  // expression operator
  double operator()(unsigned int,unsigned int) const;
};

class ShortMatrixScalarProduct1 : public ShortMatrixExpression {
  
 public:
    
  // constructor
  ShortMatrixScalarProduct1(const ShortMatrixExpression&,double) ;
  
  // copy constructor
  ShortMatrixScalarProduct1(const ShortMatrixScalarProduct1&);
  
  // matrix size
  unsigned int nCols() const;
  unsigned int nRows() const;
  
  // expression operator
  double operator()(unsigned int,unsigned int) const;
};

%extend ShortMatrix {
  ShortMatrixScalarProduct __div__(double f) const {
    return (*self)/f;
  }
  ShortMatrixScalarProduct __mul__(double f) const {
    return f*(*self);
  }
  ShortMatrixScalarProduct __neg__() const {
    return -1.*(*self);
  }
};
%extend ShortMatrixExpression {
  ShortMatrixScalarProduct1 __div__(double f) const {
    return (*self)/f;
  }
  ShortMatrixScalarProduct1 __mul__(double f) const {
    return f*(*self);
  }
  ShortMatrixScalarProduct1 __neg__() const {
    return -1.*(*self);
  }
};

%inline %{
  ShortMatrixScalarProduct mul(double f,const ShortMatrix& a) {
    return f*a;
  }
  ShortMatrixScalarProduct1 mul(double f,const ShortMatrixExpression& a) {
    return f*a;
  }
%}

// define outer product
class ShortArrayOuterProduct : public ShortMatrixExpression {
  
 public:
  
  // constructor
  ShortArrayOuterProduct(const ShortArray&,const ShortArray&);
  
  // copy constructor
  ShortArrayOuterProduct(const ShortArrayOuterProduct&);
  
  // matrix size
  unsigned int nCols() const;
  unsigned int nRows() const;
  
  // expression operator
  double operator()(unsigned int,unsigned int) const;
};

ShortArrayOuterProduct outerProd(const ShortArray&,const ShortArray&);

%extend ShortMatrix {
  // simple matrix*array product
  ShortArray __mul__(const ShortArray& a) const {
    return (*self)*a;
  }
  // simple matrix*matrix product
  ShortMatrix __mul__(const ShortMatrix& M) const {
    return (*self)*M;
  }
};


/**
 * Exception thrown when attempting to factorize a singular matrix.
 */
class SingularMatrixException : public ZException {
  
 public:
  
  // default constructor
  SingularMatrixException(const std::string& = "singular matrix");
  
  // copy constructor
  SingularMatrixException(const SingularMatrixException&);
};

// instantiate templates used by ShortSqrMatrix class
namespace std {
  %template(BooleanVector) vector<bool>;
}

/**
 * Class for small, full, square matrices.
 */
class ShortSqrMatrix : virtual public ShortMatrix { 

 public:
    
  // constructor
  ShortSqrMatrix(unsigned int = 0);
  
  // submatrix constructor
  ShortSqrMatrix(const ShortSqrMatrix&,unsigned int,unsigned int = 0);
  
  // copy constructor
  ShortSqrMatrix(const ShortSqrMatrix&);
  
  // assignment operator
  ShortSqrMatrix& operator=(const ShortMatrix&);
  ShortSqrMatrix& operator=(const ShortMatrixExpression&);
  ShortSqrMatrix& operator=(double);
  
  // size
  unsigned int size() const;
    
  // resize
  void resize(unsigned int);
  
  // invert (in place)
  void invert() throw (SingularMatrixException);

  // factorize (LU)
  void factorize(bool) throw (SingularMatrixException);
  
  // back-substitute
  void backsubstitute(ShortArray&,const ShortArray&) const;
  
  // solve linear system
  void solve(ShortArray&,const ShortArray&,bool=false)
    throw (SingularMatrixException);
  
  // solve a linear system known to be symmetric
  // (will be modified if not definite positive)
  bool symSolve(ShortArray&,const ShortArray&,std::vector<bool>&,
                double = 0.e0,double = 0.e0);
  
  // get identity matrix
  static ShortSqrMatrix identity(unsigned int);
};


/*************
   Rotations
 *************/

/**
 * Class describing a rotation operation.
 */
class Rotation {
  
 protected:
  
  // constructor
  Rotation() {}
  
 public:
  
  // export to matrix
  virtual void toMatrix(ShortSqrMatrix&) const = 0;
  
  // export to tensor
  virtual void toTensor(ShortArray&) const = 0;
};

/**
 * Class for 2D rotations.
 */
class Rotation2D : virtual public Rotation {
  
 public:
  
  // constructor
  Rotation2D(double = 0.e0);
    
  // constructor (from rotation matrix)
  Rotation2D(const ShortSqrMatrix&);

  // copy constructor
  Rotation2D(const Rotation2D&);
  
  // export to matrix
  void toMatrix(ShortSqrMatrix&) const;
  
  // export to tensor
  void toTensor(ShortArray&) const;
};

/**
 * Class for 3D rotations.
 */
class Rotation3D : virtual public Rotation {
  
 public:
  
  // types of parameterization
  enum Type {BUNGE,KOCKS};
  
 public:
  
  // constructor (from Euler angles)
  Rotation3D(double,double,double,Type = BUNGE);
  
  // constructor (from Euler parameters)
  Rotation3D(const ShortArray&);
  
  // constructor (from rotation matrix)
  Rotation3D(const ShortSqrMatrix&);
  
  // copy constructor
  Rotation3D(const Rotation3D&);
  
  // export to Euler angles
  void toEulerAngles(double&,double&,double&,Type = BUNGE) const;
  
  // export to matrix
  void toMatrix(ShortSqrMatrix&) const;
  
  // export to tensor
  void toTensor(ShortArray&) const;
  
  // from Euler parameters to CRV representation
  static void eul2crv(const ShortArray&,ShortArray&);
  
  // from matrix representation to Euler parameters
  static void euler(const ShortSqrMatrix&,ShortArray&);
  
  // from vector to matrix representation
  static void spin(const ShortArray&,ShortSqrMatrix&);
};
