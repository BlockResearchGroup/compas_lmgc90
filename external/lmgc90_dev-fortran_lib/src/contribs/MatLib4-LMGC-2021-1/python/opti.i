/*
 *  $Id: opti.i 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
%{
#include <opti/OptiMethod.h>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif
%}

/**
 * Generic class (interface) for objective functions and constraints.
 */
class OptiFunction : virtual public Cloneable {
 
 protected:
  
  // empty constructor
  OptiFunction() {}
  
  // copy constructor
  OptiFunction(const OptiFunction&) {}
  
 public:
  
  // clone operation
  virtual OptiFunction* clone() const = 0;
  
  // get dimension of definition space
  virtual unsigned int dimension() const = 0;
  
  // get value and derivatives
  virtual double value(const ShortArray&,
                       ShortArray* = 0,bool = false,
                       ShortSqrMatrix* = 0,bool = false) const = 0;
  double value(const ShortArray&,ShortArray&,ShortSqrMatrix&) const;
};


/**
 * Generic class (interface) for optimization problems.
 */
class OptiProblem : virtual public Cloneable {

 protected:
  
  // empty constructor
  OptiProblem() {}
  
  // copy constructor
  OptiProblem(const OptiProblem&) {}  

 public:
  
  // clone operation
  virtual OptiProblem* clone() const = 0;
  
  // get associated objective function
  virtual OptiFunction& objectiveFunction() const = 0;

  // set associated objective function
  virtual void setObjectiveFunction(const OptiFunction&) = 0;

  // get dimension of support space
  virtual unsigned int dimension() const = 0;

  // get bounds information
  virtual unsigned int nLower() const = 0;
  virtual unsigned int iLower(unsigned int) const = 0;
  virtual double vLower(unsigned int) const = 0;
  virtual unsigned int nUpper() const = 0;
  virtual unsigned int iUpper(unsigned int) const = 0;
  virtual double vUpper(unsigned int) const = 0;

  // set admissible domain
  void setBounds(unsigned int nLower,const std::vector<unsigned int> idxLower,
                 const std::vector<double> valLower,
                 unsigned int nUpper,const std::vector<unsigned int> idxUpper,
                 const std::vector<double> valUpper);

  // set lower bounds on design space
  virtual void setLowerBounds(unsigned int,const std::vector<unsigned int>,
                              const std::vector<double>) = 0;

  // set upper bounds on design space
  virtual void setUpperBounds(unsigned int,const std::vector<unsigned int>,
                              const std::vector<double>) = 0;

  // get equality constraints
  virtual unsigned int nEquality() const = 0;
  virtual const OptiFunction& cEquality(unsigned int) const = 0;

  // set equality constraints
  virtual void setEqualityConstraints(unsigned int,const std::vector<const OptiFunction*>) = 0;

  // get inequality constraints
  virtual unsigned int nInequality() const = 0;
  virtual const OptiFunction& cInequality(unsigned int) const = 0;

  // set inequality constraints
  virtual void setInequalityConstraints(unsigned int,const std::vector<const OptiFunction*>) = 0;
};


/**
 * Basic implementation of OptiProblem (enough for most cases).
 */
class BasicOptiProblem : virtual public OptiProblem {

 public:

  // constructor
  BasicOptiProblem(const OptiFunction&,
                   unsigned int = 0,const unsigned int[] = 0,const double[] = 0,
                   unsigned int = 0,const unsigned int[] = 0,const double[] = 0,
                   unsigned int = 0,const OptiFunction** = 0,
                   unsigned int = 0,const OptiFunction** = 0);

  // copy constructor
  BasicOptiProblem(const BasicOptiProblem&);

  // clone operation
  BasicOptiProblem* clone() const;

  // get associated objective function
  OptiFunction& objectiveFunction() const;

  // set associated objective function
  void setObjectiveFunction(const OptiFunction&);

  // get dimension of support space
  unsigned int dimension() const;

  // get bounds information
  unsigned int nLower() const;
  unsigned int iLower(unsigned int) const;
  double vLower(unsigned int) const;
  unsigned int nUpper() const;
  unsigned int iUpper(unsigned int) const;
  double vUpper(unsigned int) const;

  // set lower bounds on design space
  void setLowerBounds(unsigned int,const std::vector<unsigned int>,const std::vector<double>);

  // set upper bounds on design space
  void setUpperBounds(unsigned int,const std::vector<unsigned int>,const std::vector<double>);

  // get equality constraints
  unsigned int nEquality() const;
  const OptiFunction& cEquality(unsigned int n) const;

  // set equality constraints
  void setEqualityConstraints(unsigned int,const std::vector<const OptiFunction*>);

  // get inequality constraints
  unsigned int nInequality() const;
  const OptiFunction& cInequality(unsigned int n) const;

  // set inequality constraints
  void setInequalityConstraints(unsigned int,const std::vector<const OptiFunction*>);
};


/**
 * Thrown when optimum could not be found.
 */
class OptimizationException : public ZException {
  
 public:
  
  // default constructor
  OptimizationException(const std::string& = "");
  
  // copy constructor
  OptimizationException(const OptimizationException&);
};


/**
 * Generic class for optimization methods.
 */
class OptiMethod {

 public:

  // default tolerance
  static const double DEFAULT_TOL;

#ifdef WITH_OPTI_DEBUG
  // debug levels
  static const unsigned int DBG_LOW = 1;
  static const unsigned int DBG_MID = 2;
  static const unsigned int DBG_TOP = 3;
#endif

 protected:

  // constructor
  OptiMethod(OptiProblem&);
  
  // copy constructor
  OptiMethod(const OptiMethod&);

 public:

#ifdef WITH_OPTI_DEBUG
  // get debug level
  unsigned int debugLevel() const;

  // set debug level
  void setDebugLevel(unsigned int);
#endif

  // get associated problem dimension
  unsigned int dimension() const;

  // get associated problem
  OptiProblem& getProblem() const;
  
  // set associated problem
  virtual void setProblem(OptiProblem&);

  // solve problem, starting from the given point
  virtual double minimum(ShortArray&,double = DEFAULT_TOL)
   throw (OptimizationException) = 0;

  // line search
  double lineSearch(double,double,ShortArray&,
                    const ShortArray&,double = DEFAULT_TOL)
   throw (OptimizationException);

  // Armijo's rule
  double armijoRule(double,ShortArray&,const ShortArray&,double,double);
};

