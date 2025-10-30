/*
 *  $Id: OptiProblem.h 129 2013-04-05 05:15:49Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_OPTI_PROBLEM_H
#define ZORGLIB_OPTI_PROBLEM_H

// config
#include <matlib_macros.h>

// STL
#include <vector>
// local
#ifndef WITH_MATLIB_H
#include <opti/OptiFunction.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

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

  // destructor
  virtual ~OptiProblem() {}
  
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
                 const std::vector<double> valUpper) {
    setLowerBounds(nLower,idxLower,valLower);
    setUpperBounds(nUpper,idxUpper,valUpper);
  }

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

 private:

  // associated objective function
  OptiFunction* objective;

  // bounds
  std::vector<unsigned int> idxLower;
  std::vector<unsigned int> idxUpper;
  std::vector<double> valLower;
  std::vector<double> valUpper;

  // equality constraints
  std::vector<const OptiFunction*> cEqua;

  // inequality constraints
  std::vector<const OptiFunction*> cIneq;

 public:

  // constructor
  BasicOptiProblem(const OptiFunction&,
                   unsigned int = 0,const unsigned int[] = 0,const double[] = 0,
                   unsigned int = 0,const unsigned int[] = 0,const double[] = 0,
                   unsigned int = 0,const OptiFunction** = 0,
                   unsigned int = 0,const OptiFunction** = 0);

  // copy constructor
  BasicOptiProblem(const BasicOptiProblem&);

  // destructor
  virtual ~BasicOptiProblem();

  // clone operation
  BasicOptiProblem* clone() const {return new BasicOptiProblem(*this);}

  // get associated objective function
  OptiFunction& objectiveFunction() const {
    return *objective;
  }

  // set associated objective function
  void setObjectiveFunction(const OptiFunction& objF) {
    if (objective) delete objective;
    objective = objF.clone();
  }

  // get dimension of support space
  unsigned int dimension() const {
    return objective->dimension();
  }

  // get bounds information
  unsigned int nLower() const {return idxLower.size();}
  unsigned int iLower(unsigned int n) const {return idxLower[n];}
  double vLower(unsigned int n) const {return valLower[n];}
  unsigned int nUpper() const {return idxUpper.size();}
  unsigned int iUpper(unsigned int n) const {return idxUpper[n];}
  double vUpper(unsigned int n) const {return valUpper[n];}

  // set lower bounds on design space
  void setLowerBounds(unsigned int,const std::vector<unsigned int>,const std::vector<double>);

  // set upper bounds on design space
  void setUpperBounds(unsigned int,const std::vector<unsigned int>,const std::vector<double>);

  // get equality constraints
  unsigned int nEquality() const {return cEqua.size();}
  const OptiFunction& cEquality(unsigned int n) const {return *(cEqua[n]);}

  // set equality constraints
  void setEqualityConstraints(unsigned int,const std::vector<const OptiFunction*>);

  // get inequality constraints
  unsigned int nInequality() const {return cIneq.size();}
  const OptiFunction& cInequality(unsigned int n) const {return *(cIneq[n]);}

  // set inequality constraints
  void setInequalityConstraints(unsigned int,const std::vector<const OptiFunction*>);
};

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
