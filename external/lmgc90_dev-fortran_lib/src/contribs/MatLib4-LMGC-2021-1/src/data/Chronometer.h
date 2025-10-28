/*
 *  $Id: Chronometer.h 135 2013-08-30 15:14:32Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_DATA_CHRONO_H
#define ZORGLIB_DATA_CHRONO_H

// config
#include <matlib_macros.h>

// std C++ library
#include <string>
// std C library
#include <ctime>
// OpenMP
#ifdef _OPENMP
#include <omp.h>
#endif


#ifdef MATLIB_USE_NAMESPACE
BEGIN_MATLIB_NAMESPACE
#endif

/**
 * Class for CPU chronometer.
 */
class Chronometer {

 private:

  // indicator
  bool isRunning;

  // reference time
  clock_t refTime;

  // accumulated time
  unsigned long long accTime;

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
  
 private:
  
  // indicator
  bool isRunning;
  
  // reference time
  double refTime;
  
  // accumulated time
  double accTime;
  
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

#ifdef MATLIB_USE_NAMESPACE
END_MATLIB_NAMESPACE
#endif

#endif
