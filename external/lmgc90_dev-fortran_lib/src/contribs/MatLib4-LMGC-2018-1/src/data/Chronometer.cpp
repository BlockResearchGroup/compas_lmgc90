/*
 *  $Id: Chronometer.cpp 135 2013-08-30 15:14:32Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#include "Chronometer.h"

// std C library
#include <cmath>

#ifdef MATLIB_USE_NAMESPACE
USING_MATLIB_NAMESPACE
#endif


/*
 * Methods for class Chronometer.
 */

// constructor
Chronometer::Chronometer() {
  isRunning = false;
  refTime  = std::clock();
  accTime = 0;
}

// copy constructor
Chronometer::Chronometer(const Chronometer& src) {
  isRunning = src.isRunning;
  refTime = src.refTime;
  accTime = src.accTime;
}

// start chrono
void Chronometer::start() {
  isRunning = true;
  refTime = std::clock();
}

// stop chrono
void Chronometer::stop() {
  if (!isRunning) return;

  clock_t curTime = std::clock();
  isRunning = false;
  accTime += (curTime-refTime);
}

// reset chrono
void Chronometer::reset() {
  refTime = std::clock();
  accTime = 0;
}

// elapsed time
unsigned long long Chronometer::elapsed() {
  if (isRunning) {
    clock_t curTime = std::clock();
    return accTime+(curTime-refTime);
  }
  else
    return accTime;
}

// format elapsed time
std::string Chronometer::toString(unsigned long long t) {

  static const unsigned long long H0 = (unsigned long long)3600*CLOCKS_PER_SEC;
  static const unsigned long long M0 = (unsigned long long)60*CLOCKS_PER_SEC;
  static const float S0 = CLOCKS_PER_SEC/1000.;
  unsigned long h,m,r,s;
  h = t/H0;
  r = t-h*H0;
  m = r/M0;
  r -= m*M0;
  s = r/CLOCKS_PER_SEC;
  r -= s*CLOCKS_PER_SEC;

  O_STRING_STREAM os;
  os << h << "h:";
  os.fill('0');
  os.width(2);
  os << m << "m:";
  os.width(2);
  os << s << ".";
  os.width(3);
  float val;
  std::modf(r/S0,&val);
  os << static_cast<int>(val) << "s";
#ifdef HAVE_SSTREAM
  return os.str();
#else
  return std::string(os.str(),os.pcount());
#endif
}

#ifdef _OPENMP
/*
 * Methods for class OMPChronometer.
 */

// constructor
OMPChronometer::OMPChronometer() {
  isRunning = false;
  refTime  = omp_get_wtime();
  accTime = 0;
}

// copy constructor
OMPChronometer::OMPChronometer(const OMPChronometer& src) {
  isRunning = src.isRunning;
  refTime = src.refTime;
  accTime = src.accTime;
}

// start chrono
void OMPChronometer::start() {
  isRunning = true;
  refTime = omp_get_wtime();
}

// stop chrono
void OMPChronometer::stop() {
  if (!isRunning) return;
  
  double curTime = omp_get_wtime();
  isRunning = false;
  accTime += (curTime-refTime);
}

// reset chrono
void OMPChronometer::reset() {
  refTime = omp_get_wtime();
  accTime = 0;
}

// elapsed time
double OMPChronometer::elapsed() {
  if (isRunning) {
    double curTime = omp_get_wtime();
    return accTime+(curTime-refTime);
  }
  else
    return accTime;
}

// format elapsed time
std::string OMPChronometer::toString(double t) {
  unsigned long h,m,s;
  double r;
  h = static_cast<unsigned long>(std::floor(t/3600));
  r = t-3600*h;
  m = static_cast<unsigned long>(std::floor(r/60));
  r -= 60*m;
  s = static_cast<unsigned long>(std::floor(r));
  r -= s;
  
  O_STRING_STREAM os;
  os << h << "h:";
  os.fill('0');
  os.width(2);
  os << m << "m:";
  os.width(2);
  os << s << ".";
  os.width(3);
  os << static_cast<int>(1000*r) << "s";
#ifdef HAVE_SSTREAM
  return os.str();
#else
  return std::string(os.str(),os.pcount());
#endif
}
#endif
