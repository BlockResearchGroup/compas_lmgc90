/*
 *  $Id: matlib.i 198 2016-02-26 10:46:57Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2016, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
%module matlib

%include "std_pair.i"
%include "std_string.i"
%include "std_vector.i"

// instantiate templates
namespace std {
  %template(DoublePair) pair<double,double>;
}

%include "data.i"
%include "math.i" /* API partially exported */
%include "matl.i"
%include "opti.i"

%pythoncode %{
  def MatLibArray(*args): return ShortArray(*args)
  def MatLibMatrix(*args): return ShortSqrMatrix(*args)
%}

