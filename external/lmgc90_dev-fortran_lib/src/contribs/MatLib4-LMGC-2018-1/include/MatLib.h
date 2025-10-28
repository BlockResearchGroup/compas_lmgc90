/*
 *  $Id: MatLib.h 235 2017-05-20 10:13:37Z lstainier $
 *
 *  This file is part of ZorgLib, a computational simulation framework
 *  for thermomechanics of solids and structures (systems in general).
 *
 *  Copyright (c) 2001-2013, L. Stainier.
 *  See file LICENSE.txt for license information.
 *  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.
 */
#ifndef ZORGLIB_MATLIB_H
#define ZORGLIB_MATLIB_H

// local
#define WITH_MATLIB_H

#include "Chronometer.h"
#include "Exceptions.h"
#include "FileException.h"
#include "SyntaxError.h"
#include "Cloneable.h"
#include "Copiable.h"
#include "Function.h"
#include "Function2.h"
#include "ConstantFunction.h"
#include "TabulatedFunction.h"
#include "TabulatedFunction2.h"
#include "StringMap.h"
#include "Property.h"
#include "ShortArray.h"
#include "ShortMatrix.h"
#include "ShortSqrMatrix.h"
#include "Rotation.h"
#include "Rotation2D.h"
#include "Rotation3D.h"

#include "MaterialProperties.h"
#include "MaterialState.h"
#include "ConstitutiveModel.h"
#include "MaterialModel.h"
#include "ModelDictionary.h"
#include "MaterialCriterion.h"
#include "CriterionDictionary.h"

#include "MathUtils.h"
//#include "TensorAlgebra.h" /* not exported for now */
#include "Vector1D.h"
#include "Vector2D.h"
#include "Vector3D.h"

#include "OptiFunction.h"
#include "OptiProblem.h"
#include "OptiMethod.h"

#endif
