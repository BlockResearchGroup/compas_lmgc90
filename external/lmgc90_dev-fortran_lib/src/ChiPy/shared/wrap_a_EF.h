/*==========================================================================
 *
 * Copyright 2000-2025 CNRS-UM.
 *
 * This file is part of a software (LMGC90) which is a computer program 
 * which purpose is to modelize interaction problems (contact, multi-Physics,etc).
 *
 * This software is governed by the CeCILL license under French law and
 * abiding by the rules of distribution of free software.  You can  use, 
 * modify and/ or redistribute the software under the terms of the CeCILL
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info". 
 *
 * As a counterpart to the access to the source code and  rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty  and the software's author,  the holder of the
 * economic rights,  and the successive licensors  have only  limited
 * liability. 
 *
 * In this respect, the user's attention is drawn to the risks associated
 * with loading,  using,  modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean  that it is complicated to manipulate,  and  that  also
 * therefore means  that it is reserved for developers  and  experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or 
 * data to be ensured and,  more generally, to use and operate it in the 
 * same conditions as regards security. 
 *
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL license and that you accept its terms.
 *
 * To report bugs, suggest enhancements, etc. to the Authors, contact
 * Frederic Dubois.
 *
 * frederic.dubois@umontpellier.fr
 *
 *=========================================================================*/
#ifndef wrap_a_EF_h
#define wrap_a_EF_h
 
 /**
  * @fn void a_EF_InterpolateField(double * rvector_in, int rlength_in, in, double * rvector_in2, int rlength_in2, double * res)
  * @brief Compute the interpolation of a nodal field at a given point
  *
  * @cond PYDOC
  * python usage : interpolation = a_EF_InterpolateField(field,point)
  * @param[in] field (double array) : values on field
  * @param[in] point (double array) : coordinates of the point on reference element
  * @return interpolation (double) : interpolated value of the field
  * @endcond
  *
  * @cond CDOC
  * @param[in] rvector_in  (double *) : values on field
  * @param[in] rlength_in  (int)      : the number of nodes of the element
  * @param[in] rvector_in2 (double *) : coordinates of the point on reference element
  * @param[in] rlength_in2 (int)      : space dimension
  * @param[out] res (double)          : interpolated value of the field
  * @endcond
  */
  extern "C" void a_EF_InterpolateField(double * rvector_in, int rlength_in, double * rvector_in2, int rlength_in2, double * res);

 /**
  * @fn void a_EF_ComputeCenter(double * matrix_in, int dim1, int dim2, double ** r8_vector, int * r8_size)
  * @brief Compute the geometric center of an element
  *
  * @cond PYDOC
  * python usage : center = a_EF_ComputeCenter(coor)
  * @param[in] coor (double array) : coordinates the nodes of the element
  * @return center (double array) : computed center of the element
  * @endcond
  *
  * @cond CDOC
  * @param[in] matrix_in (double *) : the coordinates array of the element
  * @param[in] dim1 (int)           : the number of nodes of the element
  * @param[in] dim2 (int)           : the space dimension
  * @param[inout] r8_vector (double **) : reference on the computed center array
  * @param[out] r8_size (int *) : reference on the size of r8_vector
  * 
  * @endcond
  */
  extern "C" void a_EF_ComputeCenter(double * matrix_in, int dim1, int dim2, double ** r8_vector, int * r8_size);

#endif /* wrap_a_EF_h */
