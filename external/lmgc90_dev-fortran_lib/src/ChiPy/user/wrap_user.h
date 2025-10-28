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

#ifndef user_h
#define user_h

// to manage externalFEM in the wrapper copy then content of this file here
// ../../../../LMGC90v2_BindingExternalFEM/inc_wrap_user.h
 /**
  * @fn void user_getWoodFrame(double* matrix_in, int idim1, int idim2, double** matrix_out int dim1 int dim2,
                               double* rvector_in, int rlength_in, double* rvector_in2, int rlength_in2)
  * @brief Get the orthotropic frame of wood for a list of points
  *
  * @cond PYDOC
  * python usage : wood_frames = user_getWoodFrame(coors, center, orient)
  * @param[in] coors  (double array): of shape [nb_points,3] with  coordinates of points at which to compute frame
  * @param[in] center (double array): of shape [3] with marrow center position regarding the points to compute
  * @param[in] orient (double array): of shape [3] with marrow orientation
  * @return wood_frames (double array) : of shape [nb_points,9] with [3,3] LRT frame
  * @endcond
  *
  * @cond CDOC
  * @param[in] matrix_in (double *)   : the input matrix (the points coordinates)
  * @param[in] idim1 (int)            : the first dimension of the input matrix
  * @param[in] idim2 (int)            : the second dimension of the input matrix
  * @param[out] matrix_out (double**) : the output matrix (the 3x3 frame for each point)
  * @param[out] dim1 (int*)           : the first dimension of the output matrix
  * @param[out] dim2 (int*)           : the second dimension of the output matrix
  * @param[in] rvecotr_in (double *)  : the first input vector (marrow center)
  * @param[in] rlength_in (int)       : the dimension of the first input vector
  * @param[in] rvecotr_in2(double *)  : the second input vector (marrow orient)
  * @param[in] rlength_in2(int)       : the dimension of the second input vector
  * @endcond
  */
  extern "C" void user_getWoodFrame(double * matrix_in, int idim1, int idim2, double ** matrix_out, int* dim1, int* dim2, double * rvector_in, int rlength_in, double * rvector_in2, int rlength_in2);


#endif /* user_h */
