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

#ifndef wrap_PT2Dx_h
#define wrap_PT2Dx_h

 /**
  * @fn void PT2Dx_LoadTactors(void)
  * @brief load PT2Dx from RBDY2 and initialize existing_entites
  *
  * @cond PYDOC
  * python usage : PT2Dx_LoadTactors()
  * @endcond
  */
  extern "C" void PT2Dx_LoadTactors();

 /**
  * @fn int PT2Dx_GetNbPT2Dx(void)
  * @brief Get the number of PT2Dx in the container
  *
  * @cond PYDOC
  * python usage : nb_pt2d = PT2Dx_GetNbPT2Dx()
  *
  * @return nb_pt2d (integer) : the number of PT2Dx in container
  * @endcond
  *
  * @cond CDOC
  * @return nb_pt2d (int) : the number of PT2Dx in container
  * @endcond
  */
  extern "C" int PT2Dx_GetNbPT2Dx(void);
  
 /**
  * @fn void PT2Dx_SetDisplayRadius(double rvalue)
  * @brief Set a radius to display a pt2dx
  *
  * @cond PYDOC
  * python usage : PT2Dx_SetDisplayRadius(radius)
  * @param[in] radius (double) : value of the radius which should be used for display
  * @endcond
  *
  *
  * @cond CDOC
  * @param[in] rvalue (double) : value of the radius which should be used for display
  * @endcond
  */
  extern "C" void PT2Dx_SetDisplayRadius(double rvalue);

 /**
  * @fn void PT2Dx_GetPtrPT2Dx2BDYTY(int** pointer_out, int* dim1, int* dim2)
  * @brief return a pointer onto the map pt2dx2rbdy2
  *
  * @cond PYDOC
  * python usage : ptd2x2rbdy2 = PT2Dx_GetPtrPT2Dx2BDYTY()
  *
  * @return pt2dx2rbdy2 (integer array) : reference on map between pt2dx rank and body/tact rank
  * @endcond
  *
  * @cond CDOC
  * @param[out] pointer_out (int **) : a pointer on the array pt2dx2rbdy2
  * @param[out] dim1 (int *)         : first dim of pointer_out
  * @param[out] dim2 (int *)         : second dim of pointer_out
  * @endcond
  */
  extern "C" void PT2Dx_GetPtrPT2Dx2BDYTY(int** pointer_out, int* dim1, int* dim2);

 /**
  * @fn int PT2Dx_IsVisible(int itact)
  * @brief return if a body visible
  *
  * @cond PYDOC
  * usage : visible = PT2Dx_IsVisible(itact)
  * @param[in] itact (integer)   : rank of PT2Dx
  * @param     visible (integer) : 1 if body is visible, 0 else
  * @endcond
  *
  * @cond CDOC
  * @param[in] itact (int)   : rank of PT2Dx
  * @return    visible (int) : 1 if body is visible, 0 else
  * @endcond
  */
  extern "C" int PT2Dx_IsVisible(int itact);

 /**
  * @fn void PT2Dx_InitOutlines(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the outlines of all PT2Dx
  *
  * @cond PYDOC
  * usage : outlines = PT2Dx_InitOutlines()
  * @return outlines (double array) : a reference on outlines_PT2Dx
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on outlines_PT2Dx array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void PT2Dx_InitOutlines(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void PT2Dx_InitScalarFields(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the scalar fields of all PT2Dx
  *
  * @cond PYDOC
  * usage : scalarfields = PT2Dx_InitScalarfields()
  * @return scalarfields (double array) : reference on scalarfields_PT2Dx array
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on scalarfields array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void PT2Dx_InitScalarFields(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void PT2Dx_UpdatePostdata(void)
  * @brief Update values of outlines_PT2Dx and scalarfields_PT2Dx pointers
  *
  * @cond PYDOC
  * usage : PT2Dx_UpdatePostdata
  * @endcond
  *
  */
  extern "C" void PT2Dx_UpdatePostdata(void);

 /**
  * @fn void PT2Dx_GetNbPointOutlines(int ** pointer_out, int * length)
  *
  * @brief Get the list of cumulated outline points number
  *
  * @cond PYDOC
  * python usage : nb_pointOutlines = PT2Dx_GetNbPointOutlines()
  * @return nb_pointOutlines (integer array) : the cumulated number of outline points of the PT2Dx
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (int **) : reference on nb_point_outlines array
  * @param[in,out] length (int *)       : first dimension of pointer_out
  * @endcond
  */
  extern "C" void PT2Dx_GetNbPointOutlines(int** pointer_out, int* length);
  
 /**
  * @fn int PT2Dx_GetNbScalarFields(void)
  *
  * @brief Get the number of scalar fields of a PT2Dx
  *
  * @cond PYDOC
  * python usage : nb_scalarfields = PT2Dx_GetNbScalarFields()
  * @return nb_scalarfields (integer) : the number of scalar fields of a PT2Dx
  * @endcond
  *
  * @cond CDOC
  * @return nb_scalarfields (int) : the number of scalar fields of a PT2Dx
  * @endcond
  */
  extern "C" int PT2Dx_GetNbScalarFields(void);

 /**
   * @fn void PT2Dx_CleanMemory(void)
   * @brief Free all memory allocated within PT2Dx module
   *
   * @cond PYDOC
   * python usage : PT2Dx_CleanMemory()
   * @endcond
   */
   extern "C" void PT2Dx_CleanMemory(void);
  
#endif /* wrap_PT2Dx */
