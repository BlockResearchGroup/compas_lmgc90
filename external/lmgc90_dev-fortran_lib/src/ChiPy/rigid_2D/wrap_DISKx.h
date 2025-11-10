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

#ifndef wrap_DISKx_h
#define wrap_DISKx_h

 /**
  * @fn void DISKx_LoadTactors(void)
  *
  * @brief load DISKx from RBDY2 file and initialize existing_entites
  *
  * @cond PYDOC
  * python usage : DISKx_LoadTactors()
  * @endcond
  */
  extern "C" void DISKx_LoadTactors(void);

 /**
  * @fn int DISKx_GetNbDISKx(void)
  *
  * @brief Get the number of DISKx in the container
  *
  * @cond PYDOC
  * python usage : nb_diskx = DISKx_GetNbDISKx()
  *
  * @return nb_DISKx (integer) : the number of DISKx in container
  * @endcond
  *
  * @cond CDOC
  * @return nb_DISKx (int) : the number of DISKx in container
  * @endcond
  */
  extern "C" int DISKx_GetNbDISKx(void);
  
 /* /\** */
 /*  * @fn void DISKx_GetDISKx2RBDY2(int** i4_vector, int* i4_size) */
 /*  * @brief return a copy of the map diskx2rbdy2 */
 /*  * */
 /*  * @cond PYDOC */
 /*  * python usage : diskx2rbdy2 = DISKx_GetDISKx2RBDY2() */
 /*  * */
 /*  * @return diskx2rbdy2 (integer array) : map between diskx rank and body rank */
 /*  * @endcond */
 /*  * */
 /*  * @cond CDOC */
 /*  * @param[out] i4_vector (int**) : reference on diskx2rbdy2 integer array */
 /*  * @param[out] i4_size (int*)    : the size of i4_vector */
 /*  * @endcond */
 /*  *\/ */
 /*  extern "C" void DISKx_GetDISKx2RBDY2(int** i4_vector, int* i4_size); */

/**
  * @fn void DISKx_GetDISKx2BDYTY(int** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of map DISKx2bdyty
  *
  * @cond PYDOC
  * usage : polyr2bdyty = DISKx_GetDISKx2BDYTY()
  * @return polyr2bdyty (integer 2D-array) : the polyr2bdyty map
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] matrix_out (int**) : reference on a copy of the array polyr2bdyty
  * @param[out]    dim1 (int*)        : number of field on the map
  * @param[out]    dim2 (int*)        : number of polyr
  * @endcond
  */
  extern "C" void DISKx_GetDISKx2BDYTY(int** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void DISKx_GetPtrDISKx2BDYTY(int ** pointer_out, int * dim, int * dim2)
  * @brief return a pointer onto the map diskx2rbdy2
  *
  * @cond PYDOC
  * python usage : diskx2bdyty = DISKx_GetPtrDISKx2BDYTY()
  *
  * @return diskx2bdyty (integer array) : reference on map between diskx rank and body rank
  * @endcond
  *
  * @cond CDOC
  * @param[out] pointer_out (int**) : a pointer on the array diskx2rbdy2
  * @param[out] dim1 (int *)        : first dim of pointer_out
  * @param[out] dim2 (int *)        : second dim of pointer_out
  * @endcond
  */
  extern "C" void DISKx_GetPtrDISKx2BDYTY(int** pointer_out, int* dim1, int* dim2);

 /**
  * @fn int DISKx_IsVisible(int itact)
  * @brief return if a body visible
  *
  * @cond PYDOC
  * python usage : visible = DISKx_IsVisible(itact)
  *
  * @param[in] itact (integer)   : rank of DISKx
  *
  * @param[in] visible (integer) : 1 if body is visible, 0 else
  * @endcond
  *
  * @cond CDOC
  * @param[in] itact (int)   : rank of DISKx
  * @return    visible (int) : 1 if body is visible, 0 else
  * @endcond
  */
  extern "C" int DISKx_IsVisible(int itact);

 /**
  * @fn double DISKx_GetContactorRadius(int itact)
  * @brief Get the radius of a given DISKx
  *
  * @cond PYDOC
  * python usage : radius = DISKx_GetContactorRadius(itact)
  *
  * @param[in] itact (integer) : rank of a DISKx (in the list of all the DISKx)
  *
  * @return    radius (double) : the radius of the DISKx of rank itact
  * @endcond
  *
  * @cond CDOC
  * @param[in] itact (int)     : rank of a DISKx
  * @return    radius (double) : the radius of the DISKx of rank itact
  * @endcond
  */
  extern "C" double DISKx_GetContactorRadius(int itact);

 /**
  * @fn double DISKx_GetMeanRadius(void)
  * @brief Get the mean radius of DISKx in the container
  *
  * @cond PYDOC
  * python usage : radius = DISKx_GetMeanRadius()
  * 
  * @return radius (double) : the mean radius of DISKx in the container
  * @endcond
  *
  * @cond CDOC
  * @return mean_radius (double) : the mean radius of DISKx in the container
  * @endcond
  */
  extern "C" double DISKx_GetMeanRadius(void);

 /**
  * @fn double DISKx_GetMaxRadius(void)
  * @brief Get the max radius of DISKx in the container
  *
  * @cond PYDOC
  * python usage : radius = DISKx_GetMaxRadius()
  *
  * @return radius (double) : the max radius of DISKx in the contactor
  * @endcond
  *
  * @cond CDOC
  * @return radius (double) : the max radius of DISKx in the contactor
  * @endcond
  */
  extern "C" double DISKx_GetMaxRadius(void);

 /**
  * @fn double DISKx_GetMinRadius(void)
  * @brief Get the min radius of DISKx in the container
  *
  * @cond PYDOC
  * python usage : radius = DISKx_GetMinRadius()
  * 
  * @return radius (double) : the min radius of DISKx in the container
  * @endcond
  *
  * @cond CDOC
  * @return radius (double) : the min radius of DISKx in the container
  * @endcond
  */
  extern "C" double DISKx_GetMinRadius(void);

 /**
  * @fn void DISKx_GetContactorColor(int itact, char ** c5)
  * @brief Get the color of a given DISKx
  *
  * @cond PYDOC
  * python usage : color = DISKx_GetContactorColor(itact)
  *
  * @param[in] itact (integer) : rank of a DISKx
  *
  * @return color (string)     : the color of the DISKx itact
  * @endcond
  *
  * @cond CDOC
  * @param[in] itact (int)  : rank of a DISKx
  * @param[out] c5 (char**) : pointer on 5 characters string of contactor color
  * @endcond
  */
  extern "C" void DISKx_GetContactorColor(int itact, char** c5);

 /* /\** */
 /*  * @fn double DISKx_GetRadius(int ibdyty) */
 /*  * @brief get radius of the DISKx owned by body ibdyty (0 if doesn't exist). Doesn't work properly with clusters */
 /*  * */
 /*  * @cond PYDOC */
 /*  * python usage : radius = DISKx_GetRadius(ibdyty) */
 /*  * */
 /*  * @param[in] ibdyty (integer) : rank of body */
 /*  * */
 /*  * @return radius (double)     : the radius of DISKx of body ibdyty */
 /*  * @endcond */
 /*  * */
 /*  * @cond CDOC */
 /*  * @param[in] ibdyty (int) : rank of a body */
 /*  * @return (double) : the raidus of DISKx of body ibdyty */
 /*  * @endcond */
 /*  *\/ */
 /*  extern "C" double DISKx_GetRadius(int ibdyty); */

 /**
  * @fn double DISKx_GetRadius(int itacty)
  * @brief get radius of a DISKx
  *
  * @cond PYDOC
  * python usage : radius = DISKx_GetRadius(itacty)
  *
  * @param[in] itacty (integer) : rank of DISKx
  *
  * @return radius (double)     : the radius of DISKx of body ibdyty
  * @endcond
  *
  * @cond CDOC
  * @param[in] itacty (int) : rank of DISKx
  * @return (double)        : the radus of DISKx of body ibdyty
  * @endcond
  */
  extern "C" double DISKx_GetRadius(int itacty);

 /**
  * @fn void DISKx_GetContactorCoor(int itacty, double** r8_vector, int* r8_size)
  * @brief get coordinates of the center of a given DISKx
  *
  * @cond PYDOC
  * python usage : vector = DISKx_GetContactorCoor(itacty)
  *
  * @param[in] itacty (integer)   : rank of considered contactor
  *
  * @return vector (double array) : the desired vector
  * @endcond
  *
  * @cond CDOC
  * @param[in]  itacty (int)         : rank of considered contactor
  * @param[out] r8_vector (double**) : the vector to get
  * @param[out] r8_size (int*)       : the length of r8_vector
  * @endcond
  *
  */
  extern "C" void DISKx_GetContactorCoor(int itacty, double** r8_vector, int* r8_size);

 /**
  * @fn void DISKx_InitOutlines(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the outlines of all DISKx
  *
  * @cond PYDOC
  * python usage : outlines = DISKx_InitOutlines()
  *
  * @return outlines (double array) : a reference on outlines_DISKx
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on outlines_DISKx array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void DISKx_InitOutlines(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void DISKx_InitScalarFields(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the scalar fields of all DISKx
  *
  * @cond PYDOC
  * python usage : scalarfields = DISKx_InitScalarfields()
  *
  * @return scalarfields (double array) : reference on scalarfields_DISKx array
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on scalarfields array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void DISKx_InitScalarFields(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void DISKx_UpdatePostdata(void)
  * @brief Update values of outlines_DISKx and scalarfields_DISKx pointers
  *
  * @cond PYDOC
  * python usage : DISKx_UpdatePostdata()
  * @endcond
  *
  */
  extern "C" void DISKx_UpdatePostdata(void);

 /**
  * @fn void DISKx_GetNbPointOutlines(int ** pointer_out, int * length)
  * @brief Get the list of cumulated outline points number
  *
  * @cond PYDOC
  * python usage : nb_pointOutlines = DISKx_GetNbPointOutlines()
  *
  * @return nb_pointOutlines (integer array) : the cumulated number of outline points of the DISKx
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (int **) : reference on nb_point_outlines array
  * @param[in,out] length (int *)       : first dimension of pointer_out
  * @endcond
  */
  extern "C" void DISKx_GetNbPointOutlines(int** pointer_out, int* length);
  
 /**
  * @fn int DISKx_GetNbScalarFields(void)
  * @brief Get the number of scalar fields of a DISKx
  *
  * @cond PYDOC
  * python usage : nb_scalarfields = DISKx_GetNbScalarFields()
  *
  * @return nb_scalarfields (integer) : the number of scalar fields of a DISKx
  * @endcond
  *
  * @cond CDOC
  * @return nb_scalarfields (int) : the number of scalar fields of a DISKx
  * @endcond
  */
  extern "C" int DISKx_GetNbScalarFields(void);

 /**
  * @fn void DISKx_CleanMemory(void)
  * @brief Free all memory allocated within DISKx module
  *
  * @cond PYDOC
  * python usage : DISKx_CleanMemory()
  * @endcond
  */
  extern "C" void DISKx_CleanMemory(void);
  
 /**
  * @fn void DISKx_SetXdilation(int itacty, double x)
  * @brief set increase of radius of a DISKx due to expansion
  *
  * @cond PYDOC
  * python usage : DISKx_SetXdilation(itacty,x)
  *
  * @param[in] itacty (integer)   : rank of considered contactor
  *
  * @param[in] x (float)          : increase of radius
  * @endcond
  *
  * @cond CDOC
  * @param[in] itacty (int)    : rank of contactor
  * @param[in] x (double)      : increase of radius  
  * @endcond
  */
  extern "C" void DISKx_SetXdilation(int itacty, double x);


 /**
   * @fn void DISKx_SetVdilation(int itacty, double v)
   * @brief  set increase rate of radius of a DISKx due to expansion
   *
   * @cond PYDOC
   * python usage : DISKx_SetVdilation(itacty, v)
   *
   * @param[in] itacty (integer) : rank of contactor
   * @param[in] v (float)        : radius increase rate  
   * @endcond
   *
   * @cond CDOC
   * @param[in] itacty (int)    : rank of RBDY2
   * @param[in] v (double)      : radius increase rate  
   * @endcond
   */
  extern "C" void DISKx_SetVdilation(int itacty, double v);

#endif /* wrap_DISKx_h */
