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

#ifndef wrap_xKSID_h
#define wrap_xKSID_h


 /**
  * @fn void xKSID_LoadTactors(void)
  * @brief load xKSID from RBDY2 and initialize existing_entites
  *
  * @cond PYDOC
  * python usage : xKSID_LoadTactors()
  * @endcond
  */
 extern "C" void xKSID_LoadTactors();

 /**
  * @fn int xKSID_GetNbxKSID(void)
  *
  * @brief Get the number of xKSID in the container
  *
  * @cond PYDOC
  * python usage : nb_diskx = xKSID_GetNbxKSID()
  *
  * @return nb_xKSID (integer) : the number of xKSID in container
  * @endcond
  *
  * @cond CDOC
  * @return nb_xKSID (int) : the number of xKSID in container
  * @endcond
  */
  extern "C" int xKSID_GetNbxKSID(void);
  
 /**
  * @fn void xKSID_GetPtrxKSID2BDYTY(int ** pointer_out, int * dim, int * dim2)
  * @brief return a pointer onto the map xksid2rbdy2
  *
  * @cond PYDOC
  * python usage : xksid2rbdy2 = xKSID_GetPtrxKSID2BDYTY()
  *
  * @return xksid2rbdy2 (integer array) : reference on map between xksid rank and body/tact rank
  * @endcond
  *
  * @cond CDOC
  * @param[out] pointer_out (int **) : a pointer on the array xksid2rbdy2
  * @param[out] dim1 (int *)         : first dim of pointer_out
  * @param[out] dim2 (int *)         : second dim of pointer_out
  * @endcond
  */
  extern "C" void xKSID_GetPtrxKSID2BDYTY(int ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn int xKSID_IsVisible(int itact)
  * @brief return if a body visible
  *
  * @cond PYDOC
  * usage : visible = xKSID_IsVisible(itact)
  * @param[in] itact (integer)   : rank of xKSID
  * @param     visible (integer) : 1 if body is visible, 0 else
  * @endcond
  *
  * @cond CDOC
  * @param[in] itact (int)   : rank of xKSID
  * @return    visible (int) : 1 if body is visible, 0 else
  * @endcond
  */
  extern "C" int xKSID_IsVisible(int itact);

 /**
  * @fn double xKSID_GetContactorRadius(int itact)
  * @brief Get the radius of a given xKSID
  *
  * @cond PYDOC
  * python usage : radius = xKSID_GetContactorRadius(itact)
  * @param[in] itact (integer) : rank of a xKSID (in the list of all the xKSID)
  * @return    radius (double) : the radius of the xKSID of rank itact
  * @endcond
  *
  * @cond CDOC
  * @param[in] itact (int)     : rank of a xKSID
  * @return    radius (double) : the radius of the xKSID of rank itact
  * @endcond
  */
  extern "C" double xKSID_GetContactorRadius(int itact);

 /**
  * @fn void xKSID_GetContactorCoor(int itacty, double** r8_vector, int* r8_size)
  * @brief get coordinates of the center of a given xKSID
  *
  * @cond PYDOC
  * usage : vector = xKSID_GetContactorCoor(itacty)
  * @param[in] itacty (integer)   : rank of considered contactor
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
  extern "C" void xKSID_GetContactorCoor(int itacty, double** r8_vector, int* r8_size);

 /**
  * @fn void xKSID_InitOutlines(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the outlines of all xKSID
  *
  * @cond PYDOC
  * usage : outlines = xKSID_InitOutlines()
  * @return outlines (double array) : a reference on outlines_xKSID
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on outlines_xKSID array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void xKSID_InitOutlines(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void xKSID_InitScalarFields(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the scalar fields of all xKSID
  *
  * @cond PYDOC
  * usage : scalarfields = xKSID_InitScalarfields()
  * @return scalarfields (double array) : reference on scalarfields_xKSID array
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on scalarfields array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void xKSID_InitScalarFields(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void xKSID_UpdatePostdata(void)
  * @brief Update values of outlines_xKSID and scalarfields_xKSID pointers
  *
  * @cond PYDOC
  * usage : xKSID_UpdatePostdata
  * @endcond
  *
  */
  extern "C" void xKSID_UpdatePostdata(void);

 /**
  * @fn void xKSID_GetNbPointOutlines(int ** pointer_out, int * length)
  *
  * @brief Get the list of cumulated outline points number
  *
  * @cond PYDOC
  * python usage : nb_pointOutlines = xKSID_GetNbPointOutlines()
  * @return nb_pointOutlines (integer array) : the cumulated number of outline points of the xKSID
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (int **) : reference on nb_point_outlines array
  * @param[in,out] length (int *)       : first dimension of pointer_out
  * @endcond
  */
  extern "C" void xKSID_GetNbPointOutlines(int** pointer_out, int* length);
  
 /**
  * @fn int xKSID_GetNbScalarFields(void)
  *
  * @brief Get the number of scalar fields of a xKSID
  *
  * @cond PYDOC
  * python usage : nb_scalarfields = xKSID_GetNbScalarFields()
  * @return nb_scalarfields (integer) : the number of scalar fields of a xKSID
  * @endcond
  *
  * @cond CDOC
  * @return nb_scalarfields (int) : the number of scalar fields of a xKSID
  * @endcond
  */
  extern "C" int xKSID_GetNbScalarFields(void);

 /**
   * @fn void xKSID_CleanMemory(void)
   * @brief Free all memory allocated within xKSID module
   *
   * @cond PYDOC
   * python usage : xKSID_CleanMemory()
   * @endcond
   */
   extern "C" void xKSID_CleanMemory(void);

 /**
  * @fn void xKSID_SetXdilation(int itacty, double x)
  * @brief set increase of radius of a xKSID due to expansion
  *
  * @cond PYDOC
  * python usage : xKSID_SetXdilation(itacty,x)
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
  extern "C" void xKSID_SetXdilation(int itacty, double x);


 /**
   * @fn void xKSID_SetVdilation(int itacty, double v)
   * @brief  set increase rate of radius of a xKSID due to expansion
   *
   * @cond PYDOC
   * python usage : xKSID_SetVdilation(itacty, v)
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
  extern "C" void xKSID_SetVdilation(int itacty, double v);
  
#endif /* wrap_xKSID */
