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

#ifndef wrap_JONCx_h
#define wrap_JONCx_h

 /**
  * @fn void JONCx_LoadTactors(void)
  * @brief load JONCx from RBDY2 and initialize existing_entites
  *
  * @cond PYDOC
  * python usage : JONCx_LoadTactors()
  * @endcond
  */
  extern "C" void JONCx_LoadTactors(void);

 /**
  * @fn int JONCx_GetNbJONCx(void)
  * @brief Get the number of JONCx in container
  *
  * @cond PYDOC
  * python usage : nb_joncx = JONCx_GetNbJONCx()
  *
  * @return nb_joncx (integer) : the number of JONCx in container
  * @endcond
  *
  * @cond CDOC
  * @return nb_joncx (int) : the number of JONCx in container
  * @endcond
  */
  extern "C" int JONCx_GetNbJONCx(void);

 /**
  * @fn int JONCx_GetBodyId(int)
  * @brief Get the body rank of a given JONCx
  *
  * @cond PYDOC
  * python usage : ibdyty = JONCx_GetBodyId(itacty)
  *
  * @param[in] itacty (integer) : JONCx rank
  * @return ibdyty (integer) : body rank 
  * @endcond
  *
  * @cond CDOC
  * @param[in] itacty (int) : JONCx rank
  * @return ibdyty (int) : body rank 
  * @endcond
  */
  extern "C" int JONCx_GetBodyId(int itacty);


/**
  * @fn JONCx_GetShape(int itacty, double** r8_vector, int* r8_size)
  * @brief Get the shape of a JONCx
  *
  * @cond PYDOC
  * usage : shape = JONCx_GetShape(itacty)
  * @param[in] itacty (integer)  : rank of JONCx
  * @return shape (double array) : axis length of the JONCx
  * @endcond
  *
  * @cond CDOC
  * @param[in]  itacty (int)         : rank of JONCx
  * @param[out] r8_vector (double**) : axis length of the JONCx
  * @param[out] r8_size (int*)       : size of r8_vector
  * @endcond
  */
  extern "C" void JONCx_GetShape(int itacty, double** r8_vector, int* r8_size);

/**
  * @fn JONCx_GetCoor(int itacty, double** r8_vector, int* r8_size)
  * @brief Get the coor of a JONCx
  *
  * @cond PYDOC
  * usage : coor = JONCx_GetCoor(itacty)
  * @param[in] itacty (integer) : rank of JONCx
  * @return coor (double array) : coordinates of the JONCx
  * @endcond
  *
  * @cond CDOC
  * @param[in]  itacty (int)         : rank of JONCx
  * @param[out] r8_vector (double**) : coordinates of the JONCx
  * @param[out] r8_size (int*)       : size of r8_vector
  * @endcond
  */
  extern "C" void JONCx_GetCoor(int itacty, double** r8_vector, int* r8_size);

 /**
  * @fn void JONCx_GetPtrJONCx2BDYTY(int** pointer_out, int* dim1, int* dim2)
  * @brief return a pointer onto the map joncx2rbdy2
  *
  * @cond PYDOC
  * python usage : joncx2rbdy2 = JONCx_GetPtrJONCx2BDYTY()
  *
  * @return joncx2rbdy2 (integer array) : reference on map between joncx rank and body/tact rank
  * @endcond
  *
  * @cond CDOC
  * @param[out] pointer_out (int **) : a pointer on the array joncx2rbdy2
  * @param[out]        dim1 (int *)  : first dim of pointer_out
  * @param[out]        dim2 (int *)  : second dim of pointer_out
  * @endcond
  */
  extern "C" void JONCx_GetPtrJONCx2BDYTY(int** pointer_out, int* dim1, int* dim2);

 /**
  * @fn int JONCx_IsVisible(int itact)
  * @brief return if a body visible
  *
  * @cond PYDOC
  * usage : visible = JONCx_IsVisible(itact)
  * @param[in] itact (integer)   : rank of JONCx
  * @param     visible (integer) : 1 if body is visible, 0 else
  * @endcond
  *
  * @cond CDOC
  * @param[in] itact (int)   : rank of JONCx
  * @return    visible (int) : 1 if body is visible, 0 else
  * @endcond
  */
  extern "C" int JONCx_IsVisible(int itact);

 /**
  * @fn void JONCx_InitOutlines(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the outlines of all JONCx
  *
  * @cond PYDOC
  * usage : outlines = JONCx_InitOutlines()
  * @return outlines (double array) : a reference on outlines_JONCx
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on outlines_JONCx array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void JONCx_InitOutlines(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void JONCx_InitScalarFields(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the scalar fields of all JONCx
  *
  * @cond PYDOC
  * usage : scalarfields = JONCx_InitScalarfields()
  * @return scalarfields (double array) : reference on scalarfields_JONCx array
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on scalarfields array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void JONCx_InitScalarFields(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void JONCx_UpdatePostdata(void)
  * @brief Update values of outlines_JONCx and scalarfields_JONCx pointers
  *
  * @cond PYDOC
  * usage : JONCx_UpdatePostdata()
  * @endcond
  *
  */
  extern "C" void JONCx_UpdatePostdata(void);

 /**
  * @fn void JONCx_GetNbPointOutlines(int ** pointer_out, int * length)
  *
  * @brief Get the list of cumulated outline points number
  *
  * @cond PYDOC
  * python usage : nb_pointOutlines = JONCx_GetNbPointOutlines()
  * @return nb_pointOutlines (integer array) : the cumulated number of outline points of the JONCx
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (int **) : reference on nb_point_outlines array
  * @param[in,out] length (int *)       : first dimension of pointer_out
  * @endcond
  */
  extern "C" void JONCx_GetNbPointOutlines(int** pointer_out, int* length);
  
 /**
  * @fn int JONCx_GetNbScalarFields(void)
  *
  * @brief Get the number of scalar fields of a JONCx
  *
  * @cond PYDOC
  * python usage : nb_scalarfields = JONCx_GetNbScalarFields()
  * @return nb_scalarfields (integer) : the number of scalar fields of a JONCx
  * @endcond
  *
  * @cond CDOC
  * @return nb_scalarfields (int) : the number of scalar fields of a JONCx
  * @endcond
  */
  extern "C" int JONCx_GetNbScalarFields(void);

 /**
   * @fn void JONCx_CleanMemory(void)
   * @brief Free all memory allocated within JONCx module
   *
   * @cond PYDOC
   * python usage : JONCx_CleanMemory()
   * @endcond
   */
   extern "C" void JONCx_CleanMemory(void);
  
#endif /* wrap_JONCx_h */
