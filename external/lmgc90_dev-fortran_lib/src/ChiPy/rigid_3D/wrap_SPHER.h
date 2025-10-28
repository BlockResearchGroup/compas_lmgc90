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

#ifndef wrap_SPHER_h
#define wrap_SPHER_h

 /**
  * @fn void SPHER_LoadTactors(void)
  * @brief load SPHER from RBDY3 and initialize existing_entites
  *
  * @cond PYDOC
  * python usage : SPHER_LoadTactors()
  * @endcond
  */
  extern "C" void SPHER_LoadTactors(void);

 /**
  * @fn void SPHER_SetRadiusCorrection(double corr)
  * @brief set a radius correction
  *
  * @cond PYDOC
  * python usage : SPHER_SetRadiusCorrection(corr)
  * @param[in] corr (real) :
  * @endcond
  *
  * @cond CDOC
  * @param[in] corr (double) :
  * @endcond
  */
  extern "C" void SPHER_SetRadiusCorrection(double corr);

 //--vt--
 /**
  * @fn int SPHER_GetNbSPHER(void)
  * @brief Get the number of SPHER
  *
  * @cond PYDOC
  * python usage : nb_SPHER = SPHER_GetNbSPHER()
  * 
  * @return nb_SPHER (integer) : the number of SPHER
  * @endcond
  *
  * @cond CDOC
  * @return (int) the number of SPHER
  * @endcond
  */
  extern "C" int SPHER_GetNbSPHER(void);

/* /\** */
/*   * @fn void SPHER_GetSPHER2BDYTY(int * vecteur_in, int length) */
/*   * @brief return the table spher2bdyty */
/*   * */
/*   * spher2bdyty gives for a SPHER itact the corresponding body in BDYTY: spher2bdyty(itact) */
/*   * */
/*   * @param[inout] vecteur_in (int[length]) : an array that will be filled with spher2bdyty */
/*   * @param[in]    length     (int)         : the size of vecteur_in, must be the number of SPHER */
/*   *\/ */
/* extern "C" void SPHER_GetSPHER2BDYTY(int * vecteur_in, int length);*/

/**
  * @fn void SPHER_GetSPHER2BDYTY(int** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of map SPHER2bdyty
  *
  * @cond PYDOC
  * usage : polyr2bdyty = SPHER_GetSPHER2BDYTY()
  * @return polyr2bdyty (integer 2D-array) : the polyr2bdyty map
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] matrix_out (int**) : reference on a copy of the array polyr2bdyty
  * @param[out]    dim1 (int*)        : number of field on the map
  * @param[out]    dim2 (int*)        : number of polyr
  * @endcond
  */
  extern "C" void SPHER_GetSPHER2BDYTY(int** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void SPHER_GetPtrSPHER2BDYTY(int ** pointer_out, int * dim, int * dim2)
  * @brief return a pointer onto the map spher2bdyty
  *
  * @cond PYDOC
  * python usage : spher2bdyty = SPHER_GetPtrSPHER2BDYTY()
  *
  * @return spher2bdyty (integer array) : reference on map between spher rank and body rank
  * @endcond
  *
  * @cond CDOC
  * @param[out] pointer_out (int**) : a pointer on the array spher2bdyty
  * @param[out] dim1 (int *)        : first dim of pointer_out
  * @param[out] dim2 (int *)        : second dim of pointer_out
  * @endcond
  */
  extern "C" void SPHER_GetPtrSPHER2BDYTY(int** pointer_out, int* dim1, int* dim2);


/**
  * @fn double SPHER_GetContactorRadius(int itact)
  * @brief Get the radius of a SPHER contactor
  *
  * @cond PYDOC
  * python usage : radius = SPHER_GetContactorRadius(itact)
  * @param[in] itact (integer) : id of a SPHER
  * @return radius (double)    : the radius of the SPHER number itact
  * @endcond
  *
  * @cond CDOC
  * @param[in] itact (int) : id of a SPHER
  * @return (double) the radius of the SPHER number itact
  * @endcond
  */
  extern "C" double SPHER_GetContactorRadius(int itact);

 /**
  * @fn void SPHER_GetContactorCoor(int itacty, double** r8_vector, int* r8_size)
  * @brief get coordinates of the center of a given SPHER
  *
  * @cond PYDOC
  * usage : vector = SPHER_GetContactorCoor(itacty)
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
  extern "C" void SPHER_GetContactorCoor(int itacty, double** r8_vector, int* r8_size);

 /**
  * @fn void SPHER_GetContactorCoorb(int itacty, double** r8_vector, int* r8_size)
  * @brief get coordinates at the begin of the time step of the center of a given SPHER
  *
  * @cond PYDOC
  * usage : vector = SPHER_GetContactorCoorb(itacty)
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
  extern "C" void SPHER_GetContactorCoorb(int itacty, double** r8_vector, int* r8_size);

 /**
  * @fn int SPHER_IsVisible(int itacty)
  * @brief return if a given contactor is attached to a visible body 
  *
  * @cond PYDOC
  * python usage : visible = SPHER_IsVisible(itacty)
  * @param[in] itacty (integer) : id of the contactor we want visibility
  * @return visible (integer) : 1 if body is visible, 0 else
  * @endcond
  *
  * @cond CDOC
  * @param[in] itacty (int) : id of the contactor we want visibility
  * @return (int) 1 if body is visible, 0 else
  * @endcond
  */
  extern "C" int SPHER_IsVisible(int itacty);

 // external vtk visu


 /**
  * @fn void SPHER_InitOutlines(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the outlines of all SPHER
  *
  * @cond PYDOC
  * usage : outlines = SPHER_InitOutlines()
  * @return outlines (double array) : a reference on outlines_SPHER
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on outlines_SPHER array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void SPHER_InitOutlines(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void SPHER_InitScalarFields(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the scalar fields of all SPHER
  *
  * @cond PYDOC
  * usage : scalarfields = SPHER_InitScalarfields()
  * @return scalarfields (double array) : reference on scalarfields_SPHER array
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on scalarfields array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void SPHER_InitScalarFields(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void SPHER_UpdatePostdata(void)
  * @brief Update values of outlines_SPHER and scalarfields_SPHER pointers
  *
  * @cond PYDOC
  * usage : SPHER_UpdatePostdata
  * @endcond
  *
  */
  extern "C" void SPHER_UpdatePostdata(void);

 /**
  * @fn void SPHER_GetNbPointOutlines(int ** pointer_out, int * length)
  *
  * @brief Get the list of cumulated outline points number
  *
  * @cond PYDOC
  * python usage : nb_pointOutlines = SPHER_GetNbPointOutlines()
  * @return nb_pointOutlines (integer array) : the cumulated number of outline points of the SPHER
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (int **) : reference on nb_point_outlines array
  * @param[in,out] length (int *)       : first dimension of pointer_out
  * @endcond
  */
  extern "C" void SPHER_GetNbPointOutlines(int** pointer_out, int* length);
  
 /**
  * @fn int SPHER_GetNbScalarFields(void)
  *
  * @brief Get the number of scalar fields of a SPHER
  *
  * @cond PYDOC
  * python usage : nb_scalarfields = SPHER_GetNbScalarFields()
  * @return nb_scalarfields (integer) : the number of scalar fields of a SPHER
  * @endcond
  *
  * @cond CDOC
  * @return nb_scalarfields (int) : the number of scalar fields of a SPHER
  * @endcond
  */
  extern "C" int SPHER_GetNbScalarFields(void);
  
 /**
  * @fn void SPHER_GetPtrAllConnectivities(int ** i4_vector, int * i4_size)
  * @brief Get a reference on the connectivities of all SPHER
  *
  * @cond PYDOC
  * usage : connec = SPHER_GetPtrAllConnectivities()
  * @return connec (integer array) : a reference on all_connectivities
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (int **) : reference on all_connectivities array
  * @param[in,out] length (int *)       : length of pointer_out
  * @endcond
  *
  */
  extern "C" void SPHER_GetPtrAllConnectivities(int ** pointer_out, int * length);

 /**
   * @fn void SPHER_CleanMemory(void)
   * @brief Free all memory allocated within SPHER module
   *
   * @cond PYDOC
   * python usage : SPHER_CleanMemory()
   * @endcond
   */
   extern "C" void SPHER_CleanMemory(void);
  
#endif /* wrap_SPHER*/
