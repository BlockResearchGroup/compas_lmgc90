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

#ifndef wrap_CSxxx_h
#define wrap_CSxxx_h
    
 /**
  * @fn void CSxxx_LoadTactors(void)
  * @brief Load CSxxx from MAILx and Initialize existing_entities
  *
  * @cond PYDOC
  * python usage : CSxxx_LoadTactors()
  * @endcond
  */
  extern "C" void CSxxx_LoadTactors(void);

 /**
  * @fn void CSxxx_PushPreconNodes(void)
  * @brief set CSxxx supporting nodes as precon
  *
  * @cond PYDOC
  * python usage : CSxxx_PushPreconNodes()
  * @endcond
  */
  extern "C" void CSxxx_PushPreconNodes(void);

 /**
  * @fn void CSxxx_FlipOrientation(int ibdyty)
  * @brief Flip normal of all CSxxx of a given MAILx body
  *
  * @cond PYDOC
  * python usage : CSxxx_FlipOrientation(ibdyty)
  * @param[in] ibdyty (integer) : rank of desired body
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : rank of desired body
  * @endcond
  */
  extern "C" void CSxxx_FlipOrientation(int ibdyty);

 /**
  * @fn void CSxxx_FlipOrientationOnePatch(int ibdyty, int icspxx)
  * @brief Flip normal of CSxxx belonging to given patch of a given MAILx body
  *
  * @cond PYDOC
  * python usage : CSxxx_FlipOrientationOnePatch(ibdyty,icspxx)
  * @param[in] ibdyty (integer) : rank of desired body
  * @param[in] icspxx (integer) : rank of desired patch
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : rank of desired body
  * @param[in] icspxx (int) : rank of desired patch
  * @endcond
  */
extern "C" void CSxxx_FlipOrientationOnePatch(int ibdyty,int icspxx);

 /**
  * @fn void CSxxx_SetShrink(double shrink)
  * @brief shrink position of nodes in CSxxx contactors
  *
  * @cond PYDOC
  * python usage : CSxxx_SetShrink(shrink)
  * @param[in] shrink (real) : shrink value
  * @endcond
  *
  * @cond CDOC
  * @param[in] shrink (double) : shrink value
  * @endcond
  */
  extern "C" void CSxxx_SetShrink(double shrink);

  /**
  * @fn void CSxxx_SetQuadrature(int ivalue)
  * @brief Set the contact quadrature rule of a CSxxx face.
  * OBSOLETE FUNCTION !!!! To remove in the future
  *
  * @cond PYDOC
  * python usage : CSxxx_SetQuadrature(ivalue)
  * @param[in] ivalue (integer) : degree on CSxxx contactor
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue (int) : quadrature degree on CSxxx contactor
  * @endcond
  */
  extern "C" void CSxxx_SetQuadrature(int ivalue);

 /**
  * @fn void CSxxx_AddReac(char * cvalue1_c, int ivalue, double * rvector_in, int rlength_in)
  * @brief Apply an external reaction on a CSxxx.
  *
  * @cond PYDOC
  * python usage : CSxxx_AddReac(datatype, iCSxxx, reac)
  * @param[in] datatype (string of size 5) : the vector to set
  * @param[in] iCSxxx (integer)            : id of the CSpxx
  * @param[in] reac (double array)         : the value to add
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue (int)    : id of the CSxxx
  * @param[in] ivalue (double) : pressure
  * @endcond
  */
  extern "C" void CSxxx_AddReac(char * cvalue1_c, int ivalue, double * rvector_in, int rlength_in);

 /**
  * @fn void CSpxx_ApplySurfaceLoad(int ivalue,double rvalue)
  * @brief Apply an external surface load on a CSpxx.
  *
  * @cond PYDOC
  * python usage : CSpxx_ApplySurfaceLoad(ivalue,rvalue)
  * @param[in] ivalue (integer) : id of the CSpxx
  * @param[in] rvalue (real)    : surface load
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue (int) : id of the CSpxx
  * @param[in] ivalue (double) : surface load
  * @endcond
  */
  extern "C" void CSpxx_ApplySurfaceLoad(int ivalue, double * rvector_in, int rlength_in);

 /**
  * @fn void CSpxx_ApplyPressure(int ivalue,double rvalue)
  * @brief Apply an external pressure on a CSpxx.
  *
  * @cond PYDOC
  * python usage : CSpxx_ApplyPressure(ivalue,rvalue)
  * @param[in] ivalue (integer) : id of the CSpxx
  * @param[in] rvalue (real)    : pressure
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue (int) : id of the CSpxx
  * @param[in] ivalue (double) : pressure
  * @endcond
  */
  extern "C" void CSpxx_ApplyPressure(int ivalue, double rvalue);

 /**
  * @fn int CSxxx_GetNbCSxxx(void)
  * @brief Get the number of CSxxx
  *
  * @cond PYDOC
  * usage : nb_CSxxx = CSxxx_GetNbCSxxx()
  * @param nb_CSxxx (integer) : number of CSxxx in container
  * @endcond
  *
  * @cond CDOC
  * @return nb_CSxxx (int) : number of CSxxx in container
  * @endcond
  */
  extern "C" int CSxxx_GetNbCSxxx(void);

 /**
  * @fn void CSpxx_GetAllConnec(int** i4_vector, int * i4_size)
  * @brief return connectivity of all CS in a single vector using gloab node numbering of mecaMAILx
  *
  * @cond PYDOC
  * python usage : connec = CSxxx_getAllConnec()
  * @return    connec (integer 1D-array) : connectiviy of CSxxx elements
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] i4_vector (int **) : the connectivity
  * @param[in]     i4_size (int *)    : vector dimension
  * @endcond
  */
  extern "C" void CSpxx_GetAllConnec(int ** i4_vector, int * i4_size);

 /**
  * @fn void CSpxx_GetAllData(int** i4_matrix, int * i4_dim1, int * i4_dim2, double ** matrix_out, int * dim1, int * dim2 )
  * @brief return integer (ibdyty, itacty, i_as) and real data (normal) of all CSxxx
  *
  * @cond PYDOC
  * python usage : idata, rdata = CSxxx_getAllData()
  * @return    idata (integer 2D-array) : integer data array
  * @return    rdata (real 2D-array)    : real data array
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] i4_matrix  (int **) : the integer data (i_bdyty, i_tacty, i_sci)
  * @param[in]     i4_dim1    (int *)  : 1st dimension of i4_matrix
  * @param[in]     i4_dim2    (int *)  : 2dn dimension of i4_matrix
  * @param[in,out] matrix_out (int *)  : the real data (normal)
  * @param[in]        dim1    (int *)  : 1st dimension of matrix_out
  * @param[in]        dim2    (int *)  : 2nd dimension of matrix_out
  * @endcond
  */
  extern "C" void CSpxx_GetAllData(int ** i4_matrix, int * i4_dim1, int * i4_dim2, double ** matrix_out, int * dim1, int * dim2 );


/**
  * @fn void CSxxx_CleanMemory(void)
  * @brief Free all memory allocated within CSxxx module
  *
  * @cond PYDOC
  * python usage : CSxxx_CleanMemory()
  * @endcond
  */
  extern "C" void CSxxx_CleanMemory(void);

#endif /* wrap_CSxxx_h */
