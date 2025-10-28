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

#ifndef wrap_POLYG_h
#define wrap_POLYG_h

 /**
  * @fn void POLYG_LoadTactors(void)
  * @brief load POLYG from RBDY2  and initialize existing_entites
  *
  * @cond PYDOC
  * python usage : POLYG_LoadTactors()
  * @endcond
  */
  extern "C" void POLYG_LoadTactors(void);

// B.o.B.o.R
// /**
//  * @fn void POLYG_UpdateRadii(void)
//  * @brief Update radii (mean,radius, max, min) for detection
//  *
//  * @cond PYDOC
//  * python usage : POLYG_UpdateRadii()
//  * @endcond
//  */
//  extern "C" void POLYG_UpdateRadii(void);

/**
  * @fn double POLYG_GetMaxRadius(void)
  * @brief give max radius used during detection
  *
  * @cond PYDOC
  * python usage : POLYG_GetMaxRadius()
  * @endcond
  */
  extern "C" double POLYG_GetMinRadius(void);
/**
  * @fn double POLYG_GetMinRadius(void)
  * @brief give min radius used during detection
  *
  * @cond PYDOC
  * python usage : POLYG_GetMinRadius()
  * @endcond
  */
  extern "C" double POLYG_GetMaxRadius(void);

///**
//  * @fn void POLYG_UpdateNormalsRef(void)
//  * @brief Update normals taken into account dilation
//  *
//  * @cond PYDOC
//  * python usage : POLYG_UpdateNormalsRef()
//  * @endcond
//  */
//  extern "C" void POLYG_UpdateNormalsRef(void);

 /**
  * @fn int POLYG_GetNbPOLYG(void)
  * @brief Get the number of POLYG in container
  *
  * @cond PYDOC
  * python usage : nb_polyg = POLYG_GetNbPOLYG()
  *
  * @return nb_polyg (integer) : the number of POLYG in container
  * @endcond
  *
  * @cond CDOC
  * @return nb_polyg (int) : the number of POLYG in container
  * @endcond
  */
  extern "C" int POLYG_GetNbPOLYG(void);
  
 /* /\** */
 /*  * @fn void POLYG_GetPOLYG2RBDY2(int** i4_vector, int* i4_size) */
 /*  * @brief return the map polyg2rbdy2 */
 /*  * */
 /*  * @cond PYDOC */
 /*  * python usage : polyg2rbdy2 = POLYG_GetPOLYG2RBDY2() */
 /*  * @return    polyg2rbdy2 (integer array) : map between polyg rank and body rank */
 /*  * @endcond */
 /*  * */
 /*  * @cond CDOC */
 /*  * @param[out] i4_vector (int**) : map between polyg rank and body rank */
 /*  * @param[out] i4_size (int*)    : the size of i4_vector */
 /*  * @endcond */
 /*  *\/ */
 /*  extern "C" void POLYG_GetPOLYG2RBDY2(int** i4_vector, int* i4_size); */
/*  */

/**
  * @fn void POLYG_GetPOLYG2BDYTY(int** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of map POLYG2bdyty
  *
  * @cond PYDOC
  * usage : polyr2bdyty = POLYG_GetPOLYG2BDYTY()
  * @return polyr2bdyty (integer 2D-array) : the polyr2bdyty map
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] matrix_out (int**) : reference on a copy of the array polyr2bdyty
  * @param[out]    dim1 (int*)        : number of field on the map
  * @param[out]    dim2 (int*)        : number of polyr
  * @endcond
  */
  extern "C" void POLYG_GetPOLYG2BDYTY(int** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void POLYG_GetPtrPOLYG2BDYTY(int ** pointer_out, int * dim, int * dim2)
  * @brief return a pointer onto the map polyg2rbdy2
  *
  * @cond PYDOC
  * python usage : polyg2rbdy2 = POLYG_GetPtrPOLYG2BDYTY()
  *
  * @return polyg2rbdy2 (integer array) : reference on map between polyg rank and body/tactor rank
  * @endcond
  *
  * @cond CDOC
  * @param[out] pointer_out (int **) : a pointer on the array polyg2rbdy2
  * @param[out] dim1 (int *)         : first dim of pointer_out
  * @param[out] dim2 (int *)         : second dim of pointer_out
  * @endcond
  */
  extern "C" void POLYG_GetPtrPOLYG2BDYTY(int** pointer_out, int* dim1, int* dim2);


 /**
  * @fn int POLYG_IsVisible(int itact)
  * @brief return if a body visible
  *
  * @cond PYDOC
  * usage : visible = POLYG_IsVisible(itact)
  * @param[in] itact (integer)   : rank of POLYG
  * @param     visible (integer) : 1 if body is visible, 0 else
  * @endcond
  *
  * @cond CDOC
  * @param[in] itact (int)   : rank of POLYG
  * @return    visible (int) : 1 if body is visible, 0 else
  * @endcond
  */
  extern "C" int POLYG_IsVisible(int itact);

 /**
  * @fn double POLYG_GetContactorRadius(int itact)
  * @brief Get the radius of a given POLYG
  *
  * @cond PYDOC
  * python usage : radius = POLYG_GetContactorRadius(itact)
  * @param[in] itact (integer) : rank of a POLYG
  * @return radius (double) : the radius of the POLYG of rank itact
  * @endcond
  *
  * @cond CDOC
  * @param[in] itact (int)     : rank of a POLYG
  * @return    radius (double) : the radius of the POLYG of rank itact
  * @endcond
  */
  extern "C" double POLYG_GetContactorRadius(int itact);

  
 /**
  * @fn int POLYG_GetNbVertices(int ibdyty)
  * @brief Get the number of vertices of the first POLYG of a body
  *
  * @cond PYDOC
  * python usage : nb_vertices = POLYG_GetNbVertices(ibdyty)
  * @param[in] ibdyty (integer)      : rank of a body
  * @return    nb_vertices (integer) : the number of vertices of the first POLYG of the body
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : rank of a body
  * @return    (int) : the number of vertices of the first POLYG of the body
  * @endcond
  */
  extern "C" int POLYG_GetNbVertices(int ibdyty);

 /**
  * @fn void POLYG_GetVertices(int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get the coordinates of the vertices of the first POLYG of a body
  *
  * @cond PYDOC
  * usage : vertices = POLYG_GetVertices(ibdyty)
  * @param ibdyty (integer)           : rank of considered body
  * @param vertices (double 2D-array) : the coordinates of the vertices
  * @endcond
  *
  * @cond CDOC
  * @param[in]     ivalue1 (int)      : rank of considered body
  * @param[out] matrix_out (double**) : the vertices
  * @param[out] dim1 (int*)           : first dimension of matrix_out
  * @param[out] dim2 (int*)           : first dimension of matrix_out
  * @endcond
  */
  extern "C" void POLYG_GetVertices(int ivalue1, double** matrix_out, int* dim1, int* dim2);

/**
  * @fn int POLYG_GetNbVertex(int itacty)
  * @brief Get the number of vertices of a POLYG
  *
  * @cond PYDOC
  * usage : nb_vertex = POLYG_GetNpVertex(itacty)
  * @param[in] itacty (integer) : id of the POLYG contactor
  * @return nb_vertex (int) : the number of vertices of the POLYG
  * @endcond
  *
  * @cond CDOC
  * @param[in] itacty (int) : id of the POLYG contactor
  * @return (int) the number of vertices of the POLYG
  * @endcond
  */
  extern "C" int POLYG_GetNbVertex(int itacty);
 
/**
  * @fn void POLYG_GetVertex(int itacty, double * rvector_out, int rlength_out)
  * @brief Get the outline of a POLYG
  *
  * @cond PYDOC
  * usage : vertex = POLYG_GetVertex(itacty, length)
  * @param ibdyty (integer)      : rank of considered body
  * @param length (integer)      : 2 * number of vertices
  * @param vertex (double array) : the coordinates of the vertices
  * @endcond
  *
  * @cond CDOC
  * @param[in]     ivalue1 (int)         : rank of considered POLYG
  * @param[in,out] vector_out (double *) : the vertices seen as a vector, must be of the right size
  * @param[in]     length (int)          : the length of vector_out
  * @endcond
  */
  extern "C" void POLYG_GetVertex(int itacty, double * rvector_out, int rlength_out);
  /**
  * @fn int POLYG_GetBodyId(int itacty)
  * @brief Get the id of the body which the tactor belongs
  *
  * @cond PYDOC
  * python usage : id = POLYG_GetBodyId(itacty)
  * @param[in] itacty (integer)      : rank of a POLYG contactor
  * @return    id (integer) : the id of the body
  * @endcond
  *
  * @cond CDOC
  * @param[in] itacty (int) : rank of the contactor
  * @return    (int) : the id of the body
  * @endcond
  */
  extern "C" int POLYG_GetBodyId(int itacty);


 /**
  * @fn void POLYG_InitOutlines(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the outlines of all POLYG
  *
  * @cond PYDOC
  * usage : outlines = POLYG_InitOutlines()
  * @return outlines (double array) : a reference on outlines_POLYG
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on outlines_POLYG array
  * @param[in]     dim1 (int *)            : first dimension of pointer_out
  * @param[in]     dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void POLYG_InitOutlines(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void POLYG_InitScalarFields(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the scalar fields of all POLYG
  *
  * @cond PYDOC
  * usage : scalarfields = POLYG_InitScalarfields()
  * @return scalarfields (double array) : reference on scalarfields_POLYG array
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on scalarfields array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void POLYG_InitScalarFields(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void POLYG_UpdatePostdata(void)
  * @brief Update values of outlines_POLYG and scalarfields_POLYG pointers
  *
  * @cond PYDOC
  * usage : POLYG_UpdatePostdata
  * @endcond
  *
  */
  extern "C" void POLYG_UpdatePostdata(void);

 /**
  * @fn void POLYG_GetNbPointOutlines(int ** pointer_out, int * length)
  *
  * @brief Get the list of cumulated outline points number
  *
  * @cond PYDOC
  * python usage : nb_pointOutlines = POLYG_GetNbPointOutlines()
  * @return nb_pointOutlines (integer array) : the cumulated number of outline points of the POLYG
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (int **) : reference on nb_point_outlines array
  * @param[in,out] length (int *)       : first dimension of pointer_out
  * @endcond
  */
  extern "C" void POLYG_GetNbPointOutlines(int ** pointer_out, int * length);
  
 /**
  * @fn int POLYG_GetNbScalarFields(void)
  *
  * @brief Get the number of scalar fields of a POLYG
  *
  * @cond PYDOC
  * python usage : nb_scalarfields = POLYG_GetNbScalarFields()
  *
  * @return nb_scalarfields (integer) : the number of scalar fields of a POLYG
  * @endcond
  *
  * @cond CDOC
  * @return nb_scalarfields (int) : the number of scalar fields of a POLYG
  * @endcond
  */
  extern "C" int POLYG_GetNbScalarFields(void);

 /**
  * @fn POLYG_SetXdilation(itacty,ivertex,V)
  *
  * @brief 
  *
  * @cond PYDOC
  * python usage : POLYG_SetXdilation(1,1,[2.,3.])
  *
  * @return 
  * @endcond
  *
  * @cond CDOC
  * @param[in] itacty (int)               : rank of RBDY2
  * @param[in] ivertex (int)              : corresponding dof to set
  * @param[in] vector_in (double[length]) :  
  * @param[in] length (int)               : size of vector_in
  * @endcond
  */
extern "C" void POLYG_SetXdilation(int itacty,int ivertex ,double * rvector_in, int rlength_in=2);
 /**
  * @fn POLYG_SetVdilation(itacty,ivertex,V)
  *
  * @brief 
  *
  * @cond PYDOC
  * python usage : POLYG_SetVdilation(1,1,[2.,3.])
  *
  * @return 
  * @endcond
  *
  * @cond CDOC
  * @param[in] itacty (int)               : rank of RBDY2
  * @param[in] ivertex (int)              : corresponding dof to set
  * @param[in] vector_in (double[length]) :  
  * @param[in] length (int)               : size of vector_in
  * @endcond
  */
extern "C" void POLYG_SetVdilation(int itacty,int ivertex,double * rvector_in, int rlength_in=2);

 /**
   * @fn void POLYG_CleanMemory(void)
   * @brief Free all memory allocated within POLYG module
   *
   * @cond PYDOC
   * python usage : POLYG_CleanMemory()
   * @endcond
   */
   extern "C" void POLYG_CleanMemory(void);

#endif /* wrap_POLYG */
