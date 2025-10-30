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

#ifndef wrap_POLYR_h
#define wrap_POLYR_h

 /**
  * @fn void POLYR_LoadTactors(void)
  * @brief load POLYR from RBDY3 or MAILx and initialize existing_entites
  *
  * @cond PYDOC
  * python usage : POLYR_LoadTactors()
  * @endcond
  */
  extern "C" void POLYR_LoadTactors(void);

 
// fd: a virer ; passer par polyr2bdyty + getcolor sur modèle bulk
 /**
  * @fn void POLYR_GetContactorColor(int itact, char ** c5)
  * @brief Get the color of a given POLYR
  *
  * @cond PYDOC
  * python usage : color = POLYR_GetContactorColor(itacty)
  * @param[in] itacty (integer) : rank of POLYR
  * @return color (string)     : the color of the POLYR itact
  * @endcond
  *
  * @cond CDOC
  * @param[in]  itact (int) : rank of POLYR
  * @param[out] c5 (char**) : pointer on a 5 characters string
  * @endcond
  */
  extern "C" void POLYR_GetContactorColor(int itact, char** c5);
 
 /**
  * @fn void POLYR_SaveVertex(void)
  * @brief write position of vertex in a file
  *
  * @cond PYDOC
  * python usage : POLYR_SaveVertex()
  * @endcond
  */
  extern "C" void POLYR_SaveVertex(void);

 /**
  * @fn void POLYR_ModifyRadius(double ratio)
  * @brief apply an amplification/reduction size factor
  *
  * @cond PYDOC
  * python usage : POLYR_ModifyRadius(ratio)
  * @param[in] ratio (real) : ratio factor
  * @endcond
  *
  * @cond PYDOC
  * @param[in] ratio (double) : ratio factor
  * @endcond
  */
  extern "C" void POLYR_ModifyRadius(double ratio);

 /**
  * @fn void POLYR_SetThresholdBigPolyr(double ratio)
  * @brief define the threshold between a plain and a big polyr. 
  *        big polyr are such that radius > threshold*mean_radius. 
  *        default threshold = 4. Must be defined before the load of tactors.    
  *
  * @cond PYDOC
  * python usage : POLYR_SetThresholdBigPolyr(ratio)
  * @param[in] ratio (real) : ratio factor
  * @endcond
  *
  * @cond PYDOC
  * @param[in] ratio (double) : ratio factor
  * @endcond
  */
  extern "C" void POLYR_SetThresholdBigPolyr(double ratio);

 /**
  * @fn void POLYR_SetBigPolyr(int itacty)
  * @brief impose explicitly that an object is big. Must be set after the load of tactors.
  *
  * @cond PYDOC
  * python usage : POLYR_SetBigPolyr(itacty)
  * @param[in] itacty (integer) : rank of the polyr 
  * @endcond
  *
  * @cond PYDOC
  * @param[in] itacty (int) : rank of the polyr
  * @endcond
  */
extern "C" void POLYR_SetBigPolyr(int itacty);

 /**
  * @fn void POLYR_SetNbBigPolyr(int nb)
  * @brief impose explicitly the number of big POLYR. Must be set after the load of tactors.
  *
  * @cond PYDOC
  * python usage : POLYR_SetNbBigPolyr(nb)
  * @param[in] nb (integer) : number of polyr 
  * @endcond
  *
  * @cond PYDOC
  * @param[in] number (int) : number of polyr
  * @endcond
  */
extern "C" void POLYR_SetNbBigPolyr(int nb);

 /**
  * @fn void POLYR_SkipTopoBigPolyr(void)
  * @brief skip the topological decomposition of a big POLYR.
  *        its surface is considered as a soup of triangle. 
  *        usefull with complicated surface using Cundall CP detection
  *
  * @cond PYDOC
  * python usage : POLYR_SkipTopoBigPolyr()
  * @endcond
  *
  * @cond PYDOC
  * @endcond
  */
  extern "C" void POLYR_SkipTopoBigPolyr(void);

 /**
  * @fn void POLYR_SkipAutomaticReorientation(void)
  * @brief disable automatic reorientation (which works only with convex POLYR).
  *
  * @cond PYDOC
  * python usage : POLYR_SkipAutomaticReorientation()
  * @endcond
  *
  * \n Disable the automatic reorientation of normals performed by lmgc90.\n 
  * This is necessary when using non-convex objects.       \n 
  */
  extern "C" void POLYR_SkipAutomaticReorientation(void);

 /**
  * @fn void POLYR_SkipHEBuild(void)
  * @brief disable Half-Edge structure generation (HE is necessary for non convex contact detection)
  *
  * @cond PYDOC
  * python usage : POLYR_SkipHEBuild()
  * @endcond
  *
  * \n Disable the Half-Edge structure generation performed by lmgc90.\n 
  * This is necessary when testing the import of strange object.      \n  
  */
  extern "C" void POLYR_SkipHEBuild(void);

 /**
  * @fn void POLYR_TopologyAngle(double angle)
  * @brief set the maximum angle (between 0 and 180 degree) threshold between 2 elements to declare them as belonging to the same topological face
  *
  * @cond PYDOC
  * python usage : POLYR_TopologyAngle(angle)
  * @endcond
  *
  */
  extern "C" void POLYR_TopologyAngle(double angle);

 /**
  * @fn void POLYR_FlatnessAngle(double angle)
  * @brief set the maximum angle (between 0 and 180 degree) variation between elements of a topological face to declare it as flat
  *
  * @cond PYDOC
  * python usage : POLYR_FlatnessAngle(angle)
  * @endcond
  *
  */
  extern "C" void POLYR_FlatnessAngle(double angle);

 /**
  * @fn void POLYR_GetWireframe(int ivalue1, double angle, double ** matrix_out, int * dim1, int* dim2, int ** pointer_out2, int * length2)
  * @brief Get wireframe of a POLYR
  *
  * @cond PYDOC
  * python usage : coor,connectivity = POLYR_GetWireframe(itacty, angle)
  * @param[in] itacty (integer)   : rank of the POLYR
  * @param[in] angle (double)    : threshold angle to skip some nodes on boundary of faces of the POLYR
  * @return    coor         (double  array) : reference on the coor vector seen as a numpy array of size [nb_point,3]
  *            connectivity (integer array) : reference on the connectivity vector seen as a numpy array
  * @endcond
  *
  * @cond CDOC
  * @param[in]  ivalue1 (int)          : rank of the POLYR
  * @param[out] matrix_out (double **) : out matrix
  * @param[out] dim1 (int*)            : first size of out matrix
  * @param[out] dim2 (int*)            : second size of out matrix
  * @param[out] pointer_out2 (int **)  : reference on the vector
  * @param[out]  length2 (int*)        : reference on the length of the out vector
  * @endcond
  */
  extern "C" void POLYR_GetWireframe(int ivalue1, double angle, double ** matrix_out, int * dim1, int * dim2, int ** pointer_out2, int * length2);

 /**
  * @fn void POLYR_GetVertex(int itacty, double** matrix_out, int* dim1, int* dim2)
  * @brief Get the outline of a POLYR in almost current configuration
  *
  * If the POLYR is a real POLYR the current position of the center of the POLYR
  * is used but the local frame for the orientation is the on in detection configuration.
  *
  * If the POLYR is in fact a POLYD the current position of nodes of the mesh are used.
  *
  * @cond PYDOC
  * usage : vertex = POLYR_GetVertex(itacty)
  * @param  itacty (integer)         : rank of considered POLYR
  * @return vertex (double 2D-array) : the coordinates of the vertices
  * @endcond
  *
  * @cond CDOC
  * @param[in]     itacty (int)          : rank of considered POLYR
  * @param[in,out] matrix_out (double**) : reference on the array with vertices coordinates
  * @param[out]    dim1 (int*)           : number of dof per vertex
  * @param[out]    dim2 (int*)           : number of vertices
  * @endcond
  */
  extern "C" void POLYR_GetVertex(int itacty, double** matrix_out, int* dim1, int* dim2);

/**
  * @fn void POLYR_GetPtrVertexTT(int itacty, double** pointer_out, int* dim1, int* dim2)
  * @brief Get a pointer on the outline of a POLYR in detection configuration
  *
  * @cond PYDOC
  * usage : vertex = POLYR_GetPtrVertexTT(itacty)
  * @param  itacty (integer)         : rank of considered POLYR
  * @return vertex (double 2D-array) : the coordinates of the vertices
  * @endcond
  *
  * @cond CDOC
  * @param[in]     itacty (int)           : rank of considered POLYR
  * @param[in,out] pointer_out (double**) : reference on the vertex of the POLYR
  * @param[out]    dim1 (int*)            : number of dof per vertex
  * @param[out]    dim2 (int*)            : number of vertices
  * @endcond
  */
  extern "C" void POLYR_GetPtrVertexTT(int itacty, double** pointer_out, int* dim1, int* dim2);

/**
  * @fn void POLYR_GetPtrNormalTT(int itacty, double** pointer_out, int* dim1, int* dim2)
  * @brief Get a pointer on the outline of a POLYR in detection configuration - be carefull to move polyr 
  *
  * @cond PYDOC
  * usage : normal = POLYR_GetPtrNormalTT(itacty)
  * @param  itacty (integer)         : rank of considered POLYR
  * @return normal (double 2D-array) : the coordinates of the vertices
  * @endcond
  *
  * @cond CDOC
  * @param[in]     itacty (int)           : rank of considered POLYR
  * @param[in,out] pointer_out (double**) : reference on the normal of the POLYR
  * @param[out]    dim1 (int*)            : number of dof per normal
  * @param[out]    dim2 (int*)            : number of vertices
  * @endcond
  */
  extern "C" void POLYR_GetPtrNormalTT(int itacty, double** pointer_out, int* dim1, int* dim2);

 /**
  * @fn void POLYR_MoveToConfigurationTT(void)
  * @brief move the polyr in the configuration TT ; mandatory to get the wireframe in deformed configuration
  *
  * @cond PYDOC
  * python usage : POLYR_MoveToConfigurationTT()
  * @endcond
  */
  extern "C" void POLYR_MoveToConfigurationTT(void);


/**
  * @fn void POLYR_GetPOLYR2BDYTY(int** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of map POLYR2bdyty
  *
  * @cond PYDOC
  * usage : polyr2bdyty = POLYR_GetPOLYR2BDYTY()
  * @return polyr2bdyty (integer 2D-array) : the polyr2bdyty map
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] matrix_out (int**) : reference on a copy of the array polyr2bdyty
  * @param[out]    dim1 (int*)        : number of field on the map
  * @param[out]    dim2 (int*)        : number of polyr
  * @endcond
  */
  extern "C" void POLYR_GetPOLYR2BDYTY(int** matrix_out, int* dim1, int* dim2);

/**
  * @fn void POLYR_GetPtrPOLYR2BDYTY(int** pointer_out, int* dim1, int* dim2)
  * @brief Get a pointer on map POLYR2bdyty
  *
  * @cond PYDOC
  * usage : polyr2bdyty = POLYR_GetPtrPOLYR2BDYTY()
  * @return polyr2bdyty (integer 2D-array) : a pointer in the polyr2bdyty map
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] matrix_out (int**) : reference on the array polyr2bdyty
  * @param[out]    dim1 (int*)        : number of field on the map
  * @param[out]    dim2 (int*)        : number of polyr
  * @endcond
  */
  extern "C" void POLYR_GetPtrPOLYR2BDYTY(int** pointer_out, int* dim1, int* dim2);

 /* /\** */
 /*  * @fn void POLYR_GetNb(int * ires) */
 /*  * @brief get the number of POLYR */
 /*  * */
 /*  * @cond PYDOC */
 /*  * python usage : POLYR_GetNb(ires) */
 /*  * @endcond */
 /*  * */
 /*  *\/ */
 /*  extern "C" void POLYR_GetNb(int * ires); */

 /**
  * @fn int POLYR_IsVisible(int itacty)
  * @brief return if a given contactor is attached to a visible body 
  *
  * @cond PYDOC
  * python usage : visible = POLYR_IsVisible(itacty)
  * @param[in] itacty (integer) : id of the contactor we want visibility
  * @return visible (integer) : 1 if body is visible, 0 else
  * @endcond
  *
  * @cond CDOC
  * @param[in] itacty (int) : id of the contactor we want visibility
  * @return (int) 1 if body is visible, 0 else
  * @endcond
  */
  extern "C" int POLYR_IsVisible(int itacty);


 // external vtk visu

 /**
  * @fn int POLYR_GetNbPOLYR(void)
  * @brief Get the number of POLYR
  *
  * @cond PYDOC
  * python usage : nb_POLYR = POLYR_GetNbPOLYR()
  * 
  * @return nb_POLYR (integer) : the number of POLYR
  * @endcond
  *
  * @cond CDOC
  * @return (int) the number of POLYR
  * @endcond
  */
  extern "C" int POLYR_GetNbPOLYR(void);

 /**
  * @fn void POLYR_InitOutlines(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the outlines of all POLYR
  *
  * @cond PYDOC
  * usage : outlines = POLYR_InitOutlines()
  * @return outlines (double array) : a reference on outlines_POLYR
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on outlines_POLYR array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void POLYR_InitOutlines(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void POLYR_InitScalarFields(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the scalar fields of all POLYR
  *
  * @cond PYDOC
  * usage : scalarfields = POLYR_InitScalarfields()
  * @return scalarfields (double array) : reference on scalarfields_POLYR array
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on scalarfields array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void POLYR_InitScalarFields(double ** pointer_out, int * dim1, int * dim2);

 // a virer
 /**
  * @fn void POLYR_UpdatePostdata(void)
  * @brief Update values of outlines_POLYR and scalarfields_POLYR pointers
  *
  * @cond PYDOC
  * usage : POLYR_UpdatePostdata()
  * @endcond
  *
  */
  extern "C" void POLYR_UpdatePostdata(void);

 /**
  * @fn void POLYR_GetNbPointOutlines(int ** pointer_out, int * length)
  *
  * @brief Get the list of cumulated outline points number
  *
  * @cond PYDOC
  * python usage : nb_pointOutlines = POLYR_GetNbPointOutlines()
  * @return nb_pointOutlines (integer array) : the cumulated number of outline points of the POLYR
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (int **) : reference on nb_point_outlines array
  * @param[in,out] length (int *)       : first dimension of pointer_out
  * @endcond
  */
  extern "C" void POLYR_GetNbPointOutlines(int** pointer_out, int* length);
  
 /**
  * @fn int POLYR_GetNbScalarFields(void)
  *
  * @brief Get the number of scalar fields of a POLYR
  *
  * @cond PYDOC
  * python usage : nb_scalarfields = POLYR_GetNbScalarFields()
  * @return nb_scalarfields (integer) : the number of scalar fields of a POLYR
  * @endcond
  *
  * @cond CDOC
  * @return nb_scalarfields (int) : the number of scalar fields of a POLYR
  * @endcond
  */
  extern "C" int POLYR_GetNbScalarFields(void);
  
 /**
  * @fn void POLYR_GetPtrAllConnectivities(int ** i4_vector, int * i4_size)
  * @brief Get a reference on the connectivities of all POLYR
  *
  * @cond PYDOC
  * usage : connec = POLYR_GetPtrAllConnectivities()
  * @return connec (integer array) : a reference on all_connectivities
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (int **) : reference on all_connectivities array
  * @param[in,out] length (int *)       : length of pointer_out
  * @endcond
  *
  */
  extern "C" void POLYR_GetPtrAllConnectivities(int ** pointer_out, int * length);

 /**
  * @fn void POLYR_GetPtrConnectivity(int itacty, int** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the connectivity of one POLYR
  *
  * @cond PYDOC
  * usage : connec = POLYR_GetPtrConnectivity(itacty)
  * @param itacty (integer) : POLYR number
  * @return connec (integer 2D-array) : reference on connectivity
  * @endcond
  *
  * @cond CDOC
  * @param[in] itacty (int) : id of POLYR
  * @param[in,out] matrix_out (int **) : reference on connectivity array
  * @param[in,out] dim1 (int *)        : dim1 of matrix_out
  * @param[in,out] dim2 (int *)        : dim2 of matrix_out
  * @endcond
  *
  */
  extern "C" void POLYR_GetPtrConnectivity(int itacty, int ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void POLYR_GetPtrVertexRef(int itacty, double** matrix_out, int* dim1, int* dim2)
  * @brief Get the position of the vertices of a POLYR in its inertia frame
  *
  * @cond PYDOC
  * usage : vertex = POLYR_GetPtrVertexRef(itacty)
  * @param  itacty (integer)         : rank of considered POLYR
  * @return vertex (double 2D-array) : the coordinates of the vertices
  * @endcond
  *
  * @cond CDOC
  * @param[in]     itacty (int)          : rank of considered POLYR
  * @param[in,out] matrix_out (double**) : reference on the array with vertices coordinates
  * @param[out]    dim1 (int*)           : number of dof per vertex
  * @param[out]    dim2 (int*)           : number of vertices
  * @endcond
  */
  extern "C" void POLYR_GetPtrVertexRef(int itacty, double** pointer_out, int* dim1, int* dim2);

 /**
  * @fn void POLYR_GetTopoData(int** matrix_out, int* dim1, int* dim2)
  * @brief Get for each face of all POLYR : contactor id, topo id, face id and face status
  *
  * @cond PYDOC
  * usage : topo_data = POLYR_GetTopoData()
  * @return topt_data (int 2D-array) : topology data of all faces of all POLYR
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] matrix_out (int**) : topology data of all faces of all POLYR
  * @param[out]    dim1 (int*)        : 4
  * @param[out]    dim2 (int*)        : number of faces
  * @endcond
  */
  extern "C" void POLYR_GetTopoData(int** matrix_out, int* dim1, int* dim2);

 /**
   * @fn void POLYR_CleanMemory(void)
   * @brief Free all memory allocated within POLYR module
   *
   * @cond PYDOC
   * python usage : POLYR_CleanMemory()
   * @endcond
   */
   extern "C" void POLYR_CleanMemory(void);

#endif /* wrap_POLYR */
