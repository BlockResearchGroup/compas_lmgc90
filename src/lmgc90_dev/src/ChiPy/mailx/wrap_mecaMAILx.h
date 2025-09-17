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

#ifndef wrap_mecaMAILx_h
#define wrap_mecaMAILx_h

 /**
  * @fn void mecaMAILx_WithoutRenumbering(void)
  * @brief skip renumbering of the unknowns using a rcc method 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_WithoutRenumbering()
  * @endcond
  */
  extern "C" int mecaMAILx_WithoutRenumbering(void);

 /**
  * @fn void mecaMAILx_BandStorage(void)
  * @brief use band matrix 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_BandStorage()
  * @endcond
  */
  extern "C" int mecaMAILx_BandStorage(void);

 /**
  * @fn void mecaMAILx_SparseStorage(void)
  * @brief use sparse matrix
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SparseStorage()
  * @endcond
  */
  extern "C" int mecaMAILx_SparseStorage(void);

 /**
  * @fn void mecaMAILx_ExplodedStorage(void)
  * @brief use element by element matrix 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ExplodedStorage()
  * @endcond
  */
  extern "C" int mecaMAILx_ExplodedStorage(void);

 /**
  * @fn void mecaMAILx_DiagonalStorage(void)
  * @brief use diagonal matrix
  *
  * @cond PYDOC
  * python usage : mecaMAILx_DiagonalStorage()
  * @endcond
  */
  extern "C" int mecaMAILx_DiagonalStorage(void);

 /**
  * @fn void mecaMAILx_SkylineStorage(void)
  * @brief use skyline matrix
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SkylineStorage()
  * @endcond
  */
  extern "C" int mecaMAILx_SkylineStorage(void);

 /**
  * @fn void mecaMAILx_FullStorage(void)
  * @brief use full matrix
  *
  * @cond PYDOC
  * python usage : mecaMAILx_FullStorage()
  * @endcond
  */
  extern "C" int mecaMAILx_FullStorage(void);

 /**
  * @fn void mecaMAILx_SymmetricShape(void)
  * @brief assume matrix is symmetrical
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SymmetricShape()
  * @endcond
  */
  extern "C" int mecaMAILx_SymmetricShape(void);

 /**
  * @fn void mecaMAILx_UnspecifiedShape(void)
  * @brief does not assume any thing on matrix shape
  *
  * @cond PYDOC
  * python usage : mecaMAILx_UnspecifiedShape()
  * @endcond
  */
  extern "C" int mecaMAILx_UnspecifiedShape(void);

 /**
  * @fn int mecaMAILx_GetNbMecaMAILx(void)
  * @brief Get the number of mecaMAILx
  *
  * @cond PYDOC
  * python usage : nb_mecaMAILx = mecaMAILx_GetNbMecaMAILx()
  *
  * @return nb_mecaMAILx (integer) : number of mecaMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of mecaMAILx
  * @endcond
  */
  extern "C" int mecaMAILx_GetNbMecaMAILx(void);

 /**
  * @fn int mecaMAILx_GetNbNodes(int ivalue)
  * @brief Get the number of nodes of a mecaMAILx
  *
  * @cond PYDOC
  * python usage : nb_nodes = mecaMAILx_GetNbNodes(ibdyty)
  * @param[in] ivalue (integer) : id of the mecaMAILx
  * @return nb_nodes (integer) : number of nodes of a mecaMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of nodes
  * @endcond
  */
  extern "C" int mecaMAILx_GetNbNodes(int ivalue);

 /**
  * @fn int mecaMAILx_GetNbElements(int ivalue)
  * @brief Get the number of elements of a mecaMAILx
  *
  * @cond PYDOC
  * python usage : nb_elements = mecaMAILx_GetNbElements(ibdyty)
  * @param[in] ivalue (integer) : id of the mecaMAILx
  * @return nb_nodes (integer) : number of elements of a mecaMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of elements
  * @endcond
  */
  extern "C" int mecaMAILx_GetNbElements(int ivalue);

 /**
  * @fn int mecaMAILx_GetNbGp(int ibdyty, int iblmty)
  * @brief Get the number of Gauss points of an element of a mecaMAILx
  *
  * @cond PYDOC
  * python usage : nb_gp = mecaMAILx_GetNbElements(ibdyty, iblmty)
  * @param[in] ibdyty (integer) : id of the mecaMAILx
  * @param[in] iblmty (integer) : id of the element
  * @return nb_gp (integer) : number of Gauss point of an element of a mecaMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of Gauss points
  * @endcond
  */
  extern "C" int mecaMAILx_GetNbGp(int ibdyty, int iblmty);

 /**
  * @fn void mecaMAILx_SetPreconBody(int ivalue)
  * @brief ask for precomputation of the W matrix on support node dofs of contactors for one body. Assumes bulk behaviour is linear. 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetPreconBody(ivalue)
  * @param[in] ivalue (integer) : id of body to set precon
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue (int) : id of body to set precon
  * @endcond
  */
  extern "C" void mecaMAILx_SetPreconBody(int ivalue);

 /**
  * @fn void mecaMAILx_SetPreconAllBodies(void)
  * @brief ask for precomputation of the W matrix on support node dofs of contactors for all bodies. Assumes bulk behaviour is linear. 
  *    
  * @cond PYDOC
  * python usage : mecaMAILx_SetPreconAllBodies()
  * @endcond
  */
  extern "C" void mecaMAILx_SetPreconAllBodies(void);

 /**
  * @fn void mecaMAILx_ComputePreconW(void)
  * @brief compute the precon W on precon bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputePreconW()
  * @endcond
  */
  extern "C" void mecaMAILx_ComputePreconW(void);

 /**
  * @fn void mecaMAILx_InitPreconW(void)
  * @brief initialize an empty precon W
  *
  * @cond PYDOC
  * python usage : mecaMAILx_InitPreconW()
  * @endcond
  */
  extern "C" void mecaMAILx_InitPreconW(void);

 /**
  * @fn void mecaMAILx_PutPreconW(int ivalue1, int ivalue2, int ivalue3, double * rvector_in, int rlength_in)
  * @brief push a column of precon W
  *
  * @cond PYDOC
  * python usage : mecaMAILx_PutPreconW(ivalue1, ivalue2, ivalue3, vect)
  * @param[in] ivalue1 (integer) : body number
  * @param[in] ivalue2 (integer) : node number 
  * @param[in] ivalue3 (integer) : dof number
  * @param[in] vect (double)     : column 
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)              : body number
  * @param[in] ivalue2 (int)              : node number 
  * @param[in] ivalue3 (int)              : dof number
  * @param[in] vector_in (double[length]) : column 
  * @param[in] length (int)               : length of the column
  * @endcond
  */
  extern "C" void mecaMAILx_PutPreconW(int ivalue1, int ivalue2, int ivalue3, double * rvector_in, int rlength_in);

 /**
  * @fn void mecaMAILx_GetNodesPrecon(int ivalue1, int** i4_vector, int* i4_size)
  * @brief Get the list of preconditionned nodes of a mecaMAILx body
  *
  * Here memory is allocated within lmgc90 so that the pointer can be freely
  * modified by third parties without nasty effect on lmgc90 functioning.
  *
  * @cond PYDOC
  * python usage : precon_list = mecaMAILx_GetNodesPrecon(ibdyty)
  * @param[in] ibdyty   (integer)      : index of the desired mecaMAILx
  * @return precon_list (integer list) : list of the preconditionned nodes
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : index of the desired mecaMAILx
  * @param[out] i4_vector (int**) : reference on the integer array holding the list of precon nodes
  * @param[out] i4_size (int*) : reference on the size of the array referenced by i4_vector
  * @endcond
  */
  extern "C" void mecaMAILx_GetNodesPrecon(int ivalue1, int** i4_vector, int* i4_size);

 /**
  * @fn void mecaMAILx_SetCoroAllBodies(void)
  * @brief ask for corotationnal computation of the W matrix. Assumes bulk behaviour is linear. 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetCoroAllBodies()
  * @endcond
  */
  extern "C" void mecaMAILx_SetCoroAllBodies(void);

 /**
  * @fn void mecaMAILx_SetCoroBody(int ivalue)
  * @brief ask for corotationnal computation of the W matrix of a given body. Assumes bulk behaviour is linear. 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetCoroBody(ivalue)
  * @param[in] ivalue (integer) : id of body to set coro
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue (int) : id of body to set coro
  * @endcond
  */
  extern "C" void mecaMAILx_SetCoroBody(int ivalue);

 /**
  * @fn void mecaMAILx_SetTolCoro(double tol)
  * @brief set the admssible tolerance on rigid body velocity computed by deformable model
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetTolCoro(tol)
  * @param[in] tol (double) : tolerance  
  * @endcond
  *
  * @cond CDOC
  * @param[in] tol (double) : tolerance
  * @endcond
  */
  extern "C" void mecaMAILx_SetTolCoro(double tol);

/*
Managing rigid models
*/

 /**
  * @fn void mecaMAILx_SetRigidAllBodies(void)
  * @brief ask for rigid  computation of the W matrix. Assumes bulk behaviour is linear. 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetRigidAllBodies()
  * @endcond
  */
  extern "C" void mecaMAILx_SetRigidAllBodies(void);

 /**
  * @fn void mecaMAILx_SetRigidBody(int ivalue)
  * @brief ask for rigid  computation of the W matrix of a given body. Assumes bulk behaviour is linear. 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetRigidBody(ivalue)
  * @param[in] ivalue (integer) : id of body to compute as a rigid
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue (int) : id of body to compute as a rigid
  * @endcond
  */
  extern "C" void mecaMAILx_SetRigidBody(int ivalue);

 /**
  * @fn void mecaMAILx_SkipDeformableComputationAllBodies(void)
  * @brief avoid deformable part computation of a deformable body declared as rigid
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SkipDeformableComputationAllBodies()
  * @endcond
  */
  extern "C" void mecaMAILx_SkipDeformableComputationAllBodies(void);

 /**
  * @fn void mecaMAILx_SkipDeformableComputationBody(int ivalue)
  * @brief avoid deformable part computation of a given deformable body declared as rigid
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SkipDeformableComputationBody(ivalue)
  * @param[in] ivalue (integer) : id of body to compute without deformation
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue (int) : id of body to compute without deformation
  * @endcond
  * @endcond
  */
  extern "C" void mecaMAILx_SkipDeformableComputationBody(int ivalue);

 /**
  * @fn void mecaMAILx_BuildRigidBodies(void)
  * @brief computes internal matrices for rigid description
  *
  * @cond PYDOC
  * python usage : mecaMAILx_BuildRigidBodies()
  * @endcond
  */
  extern "C" void mecaMAILx_BuildRigidBodies(void);

 /**
  * @fn int mecaMAILx_IsRigid(int idbdy)
  * @brief return 1 if a given body is rigid/coro, 0 otherwize
  *
  * @cond PYDOC
  * python usage : rigid = mecaMAILx_IsRigid(ibdyty)
  * @param[in] idbdy(integer) : id of the body we want visibility
  * @return   rigid (integer) : 1 if body is visible, 0 otherwize
  * @endcond
  *
  * @cond CDOC
  * @param[in] idbdy (int) : id of the body we want visibility
  * @return (int) 1 if body is rigid, 0 otherwize
  * @endcond
  */
  extern "C" int mecaMAILx_IsRigid(int idbdy);

 /**
  * @fn int mecaMAILx_GetRigidFrame(char * cvalue1_c, int idbyty, double** matrix_out, int* dim1, int* dim2)
  * @brief return an inertia frame matrix
  *
  * Possible values for datatype field are "RFbeg", "RF___", "RFTT_
  * (stands for Rigid Frame)
  *
  * @cond PYDOC
  * python usage : mat = mecaMAILx_GetRigidFrame(datatype, ibdyty)
  * @param[in] idbdy(integer)  : id of the body
  * @return vec (float matrix) : frame matrix (beg, current or TT)
  * @endcond
  *
  * @cond CDOC
  * @param[in] cvalue1 (char[5])      : the frame to get
  * @param[in] idbdy (int)            : id of the body
  * @param[out] matrix_out (double**) : frame to get
  * @param[out] dim1 (int*)           : first dimension of the frame
  * @param[out] dim2 (int*)           : second dimension of the frame
  * @endcond
  */
  extern "C" void mecaMAILx_GetRigidFrame(char * cvalue1_c, int ibdyty, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void mecaMAILx_GetRigidCoorTT(int idbdy, double ** r8_vector, int * r8_size)
  * @brief return TT center of inertia coordinates
  *
  * @cond PYDOC
  * python usage : vec = mecaMAILx_GetRigidCoorTT(ibdyty)
  * @param[in] idbdy(integer)  : id of the body
  * @return vec (float vector) : TT center of inertia coordinates
  * @endcond
  *
  * @cond CDOC
  * @param[in] idbdy (int)           : id of the body
  * @param[out] r8_vector (double**) : TT center of inertia coordinates
  * @param[out] r8_size (int*)       : r8_vector
  * @endcond
  */
  extern "C" void mecaMAILx_GetRigidCoorTT(int ibdyty, double ** r8_vector, int * r8_size);

 /**
  * @fn int mecaMAILx_GetRigidCooref(int idbdy, double ** r8_vector, int * r8_size)
  * @brief return  ref center of inertia coordinates
  *
  * @cond PYDOC
  * python usage : vec = mecaMAILx_GetRigidCooref(ibdyty)
  * @param[in] idbdy(integer)  : id of the body
  * @return vec (float vector) : ref center of inertia coordinates
  * @endcond
  *
  * @cond CDOC
  * @param[in] idbdy (int)           : id of the body
  * @param[out] r8_vector (double**) : ref center of inertia coordinates
  * @param[out] r8_size (int*)       : size of r8_vector
  * @endcond
  */
  extern "C" void mecaMAILx_GetRigidCooref(int ibdyty, double ** r8_vector, int * r8_size);

 /**
  * @fn void mecaMAILx_SetRVDrivenDofs(int IdBody, int* vector, int length)
  * @brief declares rigid velocity dof as driven 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetRVDrivenDofs(idbody,vector_in)
  * @param[in] idbody (integer) : id of the body
  * @param[in] vector (integer) : list of driven dofs
  * @endcond
  *
  * @cond CDOC
  * @param[in] idbody (int) : id of the body
  * @param[in] vector (int) : list of driven dofs
  * @param[in] length (int) : size of the list
  * @endcond
  */
  extern "C" void  mecaMAILx_SetRVDrivenDofs(int IdBody, int * ivector_in, int ilength_in);

 /**
  * @fn void mecaMAILx_SetRVDrivenDofValue(int IdBody, int IdDof, double rv)
  * @brief set the value of rigid velocity dof value 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetRVDrivenDofValue(idbody,iddof,rv)
  * @param[in] idbody (integer) : id of the body
  * @param[in] iddof  (integer) : id of dof
  * @param[in] rv       (float) : value
  * @endcond
  *
  * @cond CDOC
  * @param[in] idbody (int) : id of the body
  * @param[in] iddof  (int) : id of dof
  * @param[in] rv  (double) : value
  * @endcond
  */
  extern "C" void  mecaMAILx_SetRVDrivenDofValue(int IdBody, int IdDof, double rv);
 /**
  * @fn void mecaMAILx_PutBodyRVector(char * cvalue1, int ivalue1, double * rvector_in, int rlength_in)
  * @brief Set a vector of a coro or rigid mecaMAILx body
  *
  * uses copy, and in case fo Fext, the operation is not
  * just setting but adding\n 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_PutBodyRVector(datatype, ibdyty, vector)
  * @param[in] datatype (string [5]) : the vector to set
  * @param[in] ibdyty (integer)      : rank of the RBDY3
  * @param[in] vector (double array) : the new value of the vector
  * @endcond
  *
  * @cond CDOC
  * @param[in] cvalue1 (char[5])    : the vector to set
  * @param[in] ivalue1 (int)        : rank of the RBDY3
  * @param[in] vector_in (double *) : the new vector
  * @param[in] length (int)         : the length of vector_in
  * @endcond
  *
  * Possible values for datatype field are: 
  * - "Coor0": reference coordinates
  * - "Xbeg_": cumulated displacements over time at beginning of time step
  * - "X____": cumulated displacements over time in computed configuration
  * - "Vbeg_": velocity at beginning of time step
  * - "V____": velocity in computed configuration
  * - "Vfree": velocity free of contacts
  * - "Reac_": contact reaction force
  * - "Raux_": working array for reaction force
  * - "Ireac": contact impulse
  * - "Iaux_": working array for impulste
  * - "Fext_": external forces
  *
  */
  extern "C" void mecaMAILx_PutBodyRVector(char * cvalue1, int ivalue1, double * rvector_in, int rlength_in);

 /**
  * @fn void mecaMAILx_GetBodyRVector(char * cvalue1, int ivalue1, double** r8_vector, int* r8_size)
  * @brief Get a copy of a vector of a mecaMAILx body
  *
  * @cond PYDOC
  * python usage : vector = mecaMAILx_GetBodyRVector(datatype, ibdyty)
  * @param[in] datatype (string [5]) : the vector to get
  * @param[in] ibdyty (integer)      : rank of the RBDY3
  * @return    vector (double array) : output vector
  * @endcond
  *
  * @cond CDOC
  * @param[in]  cvalue1 (char[5])    : the vector to get
  * @param[in]  ivalue1 (int)        : rank of the RBDY3
  * @param[out] r8_vector (double**) : the out vector
  * @param[out] r8_size (int*)       : the length of r8_vector
  * @endcond
  *
  * Possible values for datatype field are: 
  * - "Coor0": reference coordinates
  * - "Coorb": coordinates at beginning of time step
  * - "Xbeg_": cumulated displacements over time at beginning of time step
  * - "XTT__": cumulated displacements over time in detection configuration
  * - "X____": cumulated displacements over time in computed configuration
  * - "V____": velocity in computed configuration
  * - "Vbeg_": velocity at beginning of time step
  * - "Vfree": velocity free of contacts
  * - "Reac_": contact reaction force
  * - "Raux_": working array for reaction force
  * - "Ireac": contact impulse
  * - "Iaux_": working array for impulste
  * - "Fext_": external forces
  * - "Fint_": internal forces
  *
  */
  extern "C" void mecaMAILx_GetBodyRVector(char * cvalue1, int ivalue1, double** r8_vector, int* r8_size);


 /**
  * @fn void mecaMAILx_PutBodyVector(char * cvalue1_c, int ivalue1, double * matrix_in, int dim1, int dim2)
  * @brief Set a vector of a given body
  *
  * @cond PYDOC
  * python usage : mecaMAILx_PutBodyVector(datatype, ibdyty, matrix)
  * @param[in] datatype (string of size 5) : the vector to set
  * @param[in] ibdyty (integer)            : rank of body
  * @param[in] matrix (double array)       : the new value
  * @endcond
  *
  * @cond CDOC
  * @param[in] cvalue1 (char[5])    : the vector to set
  * @param[in] ivalue1 (int)        : id of considered body
  * @param[in] matrix_in (double *) : the new values
  * @param[in] dim1 (int)           : first dimension of matrix_in (in C sense)
  * @param[in] dim2 (int)           : second dimension of matrix_in (in C sense)
  * @endcond
  *
  * Possible values for datatype field are:
  * - "Coor0": reference coordinates
  * - "Coor_": coordinates in computed configuration
  * - "Coorb": coordinates at beginning of time step
  * - "X____": cumulated displacements over time in computed configuration
  * - "Xbeg_": cumulated displacements over time at beginning of time step
  * - "V____": velocity in computed configuration
  * - "Vbeg_": velocity at beginning of time step
  * - "Vfree": velocity free of contacts
  * - "Reac_": contact reaction force
  * - "Raux_": working array for reaction force
  * - "Ireac": contact impulse
  * - "Iaux_": working array for impulste
  * - "Fext_": external forces
  * - "Fint_": internal forces
  *
  */
  extern "C" void mecaMAILx_PutBodyVector(char * cvalue1_c, int ivalue1, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void mecaMAILx_GetBodyVector(char * cvalue1_c, int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of a vector of a given body
  *
  * @cond PYDOC
  * Python usage : vector = mecaMAILx_GetBodyVector(datatype, ibdyty)
  * @param datatype (string of size 5) : the vector to get
  * @param ibdyty (integer)            : rank of considered body
  * @return vector (double 2D-array)   : the desired data
  * @endcond
  *
  * @cond CDOC
  * @param[in] cvalue1 (char[5])      : the vector to get
  * @param[in] ivalue1 (int)          : id of considered body
  * @param[out] matrix_out (double**) : the vector to get
  * @param[out] dim1 (int*)           : first dimension of matrix_out
  * @param[out] dim2 (int*)           : second dimension of matrix_out
  * @endcond
  *
  * Possible values for datatype field are:
  * - "Coor0": reference coordinates
  * - "Coor_": coordinates in computed configuration
  * - "Coorb": coordinates at beginning of time step
  * - "X____": cumulated displacements over time in computed configuration
  * - "Xbeg_": cumulated displacements over time at beginning of time step
  * - "V____": velocity in computed configuration
  * - "Vbeg_": velocity at beginning of time step
  * - "Vaux_": working array for velocity
  * - "Vfree": velocity free of contacts
  * - "Reac_": contact reaction force
  * - "Raux_": working array for reaction force
  * - "Ireac": contact impulse
  * - "Iaux_": working array for impulste
  * - "Fext_": external forces
  * - "Fint_": internal forces
  *
  */
  extern "C" void mecaMAILx_GetBodyVector(char * cvalue1_c, int ivalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void mecaMAILx_GetMaterials(int ivalue1, int** i4_vector, int* i4_size)
  * @brief Get a copy of a the elements' material vector of a given body
  *
  * @cond PYDOC
  * Python usage : materials = mecaMAILx_GetMaterials(ibdyty)
  * @param ibdyty (integer)            : rank of considered body
  * @return vector (double 1D-array)   : the material index of elements
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)      : id of considered body
  * @param[out] i4_vector (int**) : the material vector
  * @param[out] i4_size (int*)    : the size of i4_vector
  * @endcond
  */
  extern "C" void mecaMAILx_GetMaterials(int ivalue1, int** i4_vector, int* i4_size);

 /**
  * @fn void mecaMAILx_GetStress(int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of the smoothed nodal stress (Cauchy) of a given body: 2D Sxx,Syy,Sxy,Szz,Svm | 3D Sxx,Sxy,Syy,Sxz,Syz,Szz,Svm
  *
  * @cond PYDOC
  * Python usage : stress = mecaMAILx_GetStress(ibdyty)
  * @param ibdyty (integer)          : rank of considered body
  * @return stress (double 2D-array) : nodal stress of the desired body 
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)          : id of considered body
  * @param[out] matrix_out (double**) : the stress
  * @param[out] dim1 (int*)           : first dimension of matrix out
  * @param[out] dim2 (int*)           : second dimension of matrix_out
  * @endcond
  */
  extern "C" void mecaMAILx_GetStress(int ivalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void mecaMAILx_GetStrain(int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of the smoothed nodal strain (Almansi) of a given body: 2D Exx,Eyy,Exy,Ezz,J | 3D Exx,Exy,Eyy,Exz,Eyz,Ezz,J
  *
  * @cond PYDOC
  * Python usage : strain = mecaMAILx_GetStrain(ibdyty)
  * @param ibdyty (integer)          : rank of considered body
  * @return strain (double 2D-array) : nodal strain of the desired body
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)          : id of considered body
  * @param[out] matrix_out (double**) : the strain to get
  * @param[out] dim1 (int*)           : first dimension of matrix_out
  * @param[out] dim2 (int*)           : second dimension of matrix_out
  * @endcond
  */
  extern "C" void mecaMAILx_GetStrain(int ivalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void mecaMAILx_GetInternalVariables(int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of the smoothed nodal internal variables (2D:10 ; 3D:57)
  *
  * @cond PYDOC
  * Python usage : strain = mecaMAILx_GetInternalVariables(ibdyty)
  * @param ibdyty (integer)          : rank of considered body
  * @return strain (double 2D-array) : nodal internal variables of the desired body
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)          : id of considered body
  * @param[out] matrix_out (double**) : the internal variables to get
  * @param[out] dim1 (int*)           : first dimension of matrix_out
  * @param[out] dim2 (int*)           : second dimension of matrix_out
  * @endcond
  */
  extern "C" void mecaMAILx_GetInternalVariables(int ivalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void mecaMAILx_GetElementStress(int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of the mean stress (Cauchy) of a given body: 2D Sxx,Syy,Sxy,Szz,Svm | 3D Sxx,Sxy,Syy,Sxz,Syz,Szz,Svm
  *
  * @cond PYDOC
  * Python usage : stress = mecaMAILx_GetElementStress(ibdyty)
  * @param ibdyty (integer)          : rank of considered body
  * @return stress (double 2D-array) : nodal stress of the desired body 
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)          : id of considered body
  * @param[out] matrix_out (double**) : the stress
  * @param[out] dim1 (int*)           : first dimension of matrix out
  * @param[out] dim2 (int*)           : second dimension of matrix_out
  * @endcond
  */
  extern "C" void mecaMAILx_GetElementStress(int ivalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void mecaMAILx_PushProperties(void)
  * @brief gives to model the couple of model,behavior used at gauss point
  *
  * @cond PYDOC
  * python usage : mecaMAILx_PushProperties()
  * @endcond
  */
  extern "C" void mecaMAILx_PushProperties(void);

 /**
  * @fn void mecaMAILx_UseNewPPSet(void)
  * @brief each gauss point will have its own property set (necessary in multi physics) 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_UseNewPPSet()
  * @endcond
  */
  extern "C" int mecaMAILx_UseNewPPSet(void);

 /**
  * @fn void mecaMAILx_ComputeFreeVelocity(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes free velocity of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputeFreeVelocity(i_list)
  * @param i_list (list of integer) : list of bodies to compute free velocity
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_ComputeFreeVelocity(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_AssembKT(int * ivector_in=nullptr, int ilength_in=0)
  * @brief assemble pseudo mass matrix and apply drvdof of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_AssembKT(i_list)
  * @param i_list (list of integer) : list of bodies to assemble pseudo mass matrix and apply drvdof
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_AssembKT(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_OnlyAssembKT(int * ivector_in=nullptr, int ilength_in=0)
  * @brief assemble pseudo mass matrix of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_OnlyAssembKT(i_list)
  * @param i_list (list of integer) : list of bodies to assemble pseudo mass matrix
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_OnlyAssembKT(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_ApplyDrvDofKT(int * ivector_in=nullptr, int ilength_in=0)
  * @brief apply drvdof pseudo mass matrix
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ApplyDrvDofKT(i_list)
  * @param i_list (list of integer) : list of bodies to apply drvdof on pseudo mass matrix
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_ApplyDrvDofKT(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_AssembRHS(int * ivector_in=nullptr, int ilength_in=0)
  * @brief assembles right hand side of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_AssembRHS(i_list)
  * @param i_list (list of integer) : list of bodies to assemble right hand side
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_AssembRHS(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn double mecaMAILx_ComputeResidueNorm(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes the norm of the residue of a list of bodies
  *
  * @cond PYDOC
  * python usage : norm = mecaMAILx_ComputeResidueNorm(i_list)
  * @param i_list (list of integer) : list of bodies to compute the norm of the residue
  *        if omitted works on all objects
  * @return norm (double) : Residue Norm
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @return norm (double)       : Residue Norm
  * @endcond
  */
  extern "C" double mecaMAILx_ComputeResidueNorm(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_ComputeBulk(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes elementary stiffness and viscosity matrices and internal forces of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputeBulk(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute stiffness and viscosity matrices and internal forces
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_ComputeBulk(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_ComputeField(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes elementary fields  of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputeField(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute elementary fields
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_ComputeField(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_ComputeFint(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes elementary internal forces of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputeFint(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute internal forces
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_ComputeFint(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_UpdateBulk(int * ivector_in=nullptr, int ilength_in=0)
  * @brief update begin elementary fields with current elementary fields of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_UpdateBulk(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute elementary fields
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_UpdateBulk(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_UpdateDof(int * ivector_in=nullptr, int ilength_in=0)
  * @brief update begin d.o.f. with current d.o.f. of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_UpdateDof(i_list)
  * @param[in] i_list (list of integer) : list of bodies to update current d.o.f
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_UpdateDof(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_ComputeDof(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes the current d.o.f knowing all the forces (free + contact) of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputeDof(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute current d.o.f
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_ComputeDof(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_IncrementStep(void)
  * @brief initializes the current d.o.f and some driven d.o.f values 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_IncrementStep()
  * @endcond
  */
  extern "C" void mecaMAILx_IncrementStep(void);

 /**
  * @fn void mecaMAILx_ComputeFext(int * ivector_in=nullptr, int ilength_in=0)
  * @brief compute elementary external forces of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputeFext(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute external forces
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_ComputeFext(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_ComputeMass(int * ivector_in=nullptr, int ilength_in=0)
  * @brief compute elementary mass and inertia of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputeMass(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute mass and inertia
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_ComputeMass(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_FatalDamping(int * ivector_in=nullptr, int ilength_in=0)
  * @brief set to 0 current velocities of a list of bodies
  *
  * This keyword must be between the ComputeDof and UpdateDof ones.
  *
  * @cond PYDOC
  * python usage : mecaMAILx_FatalDamping(i_list)
  * @param[in] i_list (list of integer) : list of bodies to reset current velocity
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_FatalDamping(int * ivector_in=nullptr, int ilength_in=0);
    
 /**
  * @fn int mecaMAILx_CheckEquilibriumState()
  * @brief Check if the bodies riches an equilibrium state (velocities almost equal to 0) 
  *
  * @cond PYDOC
  * python usage : iconv = mecaMAILx_CheckEquilibriumState()
  *
  * @return iconv (boolean) : True if in equilibrium state
  * @endcond
  *
  * @cond CDOC
  * @return (bool) True if in equilibrium state
  * @endcond
  */
  extern "C" int mecaMAILx_CheckEquilibriumState(void);

 /**
  * @fn void mecaMAILx_SetEquilibriumNorm(char * checktype_c, double tol)
  * @brief set the norm for CheckEquilibriumState
  *
  * Type of check test:
  * - Qvlcy : quadratic norm of velocy
  * - Maxm  : maximum   norm of velocy
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetEquilibriumNorm(checktype, tol)
  * @param[in] checktype (char[5]) : type of check test
  * @param[in] tol       (double)  : tolerance
  * @endcond
  *
  * @cond CDOC
  * @param[in] checktype_c (char[5]) : type of check test
  * @param[in] tol (double)          : tolerance
  * @endcond
  */
  extern "C" void mecaMAILx_SetEquilibriumNorm(char * checktype_c, double tol);

 /**
  * @fn void mecaMAILx_ReadDrivenDof(void)
  * @brief Read DRV_DOF.DAT
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ReadDrivenDof()
  * @endcond
  */
  extern "C" void mecaMAILx_ReadDrivenDof(void);
    
 /**
  * @fn void mecaMAILx_ReadIniGPV(int num=0)
  * @brief Read GPV file
  *
  * If num <= 0 : DATBOX/GPV.INI file is read
  *
  * Else : OUTBOX/GPV.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniGPV
  * last call
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ReadIniGPV(num=0)
  * @param[in] num (integer) : which GPV file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which GPV file to read
  * @endcond
  *
  */
  extern "C" void mecaMAILx_ReadIniGPV(int num=0);

 /**
  * @fn void mecaMAILx_ReadIniDof(int num=0)
  * @brief Read DOF file
  *
  * If num <= 0 : DATBOX/DOF.INI file is read
  *
  * Else : OUTBOX/DOF.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniDof
  * last call
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ReadIniDof(num=0)
  * @param[in] num (integer) : which DOF file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which DOF file to read
  * @endcond
  *
  */
  extern "C" void mecaMAILx_ReadIniDof(int num=0);

 /**
  * @fn void mecaMAILx_LoadBehaviours(void)
  * @brief load behaviours from bulk_behav 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_LoadBehaviours()
  * @endcond
  */
  extern "C" void mecaMAILx_LoadBehaviours(void);

 /**
  * @fn void mecaMAILx_LoadModels(void)
  * @brief load models from models
  *
  * @cond PYDOC
  * python usage : mecaMAILx_LoadModels()
  * @endcond
  */
  extern "C" void mecaMAILx_LoadModels(void);

 /**
  * @fn void mecaMAILx_WriteDrivenDof(void)
  * @brief Write DRV_DOF.OUT
  *
  * @cond PYDOC
  * python usage : mecaMAILx_WriteDrivenDof()
  * @endcond
  */
  extern "C" void mecaMAILx_WriteDrivenDof(void);

 /**
  * @fn void mecaMAILx_WriteLastDof(void)
  * @brief Write ascii DOF.LAST file
  *
  * @cond PYDOC
  * python usage : mecaMAILx_WriteLastDof()
  * @endcond
  */
  extern "C" void mecaMAILx_WriteLastDof(void);

 /**
  * @fn void mecaMAILx_WriteOutDof(void)
  * @brief Write ascii DOF.OUT file. Can be activate only each N step
  *
  * @cond PYDOC
  * python usage : mecaMAILx_WriteOutDof()
  * @endcond
  */
  extern "C" void mecaMAILx_WriteOutDof(void);

 /**
  * @fn void mecaMAILx_DisplayOutDof(void)
  * @brief Display body degrees of freedom
  *
  * @cond PYDOC
  * python usage : mecaMAILx_DisplayOutDof()
  * @endcond
  */
  extern "C" void mecaMAILx_DisplayOutDof(void);

 /**
  * @fn void mecaMAILx_DisplayBulkElement(int IdBody,int IdElem)
  * @brief Display fields of a bulk element
  *
  * @cond PYDOC
  * @param[in] IdBody (int)  : id of the concern body
  * @param[in] IdElem (int)  : id of the concern element
  * python usage : mecaMAILx_DisplayBulkElement(IdBody,IdElem)
  * @endcond
  * 
  * @cond CDOC
  * @param[in] IdBody (int)  : id of the concern body
  * @param[in] IdElem (int)  : id of the concern element
  * @endcond
  */
  extern "C" void mecaMAILx_DisplayBulkElement(int IdBody,int IdElem);

 /**
  * @fn void mecaMAILx_WriteLastRnod(int * ivector_in=nullptr, int ilength_in=0)
  * @brief Write ascii Rnod.LAST file of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_WriteLastRnod(i_list)
  * @param i_list (list of integer) : list of bodies to write in Rnod.LAST
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_WriteLastRnod(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_WriteOutRnod(int * ivector_in=nullptr, int ilength_in=0)
  * @brief Write ascii Rnod.OUT file of a list of bodies. Can be activat only each N step
  *
  * @cond PYDOC
  * python usage : mecaMAILx_WriteOutRnod(i_list)
  * @param i_list (list of integer) : list of bodies to write in Rnod.OUT
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_WriteOutRnod(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_DisplayOutRnod(int * ivector_in=nullptr, int ilength_in=0)
  * @brief Display body forces of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_DisplayOutRnod(i_list)
  * @param i_list (list of integer) : list of bodies to display body forces
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_DisplayOutRnod(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_WriteLastNodalForces(int * ivector_in=nullptr, int ilength_in=0)
  * @brief Write ascii Rnod.LAST file of a list of bodies.
  *
  * This function is almost like WriteLastRnod, but write also internal and inertial forces.
  *
  * @cond PYDOC
  * python usage : mecaMAILx_WriteLastNodalForces(i_list)
  * @param i_list (list of integer) : list of bodies to write in Rnod.LAST
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_WriteLastNodalForces(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_WriteOutNodalForces(int * ivector_in=nullptr, int ilength_in=0)
  * @brief Write ascii Rnod.OUT file of a list of bodies. Can be activat only each N step
  *
  * This function is almost like WriteOutRnod, but write also internal and inertial forces.
  *
  * @cond PYDOC
  * python usage : mecaMAILx_WriteOutNodalForces(i_list)
  * @param i_list (list of integer) : list of bodies to write in Rnod.OUT
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_WriteOutNodalForces(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_DisplayOutNodalForces(int * ivector_in=nullptr, int ilength_in=0)
  * @brief Display computed nodal forces of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_DisplayOutNodalForces(i_list)
  * @param i_list (list of integer) : list of bodies to display body forces
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_DisplayOutNodalForces(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn int mecaMAILx_GetScalarFieldRank(int ibdyty, int iblmty, char* field_name);
  * @brief Get the rank of scalar field of an element of a body from its name
  *
  * @cond PYDOC
  * python usage : f_rank = mecaMAILx_GetScalarFieldRank(ibdyty, iblmty, name)
  * @param[in] ibdyty (integer) : id of the concern body
  * @param[in] iblmty (integer) : id of the concern element
  * @param[in] name (string)    : name of the desired field
  * @return f_rank (integer) : rank of the corresponding field
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : id of the concern body
  * @param[in] iblmty (int) : id of the concern element
  * @param[in] name (char[30]) : name of the field
  * @return (int) : rank of the corresponding field
  * @endcond
  */
  extern "C" int mecaMAILx_GetScalarFieldRank(int ibdyty, int blmty, char* name);

 /**
  * @fn void mecaMAILx_SetScalarFieldByNode(int IdBody, int f_rank, double * rvector_in, int rlength_in)
  * @brief Update elementary scalar field through a nodal external field on a given body
  *
  * Use the form functions of the elements and input values to compute and store field values
  * at Gauss points.
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetScalarFieldByNode(IdBody, f_rank, f)
  * @param[in] IdBody (integer) : id of the concern body
  * @param[in] f_rank (integer) : rank of the field to set 
  * @param[in] f (double array) : value of the field
  * @endcond
  *
  * \n You need to declare this field in your MODELS.DAT\n 
  *
  * @cond CDOC
  * @param[in] IdBody (int)               : id of the concern body
  * @param[in] f_rank (int)               : rank of the field
  * @param[in] vector_in (double[length]) : value of the field
  * @param[in] length (int)               : size of the field
  * @endcond
  */
  extern "C" void mecaMAILx_SetScalarFieldByNode(int IdBody, int f_rank, double * rvector_in, int rlength_in);

 /**
  * @fn void mecaMAILx_SetScalarFieldByElement(int IdBody, int f_rank, double * rvector_in, int rlength_in)
  * @brief Update elementary scalar field through a element external field on a given body
  *
  * Field values are stored at Gauss point, on an element all Gauss point have the element value
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetScalarFieldByElement(IdBody, f_rank, f)
  * @param[in] IdBody (integer) : id of the concern body
  * @param[in] f_rank (integer) : rank of the field to set 
  * @param[in] f (double array) : value of the field
  * @endcond
  *
  * \n You need to declare this field in your MODELS.DAT\n 
  *
  * @cond CDOC
  * @param[in] IdBody (int)               : id of the concern body
  * @param[in] f_rank (int)               : rank of the field
  * @param[in] vector_in (double[length]) : value of the field
  * @param[in] length (int)               : size of the field
  * @endcond
  */
  extern "C" void mecaMAILx_SetScalarFieldByElement(int IdBody, int f_rank, double * rvector_in, int rlength_in);

 /**
  * @fn int mecaMAILx_GetVectorFieldRank(int ibdyty, int iblmty, char* field_name);
  * @brief Get the rank of field of an element of a body from its name
  *
  * @cond PYDOC
  * python usage : f_rank = mecaMAILx_GetVectorFieldRank(ibdyty, iblmty, name)
  * @param[in] ibdyty (integer) : id of the concern body
  * @param[in] iblmty (integer) : id of the concern element
  * @param[in] name (string)    : name of the desired vector field
  * @return f_rank (integer) : rank of the corresponding vector field
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : id of the concern body
  * @param[in] iblmty (int) : id of the concern element
  * @param[in] name (char[30]) : name of the vector field
  * @return (int) : rank of the corresponding vector field
  * @endcond
  */
  extern "C" int mecaMAILx_GetVectorFieldRank(int ibdyty, int blmty, char* name);

 /**
  * @fn void mecaMAILx_SetVectorFieldByNode(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2)
  * @brief Update elementary fields through a nodal external field on a given body
  *
  * Use the form functions of the elements and input values to compute and store field values
  * at Gauss points.
  *
  * You need to declare this field in your MODELS.DAT\n 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetVectorFieldByNode(IdBody, f_rank, f)
  * @param[in] IdBody (integer) : id of the concern body
  * @param[in] f_rank (integer) : rank of the vector field to set 
  * @param[in] f (double array) : value of the vector field
  * @endcond
  *
  * @cond CDOC
  * @param[in] IdBody (int)                  : id of the concern body
  * @param[in] f_rank (int)                  : rank of the vector field
  * @param[in] matrix_in (double[dim1,dim2]) : value of the vector field
  * @param[in] dim1   (int)                  : first dimension of the vector field
  * @param[in] dim2   (int)                  : second dimension of the vector field
  * @endcond
  */
  extern "C" void mecaMAILx_SetVectorFieldByNode(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void mecaMAILx_SetVectorFieldByElement(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2)
  * @brief Update elementary fields through a nodal external field on a given body
  *
  * Use the form functions of the elements and input values to compute and store field values
  * at Gauss points.
  *
  * You need to declare this field in your MODELS.DAT\n 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetVectorFieldByElement(IdBody, f_rank, f)
  * @param[in] IdBody (integer) : id of the concern body
  * @param[in] f_rank (integer) : rank of the vector field to set 
  * @param[in] f (double array) : value of the vector field
  * @endcond
  *
  * @cond CDOC
  * @param[in] IdBody (int)                  : id of the concern body
  * @param[in] f_rank (int)                  : rank of the vector field
  * @param[in] matrix_in (double[dim1,dim2]) : value of the vector field
  * @param[in] dim1   (int)                  : first dimension of the vector field
  * @param[in] dim2   (int)                  : second dimension of the vector field
  * @endcond
  */
  extern "C" void mecaMAILx_SetVectorFieldByElement(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void mecaMAILx_Terminate(void)
  * @brief Stop job properly
  *
  * @cond PYDOC
  * python usage : mecaMAILx_Terminate()
  * @endcond
  */
  extern "C" void mecaMAILx_Terminate(void);

 /**
  * @fn void mecaMAILx_ComputeOrthoFrame(int * ivector_in=nullptr, int ilength_in=0)
  * @brief Use user routine to compute the ortho frame of a list of bodies
  *
  * This method uses a routine define by the user in user.f90
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputeOrthoFrame(i_list)
  * @param i_list (list of integer) : list of bodies to compute ortho frame with user routine
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  *
  */
  extern "C" void mecaMAILx_ComputeOrthoFrame(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_ComputeUserField(int ifield, int * ivector_in=nullptr, int ilength_in=0)
  * @brief Use user routine to compute a field at gp
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputeUserField(ifield, i_list)
  * @param ifield (integer) : id of the field to compute
  * @param i_list (list of integer) : list of bodies to compute user fields on
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] ifield (int) : id of the field to compute
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_ComputeUserField(int ifield, int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void void mecaMAILx_SetVisible(int ibdyty)
  * @brief set visible a given mecaMAILx 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetVisible(ibdyty)
  * @param[in] ibdyty(integer) : index of the mecaMAILx
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the mecaMAILx
  * @endcond
  */
  extern "C" void mecaMAILx_SetVisible(int ibdyty);

 /**
  * @fn void void mecaMAILx_SetInvisible(int ibdyty)
  * @brief rended a given mecaMAILx invisible
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetInvisible(ibdyty)
  * @param[in] ibdyty(integer) : index of the mecaMAILx
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the mecaMAILx
  * @endcond
  */
  extern "C" void mecaMAILx_SetInvisible(int ibdyty);

 /**
  * @fn int mecaMAILx_IsVisible(int idbdy)
  * @brief return if a given body visible
  *
  * @cond PYDOC
  * python usage : visible = mecaMAILx_IsVisible(ibdyty)
  * @param[in] idbdy(integer) : id of the body we want visibility
  * @return visible (integer) : 1 if body is visible, 0 else
  * @endcond
  *
  * @cond CDOC
  * @param[in] idbdy (int) : id of the body we want visibility
  * @return (int) 1 if body is visible, 0 else
  * @endcond
  */
  extern "C" int mecaMAILx_IsVisible(int idbdy);

  /**
  * @fn void mecaMAILx_ComputeRayleighDamping(double alpha, double beta, int * ivector_in=nullptr, int ilength_in=0)
  * @brief compute the Rayleigh damping: C=alpha*M+beta*K of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputeRayleighDamping(alpha,beta,i_list)
  * @param alpha (real) : damping value
  * @param beta (real) : damping value
  * @param i_list (list of integer) : list of bodies to compute Rayleigh damping
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] alpha (double)   : factor on mass
  * @param[in] beta  (double)   : factor on stiffness
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_ComputeRayleighDamping(double alpha, double beta, int * ivector_in=nullptr, int ilength_in=0);

  /**
  * @fn void mecaMAILx_ComputeRayleighDampingDiscreteElement(double damp, int * ivector_in=nullptr, int ilength_in=0)
  * @brief set damping for discrete FE element of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputeRayleighDampingDiscreteElement(damp, i_list)
  * @param ref_size (real) : damping value
  * @param i_list (list of integer) : list of bodies to compute damping for discrete FE element
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] damp (double)    : damping value
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_ComputeRayleighDampingDiscreteElement(double damp, int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn int mecaMAILx_GetNodeCoorTT(int idbdy,int nodty, double ** r8_vector, int * r8_size)
  * @brief return TT node coordinates
  *
  * @cond PYDOC
  * python usage : vec = mecaMAILx_GetNodeCoorTT(ibdyty,inodty)
  * @param[in] idbdy(integer)  : id of the body
  * @param[in] inodty(integer) : id of the node
  * @return vec (float vector) : TT node coordinates 
  * @endcond
  *
  * @cond CDOC
  * @param[in] idbdy (int)           : id of the body
  * @param[in] inodty(int)           : id of the node
  * @param[out] r8_vector (double**) : TT node coordinates
  * @param[out] r8_size (int* )      : size of r8_vector
  * @endcond
  */
  extern "C" void mecaMAILx_GetNodeCoorTT(int ibdyty,int inodty, double ** r8_vector, int * r8_size);

 /**
  * @fn int mecaMAILx_GetNodeCooref(int idbdy,int nodty, double ** r8_vector, int * r8_size)
  * @brief return ref node coordinates
  *
  * @cond PYDOC
  * python usage : vec = mecaMAILx_GetNodeCoorref(ibdyty,inodty)
  * @param[in] idbdy(integer) : id of the body
  * @param[in] inodty(integer) : id of the node
  * @return vec (float vector) : ref node coordinates 
  * @endcond
  *
  * @cond CDOC
  * @param[in] idbdy (int)           : id of the body
  * @param[in] inodty(int)           : id of the node
  * @param[out] r8_vector (double**) : ref node coordinates
  * @param[out] r8_size (int*)       : size of r8_vector
  * @endcond
  */
  extern "C" void mecaMAILx_GetNodeCooref(int ibdyty,int inodty, double ** r8_vector, int * r8_size);


 /**
  * @fn void mecaMAILx_GetBodyMatrix(char * cvalue1_c, int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of a matrix of a given body
  *
  * Possible values for datatype field are "mass_", "stiff", "damp_"
  *
  * @cond PYDOC
  * Python usage : matrix = mecaMAILx_GetBodyMatrix(datatype, ibdyty)
  * @param datatype (string of size 5) : the matrix to get
  * @param ibdyty (integer)            : rank of considered body
  * @return matrix (double array)      : the desired matrix
  * @endcond
  *
  * @cond CDOC
  * @param[in]  cvalue1 (char[5])     : the matrix to get
  * @param[in]  ivalue1 (int)         : id of considered body
  * @param[out] matrix_out (double**) : the matrix to get
  * @param[out] dim1 (int*)           : the length of matrix_out
  * @param[out] dim2 (int*)           : the length of matrix_out
  * @endcond
  */
  extern "C" void mecaMAILx_GetBodyMatrix(char * cvalue1_c, int ivalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void mecaMAILx_getDrvVlocy(int ibdyty, int** i4_vector, int* i4_size, double** r8_vector, int* r8_size);
  * @brief Get the driven dof of a body
  *
  * @cond PYDOC
  * python usage : [drvdof_indices, drvdof_values] = mecaMAILx_getDrvVlocy(ibdyty)
  * @param[in] ibdyty (integer) : index of the mecaMAILx
  * @param drvdof_indices (integer array) : indices list of driven dof
  * @param drvdof_values  (real array) : values of the driven dof
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the mecaMAILx
  * @param[out] i4_vector (int**) : reference onto the indices list of driven dofs
  * @param[out] i4_size   (int*)  : size of the array referenced by i4_vector
  * @param[out] r8_vector (double**) : reference onto the values of driven dofs
  * @param[out] r8_size   (int*)  : size of the array referenced by r8_vector
  * @endcond
  */
  extern "C" void mecaMAILx_getDrvVlocy(int ibdyty, int** i4_vector, int* i4_size, double** r8_vector, int* r8_size);
  
 /**
  * @fn void mecaMAILx_computeDrvVlocy(int ibdyty, double * rvector_in, int rlength_in);
  * @brief Compute the value of the driven velocity of a body a current time
  *
  * In place replacement in the input array of the new value(s) of the driven velocity
  *
  * @cond PYDOC
  * python usage : mecaMAILx_computeDrvVlocy(ibdyty, values)
  * @param[in] ibdyty (integer) : index of the mecaMAILx
  * @param[in,out] values (double array) : numpy array, input old values of imposed velocity, output new ones
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : index of the mecaMAILx
  * @param[in,out] vector_in (double *) : input old values of imposed velocity, output new ones
  * @param[in] length (int) : size of vector_in array
  * @endcond
  */
  extern "C" void mecaMAILx_computeDrvVlocy(int ibdyty, double * rvector_in, int rlength_in);


 /**
  * @fn void mecaMAILx_SetVlocyDrivenDof(int IdBody, int f_dof, int f_node, double f_value)
  * @brief Apply Drv Dof on a given body
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetVlocyDrivenDof(IdBody, f_dof, f_node, f_value)
  * @param[in] IdBody (integer) : id of the concern body
  * @param[in] f_dof (integer)  : dof of the concern node
  * @param[in] f_node (integer) : node
  * @param[in] f_value (double) : value of the drvdof
  * @endcond
  *
  * @cond CDOC
  * @param[in] IdBody (int)               : id of the concern body
  * @param[in] f_dof  (int)               : dof of the drvdof
  * @param[in] f_node (int)               : node
  * @param[in] f_value (double)           : value
  * @endcond
  */
  extern "C" void mecaMAILx_SetVlocyDrivenDof(int IdBody, int f_dof, int f_node , double f_value);

 /**
  * @fn void mecaMAILx_ComputeContactDetectionConfiguration(int * ivector_in=nullptr, int ilength_in=0)
  * @brief compute the contact detection configuration of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputeContactDetectionConfiguration(i_list)
  * @param i_list (list of integer) : list of bodies to compute contact detection configuration
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
 */
 extern "C" void mecaMAILx_ComputeContactDetectionConfiguration(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_NullifyReac(char * cvalue1_c, int IdBody)
  * @brief set to 0 the reac of the IdBody mecaMAILx 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_NullifyReac(datatype, IdBody)
  * @param[in] datatype (string of size 5) : the vector to set
  * @param[in] IdBody (integer) : id of the concerned body
  * @endcond
 */
extern "C" void mecaMAILx_NullifyReac(char * cvalue1_c, int IdBody);


 /**
  * @fn void mecaMAILx_GetAll(int idBody,  double ** matrix_out, int * dim1, int * dim2)
  * @brief return mechanical data computed for idBody
  *
  * @cond PYDOC
  * python usage : array = mecaMAILx_GetAll(idBody)
  * @param[in] IdBody (integer)     : id of the concerned body
  * @return array (double 2D-array) : mechanical data
  * @endcond
  *
  * @cond CDOC
  * @param[in]     IdBody (int)           : id of the concerned body
  * @param[in,out] matrix_out (double **) : xxx
  * @param[in]     dim1 (int *)  i        : matrix_out first dimension
  * @param[in]     dim2 (int *)           : matrix_out second dimension
  * @endcond
  */
  extern "C" void mecaMAILx_GetAll(int idBody, double ** matrix_out, int * dim1, int * dim2);

 /**
  * @fn void mecaMAILx_GetCooref(int idBody,  double ** matrix_out, int * dim1, int * dim2)
  * @brief return node coordinates of idBody
  *
  * @cond PYDOC
  * python usage : array = mecaMAILx_GetCooref(idBody)
  * @param[in] IdBody (integer)     : id of the concerned body
  * @return array (double 2D-array) : coordinates
  * @endcond
  *
  * @cond CDOC
  * @param[in]     IdBody (int)           : id of the concerned body
  * @param[in,out] matrix_out (double **) : xxx
  * @param[in]     dim1 (int *)  i        : matrix_out first dimension
  * @param[in]     dim2 (int *)           : matrix_out second dimension
  * @endcond
  */
  extern "C" void mecaMAILx_GetCooref(int idBody, double ** matrix_out, int * dim1, int * dim2);

 /**
  * @fn void mecaMAILx_GetConnectivity(int idBody,  int ** i4_vector, int * i4_size)
  * @brief return connectivity of idBody elements
  *
  * @cond PYDOC
  * python usage : vector = mecaMAILx_GetConnectivity(idBody)
  * @param[in] IdBody (integer) : id of the concerned body
  * @return    vector (integer)  : connectivity
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)     : id of the concerned body
  * @param[out] i4_vecto (int**) : xxx
  * @param[out] dim1 (int *)  i  : dimension
  * @endcond
  */
  extern "C" void mecaMAILx_GetConnectivity(int idBody, int ** i4_vector, int * i4_size );

 /**
  * @fn void mecaMAILx_GetElementsVolume(int idBody,  double** r8_vector, int* r8_size)
  * @brief return volume of elements
  *
  * @cond PYDOC
  * python usage : volumes = mecaMAILx_GetElementsVolume(idBody)
  * @param[in] IdBody (integer) : id of the concerned body
  * @return volumes[nb_ele] (double)  : volume
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)         : id of the concerned body
  * @param[out] r8_vector (double**) : xxx
  * @param[out] r8_size (int*)       : dimension
  * @endcond
  */
  extern "C" void mecaMAILx_GetElementsVolume(int idBody, double** r8_vector, int* r8_size );

 /**
  * @fn void mecaMAILx_GetGpCoor(int idBody,  double ** matrix_out, int * dim1, int * dim2)
  * @brief return Gauss points coordinates of idBody
  *
  * @cond PYDOC
  * python usage : array = mecaMAILx_GetGpCoor(idBody)
  * @param[in] IdBody (integer)     : id of the concerned body
  * @return array (double 2D-array) : coordinates of all Gauss points
  * @endcond
  *
  * @cond CDOC
  * @param[in]     IdBody (int)           : id of the concerned body
  * @param[in,out] matrix_out (double **) : coordinates of all Gauss points
  * @param[in]     dim1 (int *)  i        : matrix_out first dimension
  * @param[in]     dim2 (int *)           : matrix_out second dimension
  * @endcond
  */
  extern "C" void mecaMAILx_GetGpCoor(int idBody, double ** matrix_out, int * dim1, int * dim2);

 /**
  * @fn void mecaMAILx_GetGpStrain(int idBody,int idEle, int idGp, double** r8_vector, int* r8_size)
  * @brief return strain values stored at a gp
  *
  * @cond PYDOC
  * python usage : strain = mecaMAILx_GetGpStrain(idBody,idEle,idGp)
  * @param[in] IdBody (integer) : id of the concerned body
  * @param[in] IdEle  (integer) : id of the concerned element
  * @param[in] IdGp   (integer) : id of the concerned gauss point
  * @return strain[size] (double)  : value of strain
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)         : id of the concerned body
  * @param[in]  Id     (int)         : id of the concerned element
  * @param[in]  f      (int)         : id of the concerned gauss point 
  * @param[out] r8_vector (double**) : xxx
  * @param[out] r8_size (int*)       : dimension
  * @endcond
  */
  extern "C" void mecaMAILx_GetGpStrain(int idBody, int idEle, int idGp, double** r8_vector, int* r8_size );

 /**
  * @fn void mecaMAILx_GetGpStress(int idBody,int idEle, int idGp, double** r8_vector, int* r8_size)
  * @brief return stress values stored at a gp
  *
  * @cond PYDOC
  * python usage : stress = mecaMAILx_GetGpStress(idBody,idEle,idGp)
  * @param[in] IdBody (integer) : id of the concerned body
  * @param[in] IdEle  (integer) : id of the concerned element
  * @param[in] IdGp   (integer) : id of the concerned gauss point
  * @return stress[size] (double)  : value of stress
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)         : id of the concerned body
  * @param[in]  Id     (int)         : id of the concerned element
  * @param[in]  f      (int)         : id of the concerned gauss point 
  * @param[out] r8_vector (double**) : xxx
  * @param[out] r8_size (int*)       : dimension
  * @endcond
  */
  extern "C" void mecaMAILx_GetGpStress(int idBody, int idEle, int idGp, double** r8_vector, int* r8_size );

 /**
  * @fn void mecaMAILx_GetGpInternals(int idBody,int idEle, int idGp, double** r8_vector, int* r8_size)
  * @brief return internal values stored at a gp
  *
  * @cond PYDOC
  * python usage : internals = mecaMAILx_GetGpInternals(idBody,idEle,idGp)
  * @param[in] IdBody (integer) : id of the concerned body
  * @param[in] IdEle  (integer) : id of the concerned element
  * @param[in] IdGp   (integer) : id of the concerned gauss point
  * @return internals[nb_internals] (double)  : value of internals
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)         : id of the concerned body
  * @param[in]  Id     (int)         : id of the concerned element
  * @param[in]  f      (int)         : id of the concerned gauss point 
  * @param[out] r8_vector (double**) : xxx
  * @param[out] r8_size (int*)       : dimension
  * @endcond
  */
  extern "C" void mecaMAILx_GetGpInternals(int idBody, int idEle, int idGp, double** r8_vector, int* r8_size );


 /**
  * @fn void mecaMAILx_GetGpPrincipalField(int idBody,int idEle, int idGp, int idField, double ** ptr, int * dim1, int * dim2)
  * @brief return principal field (strain or stress) at a gp
  *
  * @cond PYDOC
  * python usage : field = mecaMAILx_GetGpPrincipalField(idBody,idEle,idGp,idField)
  * @param[in] IdBody (integer) : id of the concerned body
  * @param[in] IdEle  (integer) : id of the concerned element
  * @param[in] IdGp   (integer) : id of the concerned gauss point
  * @param[in] IdField(integer) : id of the field (1: strain, 2: stress)
  * @return field  (double array): tensor field with principal values
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody  (int)       : id of the concerned body
  * @param[in]  Id      (int)       : id of the concerned element
  * @param[in]  f       (int)       : id of the concerned gauss point
  * @param[in]  idField (int)       : id of the field (1: strain, 2: stress)
  * @param[out] matrix_out (double) : field
  * @param[out] dim 1 (int)         : 3
  * @param[out] dim 2 (int)         : 4
  * @endcond
  */
  extern "C" void mecaMAILx_GetGpPrincipalField(int idBody, int idEle, int idGp, int idField, double** matrix_out, int* dim1, int* dim2);


 /**
  * @fn void mecaMAILx_GetElementsInternal(int idBody,int id, int f, double** r8_vector, int* r8_size)
  * @brief return a value over elements of an internal stored at gp
  *
  * @cond PYDOC
  * python usage : internals = mecaMAILx_GetElementsInternal(idBody,id,f)
  * @param[in] IdBody (integer) : id of the concerned body
  * @param[in] Id     (integer) : id of the internal
  * @param[in] f     (integer)  : flag 1: mean, 2: sum, 3:max, 4: min 
  * @return internals[nb_ele] (double)  : value of internal
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)         : id of the concerned body
  * @param[in]  Id     (int)         : id of the internal
  * @param[in]  f      (int)         : flag 1: mean, 2: sum, 3:max, 4: min 
  * @param[out] r8_vector (double**) : xxx
  * @param[out] r8_size (int*)       : dimension
  * @endcond
  */
extern "C" void mecaMAILx_GetElementsInternal(int idBody, int id, int f, double** r8_vector, int* r8_size );

/**
  * @fn void mecaMAILx_GetElementsInternalIntegral(int idBody,int id, double** r8_vector, int* r8_size)
  * @brief return integral over elements of an internal stored at gp
  *
  * @cond PYDOC
  * python usage : internals = mecaMAILx_GetElementsInternalIntegral(idBody,id)
  * @param[in] IdBody (integer) : id of the concerned body
  * @param[in] Id     (integer) : id of the internal
  * @return internals[nb_ele] (double)  : value of internal
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)         : id of the concerned body
  * @param[in]  Id     (int)         : id of the internal
  * @param[out] r8_vector (double**) : xxx
  * @param[out] r8_size (int*)       : dimension
  * @endcond
  */
extern "C" void mecaMAILx_GetElementsInternalIntegral(int idBody, int id, double** r8_vector, int* r8_size );

/**
  * @fn void mecaMAILx_GetElementsCenter(int idBody,  double** r8_vector, int* r8_size)
  * @brief return center of elements
  *
  * @cond PYDOC
  * python usage : centers = mecaMAILx_GetElementsCenter(idBody)
  * @param[in] IdBody (integer) : id of the concerned body
  * @return centers[3*nb_ele] (double)  : center
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)         : id of the concerned body
  * @param[out] r8_vector (double**) : xxx
  * @param[out] r8_size (int*)       : dimension
  * @endcond
  */
  extern "C" void mecaMAILx_GetElementsCenter(int idBody, double** r8_vector, int* r8_size );

 /**
  * @fn void mecaMAILx_GetElementsJacobian(int idBody,  double** r8_vector, int* r8_size)
  * @brief return jacobian of elements
  *
  * @cond PYDOC
  * python usage : jacobians = mecaMAILx_GetElementsJacobian(idBody)
  * @param[in] IdBody (integer) : id of the concerned body
  * @return jacobians[nb_ele] (double)  : jacobian
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)         : id of the concerned body
  * @param[out] r8_vector (double**) : xxx
  * @param[out] r8_size (int*)       : dimension
  * @endcond
  */
  extern "C" void mecaMAILx_GetElementsJacobian(int idBody, double** r8_vector, int* r8_size );

 /**
  * @fn void mecaMAILx_ComputeElementsEnergy(void)
  * @brief return energy of elements
  *
  * @cond PYDOC
  * python usage : mecaMAILx_ComputeElementsEnergy()
  * @endcond
  */
extern "C" void mecaMAILx_ComputeElementsEnergy(void);

 /**
  * @fn void mecaMAILx_GetPtrElementsEnergy(int idBody,  double** pointer_out, int* length)
  * @brief return energy of elements
  *
  * @cond PYDOC
  * python usage : energies = mecaMAILx_GetElementsEnergy(idBody)
  * @param[in] IdBody (integer)         : id of the concerned body
  * @return    energies (double array)  : reference on the desired vector seen as a numpy array
  * @endcond
  *
  * @cond CDOC
  * @param[in]     IdBody (int)         : id of the concerned body
  * @param[out] pointer_out (double **) : reference on the vector
  * @param[in]  length (int*)           : reference on the length of the out vector
  * @endcond
  */
extern "C" void mecaMAILx_GetPtrElementsEnergy(int idBody, double** pointer_out, int* length);

 /**
  * @fn void mecaMAILx_GetElementsNeighbor(int idBody, double tol, int** matrix_out, int* dim1, int* dim2)
  * @brief return elements in the tol-neighbor of an element of idBody
  *
  * @cond PYDOC
  * python usage : neighbors = mecaMAILx_GetElementsNeighbor(idBody,tol)
  * @param[in] IdBody (integer)     : id of the concerned body
  * @param[in] tol (double)         : tolerance
  * @return array (double 2D-array) : neighbor[nb_ele,max_neighbors]
  * @endcond
  *
  * @cond CDOC
  * @param[in]     IdBody (int)        : id of the concerned body
  * @param[in]     tol    (double)     : tolerance
  * @param[in,out] matrix_out (int **) : matrix of neighouring indices of each elements
  * @param[out]    dim1 (int* )        : matrix_out first dimension
  * @param[out]    dim2 (int* )        : matrix_out second dimension
  * @endcond
  */
extern "C" void mecaMAILx_GetElementsNeighbor(int idBody, double tol, int** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void mecaMAILx_GetPtrElementsVisibility(int idBody, int ** pointer_out, int * length)
  * @brief Get a pointer on the elements visibility vector
  *
  * @cond PYDOC
  * python usage : eviz = mecaMAILx_GetPtrElementsVisibility(ibdyty)
  * @param[in] ibdyty (integer)  : rank of the mecaMAILx
  * @return    eviz (int array)  : reference on the desired vector seen as a numpy array
  * @endcond
  *
  * @cond CDOC
  * @param[in]  idBody (int)            : rank of the RBDY3
  * @param[out] pointer_out (double **) : reference on the vector
  * @param[in]  length (int*)           : reference on the length of the out vector
  * @endcond
  */
extern "C" void mecaMAILx_GetPtrElementsVisibility(int idBody, int ** pointer_out, int * length);

 /**
  * @fn void mecaMAILx_AddNodalFieldDivergence(int ivalue1, int ivalue2)
  * @brief Add the divergence of a diagonal field to external forces
  *
  * @cond PYDOC
  * python usage : mecaMAILx_AddNodalFieldDivergence(ibdyty, ifield)
  * @param[in] ibdyty (integer)            : rank of body
  * @param[in] ifield (integer)            : rank of field
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)        : id of considered body
  * @param[in] ivalue2 (int)        : id of considered field
  * @endcond
  */
  extern "C" void mecaMAILx_AddNodalFieldDivergence(int ivalue1, int ivalue2);

/**
  * @fn void mecaMAILx_CleanMemory(void)
  * @brief Free all memory allocated within mecaMAILx module
  *
  * @cond PYDOC
  * python usage : mecaMAILx_CleanMemory()
  * @endcond
  */
  extern "C" void mecaMAILx_CleanMemory(void);

 /**
  * @fn void mecaMAILx_ComputeInfoPrincipalStressField( int ivalue1, double ** r8_vector, int * r8_size)
  * @brief Get info on the principal stress field: min,mean,max
  *
  * @cond PYDOC
  * Python usage : info = mecaMAILx_ComputeInfoPrincipalStressField(ibdyty)
  * @param ibdyty (integer)            : rank of considered body
  * @return info (double array)        : the desired info
  * @endcond
  *
  * @cond CDOC
  * @param[in]  ivalue1 (int)        : id of considered body
  * @param[out] r8_vector (double**) : the vector to get
  * @param[out] r8_size (double*)    : the vector length
  * @endcond
  */
extern "C" void mecaMAILx_ComputeInfoPrincipalStressField(int ivalue1, double ** r8_vector, int * r8_size);

 /**
  * @fn void mecaMAILx_ComputePDFPressure( double ** r8_vector, int * r8_size)
  * @brief Get pdf on the pressure
  *
  * @cond PYDOC
  * Python usage : pdf = mecaMAILx_ComputePDFPressure()
  * @return pdf (double array)        : the desired info
  * @endcond
  *
  * @cond CDOC
  * @param[out] r8_vector (double**) : the vector to get
  * @param[out] r8_size (double*)    : the vector length
  * @endcond
  */
extern "C" void mecaMAILx_ComputePDFPressure(double ** r8_vector, int * r8_size);

 /**
  * @fn double mecaMAILx_GetDeformationEnergy(int ivalue1, double * rvector_in, int rlength_in)
  * @brief Get the deformation energy of a given displacement field
  *
  * @cond PYDOC
  * python usage : energy = mecaMAILx_GetDeformationEnergy(id,displacement)
  *
  * @param ibdyty (integer)             : rank of considered body
  * @param displacement (double vector) : displacement field
  * @return energy (double) : deformation energy
  * @endcond
  *
  * @cond CDOC
  * @param ivalue1 (integer)         : rank of considered body
  * @param r8_vector (double vector) : displacement field
  * @param r8_size (double)          : size of vector
  * @return energy (double)          : deformation energy
  * @endcond
  */
  extern "C" double mecaMAILx_GetDeformationEnergy(int ivalue1, double * rvector_in, int rlength_in);

 /**
  * @fn double mecaMAILx_GetKineticEnergy(int ivalue1, double * rvector_in, int rlength_in)
  * @brief Get the kinetic energy of a given velocity field
  *
  * @cond PYDOC
  * python usage : energy = mecaMAILx_GetKineticEnergy(id,velocity)
  *
  * @param ibdyty (integer)             : rank of considered body
  * @param velocity (double vector) : velocity field
  * @return energy (double)             : kinetic energy
  * @endcond
  *
  * @cond CDOC
  * @param ivalue1 (integer)         : rank of considered body
  * @param r8_vector (double vector) : velocity field
  * @param r8_size (double)          : size of vector
  * @return energy (double)          : kinetic energy
  * @endcond
  */
  extern "C" double mecaMAILx_GetKineticEnergy(int ivalue1, double * rvector_in, int rlength_in);



 /**
  * @fn void mecaMAILx_GetNeighborElementsToElement(int idBody, int idEle, int ** i4_vector, int * i4_size)
  * @brief return neighbor elements to element idEle of body idBody
  *
  * @cond PYDOC
  * python usage : vector = mecaMAILx_GetNeighborElementsToElement(idBody,idEle)
  * @param[in] IdBody (integer) : id of the concerned body
  * @param[in] IdEle (integer)  : id of the concerned element
  * @return    vector (integer) : list of elements
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)     : id of the concerned body
  * @param[in]  IdEle (int)      : id of the concerned element
  * @param[out] i4_vecto (int**) : xxx
  * @param[out] dim1 (int *)  i  : dimension
  * @endcond
  */
  extern "C" void mecaMAILx_GetNeighborElementsToElement(int idBody, int idELe, int ** i4_vector, int * i4_size );

 /**
  * @fn void mecaMAILx_GetNeighborElementsToNode(int idBody, int idNode, int ** i4_vector, int * i4_size)
  * @brief return neighbor elements to node idNode of body idBody
  *
  * @cond PYDOC
  * python usage : vector = mecaMAILx_GetNeighborElementsToNode(idBody,idNode)
  * @param[in] IdBody (integer) : id of the concerned body
  * @param[in] IdNode (integer) : id of the concerned node
  * @return    vector (integer) : list of elements
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)     : id of the concerned body
  * @param[in]  IdNode (int)     : id of the concerned node
  * @param[out] i4_vecto (int**) : xxx
  * @param[out] dim1 (int *)  i  : dimension
  * @endcond
  */
  extern "C" void mecaMAILx_GetNeighborElementsToNode(int idBody, int idNode, int ** i4_vector, int * i4_size );

 /**
  * @fn void mecaMAILx_GetBoundaryElements(int idBody, int ** i4_vector, int * i4_size)
  * @brief return boundary elements
  *
  * @cond PYDOC
  * python usage : vector = mecaMAILx_GetBoundaryElements(idBody)
  * @param[in] IdBody (integer) : id of the concerned body
  * @return    vector (integer) : for each element =0 no boundary, otherwise gives the number of free edge/face
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)     : id of the concerned body
  * @param[out] i4_vecto (int**) : xxx
  * @param[out] dim1 (int *)  i  : dimension
  * @endcond
  */
  extern "C" void mecaMAILx_GetBoundaryElements(int idBody, int ** i4_vector, int * i4_size );

 /**
  * @fn void mecaMAILx_LoadWPreconBody(int ivalue)
  * @brief load the precomputed W matrix on support node dofs of contactors for one body. Assumes bulk behaviour is linear. 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_LoadWPreconBody(ivalue)
  * @param[in] ivalue (integer) : id of body to set precon
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue (int) : id of body to set precon
  * @endcond
  */
  extern "C" void mecaMAILx_LoadWPreconBody(int ivalue);


 /**
  * @fn void mecaMAILx_GetPtrPreconW(int idBody, double** pointer_out, int* dim1, int* dim2)
  * @brief Get a pointer on the preconW Matrix of a given body 
  *
  * @cond PYDOC
  * python usage : pcW = mecaMAILx_GetPtrPreconW(ibdyty)
  * @param[in] ibdyty (integer)  : rank of the mecaMAILx
  * @return    pcW (double array)  : reference on the desired vector seen as a numpy array
  * @endcond
  *
  * @cond CDOC
  * @param[in]  idBody (int)            : rank of the mecaMAILx
  * @param[out] pointer_out (double **) : reference on the matrix
  * @param[out] dim1 (int*)             : first dimension of the matrix preconW
  * @param[out] dim2 (int*)             : second dimension of the matrix preconW
  * @endcond
  */
  extern "C" void mecaMAILx_GetPtrPreconW(int idBody, double** pointer_out, int* dim1, int* dim2);

 /**
  * @fn void mecaMAILx_GetInternalVariable(int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of the internal variable of a given body
  *
  * @cond PYDOC
  * Python usage : internal = mecaMAILx_GetInternalVariable(ibdyty)
  * @param ibdyty (integer)         : rank of considered body
  * @return internal (double array) : internal variable of desired body
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)          : id of considered body
  * @param[out] matrix_out (double**) : the stress
  * @param[out] dim1 (int*)           : first dimension of matrix_out
  * @param[out] dim2 (int*)           : second dimension of matrix_out
  * @endcond
  */
  extern "C" void mecaMAILx_GetInternalVariable(int ivalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn int mecaMAILx_GetNbInternal(int ivalue)
  * @brief Get the number of internal variable of a given body
  *
  * @cond PYDOC
  * python usage : nb_internal = mecaMAILx_GetNbInternal(ibdyty)
  * @param[in] ivalue (integer) : rank of the body
  * @return nb_internal (integer) : number of internal variable of a body
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of internal variable
  * @endcond
  */
  extern "C" int mecaMAILx_GetNbInternal(int ivalue);
  
 /**
  * @fn void mecaMAILx_GetPtrBodyVector(char * cvalue1_c, int IdBody, double** pointer_out, int* length)
  * @brief return pointer on body vector cvalue1_c of body IdBody
  *
  * Reac and Raux are impulsions (and not forces)
  *
  * @cond PYDOC
  * python usage : vector_ptr = mecaMAILx_GetPtrBodyVector( cvalue1_c, IdBody )
  * @param[in] cvalue1_c  (string of size 5)  : name of the body vector
  * @param[in] IdBody     (integer)           : id of the body
  * @return vector_ptr (double array)         : reference on the desired body vector
  *
  * @cond CDOC
  * @param[in]    cvalue1     (char[5])   : name of the body vector 
  * @param[in]    IdBody      (int)       : id of the body
  * @param[out]   pointer_out (double **) : reference on the out vector
  * @param[out]   length      (int*)      : reference on the length of the out vector
  * @endcond
  */
  extern "C" void mecaMAILx_GetPtrBodyVector(char * cvalue1_c, int IdBody, double** pointer_out, int* length);
  
 /**
  * @fn void mecaMAILx_GetDofStatus(int idbdy, double ** r8_vector, int * r8_size)
  * @brief Get the status of nodes: 0 free, 1 x, 10 y
  *
  *
  * @cond PYDOC
  * Python usage : vector = mecaMAILx_GetDofStatus(ibdyty)
  * @param ibdyty (integer)            : rank of considered body
  * @return vector (double 2D-array)   : the desired data
  * @endcond
  *
  * @cond CDOC
  * @param[in] idbdy (int)            : id of considered body
  * @param[out] r8_vector (double**)  : the vector to get
  * @param[out] r8_size (int*)        : dimension of r8_vector
  * @endcond
  */
  extern "C" void mecaMAILx_GetDofStatus(int idbdy, double ** r8_vector, int * r8_size);
  
/**
  * @fn void mecaMAILx_PrepGlobalSolver(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes free velocity of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_PrepGlobalSolver(i_list)
  * @param i_list (list of integer) : list of bodies to compute free velocity
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_PrepGlobalSolver(int * ivector_in=nullptr, int ilength_in=0);

/**
  * @fn void mecaMAILx_PostGlobalSolver(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes the current d.o.f knowing all the forces (free + contact) of a list of bodies
  *
  * @cond PYDOC
  * python usage : mecaMAILx_PostGlobalSolver(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute current d.o.f
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mecaMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_PostGlobalSolver(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void mecaMAILx_AddBodyForceToFext(int ivalue1, double * matrix_in, int dim1, int dim2)
  * @brief Add a body force (M*gamma) to Fext for a given body
  *
  * @cond PYDOC
  * python usage : mecaMAILx_AddBodyForceToFext(ibdyty, matrix)
  * @param[in] ibdyty (integer)            : rank of body
  * @param[in] matrix (double array)       : the new value
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)        : id of considered body
  * @param[in] matrix_in (double *) : the new values
  * @param[in] dim1 (int)           : first dimension of matrix_in (in C sense)
  * @param[in] dim2 (int)           : second dimension of matrix_in (in C sense)
  * @endcond
  */
 extern "C" void mecaMAILx_AddBodyForceToFext(int ivalue1, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void mecaMAILx_CheckProperties(void)
  * @brief check if model and material are matching ; set material parameter if external model
  *
  * @cond PYDOC
  * python usage : mecaMAILx_CheckProperties()
  * @endcond
  */
  extern "C" int mecaMAILx_CheckProperties(void);

 /**
  * @fn void mecaMAILx_GetNbGpByElem(char** string_vector, int* vector_size, int* string_size, , int** i4_vector, int* i4_size)
  * @brief Get the list of finite elements for MECAx models and the associated number of Gauss Points.
  *
  * Here memory is allocated within lmgc90 so that the pointer can be freely
  * modified by third parties without nasty effect on lmgc90 functioning.
  *
  * @cond PYDOC
  * python usage : names, nb_gps = mecaMAILx_GetNbGpByElem()
  * @return names  (string list) : list of the finite elements
            nb_gps (integer list): list of the number of Gauss Points
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : list of finite elements
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @param[out] i4_vector (int**) : reference on the integer array holding the list of number of Gauss Points
  * @param[out] i4_size (int*) : reference on the size of the array referenced by i4_vector
  * @endcond
  */
  extern "C" void mecaMAILx_GetNbGpByElem(char** string_vector, int* vector_size, int* string_size, int** i4_vector, int* i4_size);

 /**
  * @fn void mecaMAILx_MassScaling(double scale)
  * @brief set mass scaling (default 1.d0)
  *
  * @cond PYDOC
  * python usage : mecaMAILx_MassScaling(scale)
  * @param[in] scale (double) : scaling 
  * @endcond
  *
  * @cond CDOC
  * @param[in] scale (double) : scaling
  * @endcond
  */
  extern "C" void mecaMAILx_MassScaling(double scale);

 /**
  * @fn void mecaMAILx_GetGpAllJoint(double** matrix_out, int* dim1, int* dim2)
  * @brief return GP value for joints
  *
  * @cond PYDOC
  * python usage : vec = mecaMAILx_GetGpAllJoint()
  * @return vec (float matrix) : value at GP
  * @endcond
  *
  * @cond CDOC
  * @param[out] matrix_out (double**) : value at GP
  * @param[out] dim1 (int*)           : first dimension of the frame
  * @param[out] dim2 (int*)           : second dimension of the frame
  * @endcond
  */
  extern "C" void mecaMAILx_GetGpAllJoint(double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void  mecaMAILx_SetVisibleVlocyDrivenDof( int ibdyty, int inod, int idof )
  * @brief allows to (re)activate a given vlocydrivendof (i.e. which has been declared in preprocessing)
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetVisibleVlocyDrivenDof(ibdyty, inod, idof)
  * @param[in] ibdyty(integer) : index of the mecaMAILx
  * @param[in] inod(integer)   : index of the node to set visible
  * @param[in] idof(integer)   : index of the dof of the node to set visible
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the mecaMAILx
  * @param[in] inod(int)   : index of the node to set visible
  * @param[in] idof(int)   : index of the dof of the node to set visible
  * @endcond
 */
 extern "C" void mecaMAILx_SetVisibleVlocyDrivenDof( int ibdyty, int inod, int idof ) ;

 /**
  * @fn void  mecaMAILx_SetInvisibleVlocyDrivenDof( int ibdyty, int inod, int idof )
  * @brief allows to deactivate a given vlocydrivendof (i.e. which has been declared in preprocessing)
  *
  * @cond PYDOC
  * python usage : mecaMAILx_SetInvisibleVlocyDrivenDof(ibdyty, inod, idof)
  * @param[in] ibdyty(integer) : index of the mecaMAILx
  * @param[in] inod(integer)   : index of the node to set invisible
  * @param[in] idof(integer)   : index of the dof of the node to set invisible
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the mecaMAILx
  * @param[in] inod(int)   : index of the node to set invisible
  * @param[in] idof(int)   : index of the dof of the node to set invisible
  * @endcond
 */
 extern "C" void mecaMAILx_SetInvisibleVlocyDrivenDof( int ibdyty, int inod, int idof ) ;

 /**
  * @fn void  mecaMAILx_UpdateVlocyDrivenDofStructures( int ibdyty )
  * @brief takes into account modifications on Vlocy driven dof status 
  *
  * @cond PYDOC
  * python usage : mecaMAILx_UpdateVlocyDrivenDofStructures(ibdyty)
  * @param[in] ibdyty(integer) : index of the mecaMAILx
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the mecaMAILx
  * @endcond
 */
 extern "C" void mecaMAILx_UpdateVlocyDrivenDofStructures( int ibdyty) ;


#endif /* wrap_mecaMAILx_h */
