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

#ifndef wrap_multiMAILx_h
#define wrap_multiMAILx_h

 /**
  * @fn void multiMAILx_UsePicardScheme(void)
  * @brief use Picard scheme (fixed point method)
  *
  * @cond PYDOC
  * python usage : multiMAILx_UsePicardScheme()
  * @endcond
  */
  extern "C" void multiMAILx_UsePicardScheme(void);

 /**
  * @fn void multiMAILx_UseNewtonScheme(void)
  * @brief use Newton scheme
  *
  * @cond PYDOC
  * python usage : multiMAILx_UseNewtonScheme()
  * @endcond
  */
  extern "C" void multiMAILx_UseNewtonScheme(void);

 /**
  * @fn void multiMAILx_WithoutRenumbering(void)
  * @brief skip renumbering of the unknowns using a rcc method 
  *
  * @cond PYDOC
  * python usage : multiMAILx_WithoutRenumbering()
  * @endcond
  */
  extern "C" int multiMAILx_WithoutRenumbering(void);

 /**
  * @fn void multiMAILx_BandStorage(void)
  * @brief use band matrix 
  *
  * @cond PYDOC
  * python usage : multiMAILx_BandStorage()
  * @endcond
  */
  extern "C" int multiMAILx_BandStorage(void);

 /**
  * @fn void multiMAILx_SparseStorage(void)
  * @brief use sparse matrix
  *
  * @cond PYDOC
  * python usage : multiMAILx_SparseStorage()
  * @endcond
  */
  extern "C" int multiMAILx_SparseStorage(void);

 /**
  * @fn void multiMAILx_ExplodedStorage(void)
  * @brief use element by element matrix 
  *
  * @cond PYDOC
  * python usage : multiMAILx_ExplodedStorage()
  * @endcond
  */
  extern "C" int multiMAILx_ExplodedStorage(void);

 /**
  * @fn void multiMAILx_DiagonalStorage(void)
  * @brief use diagonal matrix
  *
  * @cond PYDOC
  * python usage : multiMAILx_DiagonalStorage()
  * @endcond
  */
  extern "C" int multiMAILx_DiagonalStorage(void);

 /**
  * @fn void multiMAILx_SkylineStorage(void)
  * @brief use skyline matrix
  *
  * @cond PYDOC
  * python usage : multiMAILx_SkylineStorage()
  * @endcond
  */
  extern "C" int multiMAILx_SkylineStorage(void);

 /**
  * @fn void multiMAILx_FullStorage(void)
  * @brief use full matrix
  *
  * @cond PYDOC
  * python usage : multiMAILx_FullStorage()
  * @endcond
  */
  extern "C" int multiMAILx_FullStorage(void);

 /**
  * @fn void multiMAILx_SymmetricShape(void)
  * @brief assume matrix is symmetrical
  *
  * @cond PYDOC
  * python usage : multiMAILx_SymmetricShape()
  * @endcond
  */
  extern "C" int multiMAILx_SymmetricShape(void);

 /**
  * @fn void multiMAILx_UnspecifiedShape(void)
  * @brief does not assume any thing on matrix shape
  *
  * @cond PYDOC
  * python usage : multiMAILx_UnspecifiedShape()
  * @endcond
  */
  extern "C" int multiMAILx_UnspecifiedShape(void);

 /**
  * @fn int multiMAILx_GetNb(void)
  * @brief Get the number of multiMAILx
  *
  * @cond PYDOC
  * python usage : nb_multiMAILx = multiMAILx_GetNb()
  *
  * @return nb_multiMAILx (integer) : number of multiMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of multiMAILx
  * @endcond
  */
  extern "C" int multiMAILx_GetNb(void);

 /**
  * @fn int multiMAILx_GetNbNodes(int ivalue)
  * @brief Get the number of nodes of a multiMAILx
  *
  * @cond PYDOC
  * python usage : nb_nodes = multiMAILx_GetNbNodes(ibdyty)
  * @param[in] ivalue (integer) : id of the multiMAILx
  * @return nb_nodes (integer) : number of nodes of a multiMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of nodes
  * @endcond
  */
  extern "C" int multiMAILx_GetNbNodes(int ivalue);

 /**
  * @fn int multiMAILx_GetNbElements(int ivalue)
  * @brief Get the number of elements of a multiMAILx
  *
  * @cond PYDOC
  * python usage : nb_elements = multiMAILx_GetNbElements(ibdyty)
  * @param[in] ivalue (integer) : id of the multiMAILx
  * @return nb_nodes (integer) : number of elements of a multiMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of elements
  * @endcond
  */
  extern "C" int multiMAILx_GetNbElements(int ivalue);

 /**
  * @fn int multiMAILx_IsVisible(int idbdy)
  * @brief return if a given body visible
  *
  * @cond PYDOC
  * python usage : visible = multiMAILx_IsVisible(ibdyty)
  * @param[in] idbdy(integer) : id of the body we want visibility
  * @return visible (integer) : 1 if body is visible, 0 else
  * @endcond
  *
  * @cond CDOC
  * @param[in] idbdy (int) : id of the body we want visibility
  * @return (int) 1 if body is visible, 0 else
  * @endcond
  */
  extern "C" int multiMAILx_IsVisible(int idbdy);

 /**
  * @fn void multiMAILx_GetBodyVector(char * cvalue1_c, int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of a vector of a given body
  *
  * Possible values for datatype field are "X____", "Xbeg_",
  * "V____", "Vbeg_", "Pw___", "Pwbeg", "Pn___", "Pnbeg"
  *
  * @cond PYDOC
  * Python usage : vector = multiMAILx_GetBodyVector(datatype, ibdyty)
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
  * - "X____": cumulated displacements over time in computed configuration
  * - "Xbeg_": cumulated displacements over time at beginning of time step
  * - "V____": velocity in computed configuration
  * - "Vbeg_": velocity at beginning of time step
  * - "Pcbeg": pressure of 1st fluid at beginning of time step
  * - "Pc___": pressure of 1st fluid in computed configuration
  * - "Pnbeg": pressure of 2nd fluid at beginning of time step
  * - "Pn___": pressure of 2nd fluid in computed configuration
  * - "U_Fex": external forces
  * - "PcFex": external pressure for 1st fluid
  * - "PnFex": external pressure for 2nd fluid
  * - "U_Fin": internal forces
  * - "PcFin": internal pressure for 1st fluid
  * - "PnFin": internal pressure for 2nd fluid
  * - "U_Fdp":
  * - "PcFdp":
  * - "PnFdp":
  * - "U_Fdy":
  * - "PcFdy":
  * - "PnFdy":
  *
  */
  extern "C" void multiMAILx_GetBodyVector(char * cvalue1_c, int ivalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void multiMAILx_PutBodyVector(char * cvalue1_c, int ivalue1, double * matrix_in, int dim1, int dim2)
  * @brief Set a vector of a given body
  *
  * @cond PYDOC
  * python usage : multiMAILx_PutBodyVector(datatype, ibdyty, matrix)
  * @param[in] datatype (string of size 5) : the vector to set
  * @param[in] ibdyty (integer)            : rank of body
  * @param[in] matrix (double array)       : the new values
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
  * - "X____": cumulated displacements over time in computed configuration
  * - "Xbeg_": cumulated displacements over time at beginning of time step
  * - "V____": velocity in computed configuration
  * - "Vbeg_": velocity at beginning of time step
  * - "Pcbeg": pressure of 1st fluid at beginning of time step
  * - "Pc___": pressure of 1st fluid in computed configuration
  * - "Pnbeg": pressure of 2nd fluid at beginning of time step
  * - "Pn___": pressure of 2nd fluid in computed configuration
  */
  extern "C" void multiMAILx_PutBodyVector(char * cvalue1_c, int ivalue1, double* matrix_in, int dim1, int dim2);

 /**
  * @fn void multiMAILx_ReadDrivenDof(void)
  * @brief Read DRV_DOF.DAT
  *
  * @cond PYDOC
  * python usage : multiMAILx_ReadDrivenDof()
  * @endcond
  */
  extern "C" void multiMAILx_ReadDrivenDof(void);
    
 /**
  * @fn void multiMAILx_WriteDrivenDof(void)
  * @brief Write DRV_DOF.OUT
  *
  * @cond PYDOC
  * python usage : multiMAILx_WriteDrivenDof()
  * @endcond
  */
  extern "C" void multiMAILx_WriteDrivenDof(void);

 /**
  * @fn void multiMAILx_ReadIniGPV(int num=0)
  * @brief Read GPV file
  *
  * If num <= 0 : DATBOX/GPV.INI file is read
  *
  * Else : OUTBOX/GPV.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniGPV
  * last call
  *
  * @cond PYDOC
  * python usage : multiMAILx_ReadIniGPV(num=0)
  * @param[in] num (integer) : which GPV file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which GPV file to read
  * @endcond
  *
  */
  extern "C" void multiMAILx_ReadIniGPV(int num=0);

 /**
  * @fn void multiMAILx_ReadIniDof(int num=0)
  * @brief Read DOF file
  *
  * If num <= 0 : DATBOX/DOF.INI file is read
  *
  * Else : OUTBOX/DOF.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniDof
  * last call
  *
  * @cond PYDOC
  * python usage : multiMAILx_ReadIniDof(num=0)
  * @param[in] num (integer) : which DOF file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which DOF file to read
  * @endcond
  *
  */
  extern "C" void multiMAILx_ReadIniDof(int num=0);

 /**
  * @fn void multiMAILx_WriteLastDof(int * ivector_in=nullptr, int ilength_in=0)
  * @brief Write DOF.LAST file
  *
  * @cond PYDOC
  * python usage : multiMAILx_WriteLastDof(i_list)
  * @param[in] i_list (list of integer) : list of bodies to write dof
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of multiMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void multiMAILx_WriteLastDof(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void multiMAILx_WriteOutDof(int * ivector_in=nullptr, int ilength_in=0)
  * @brief Write DOF.OUT file
  *
  * @cond PYDOC
  * python usage : multiMAILx_WriteOutDof(i_list)
  * @param[in] i_list (list of integer) : list of bodies to write dof
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of multiMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void multiMAILx_WriteOutDof(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void multiMAILx_LoadBehaviours(void)
  * @brief load behaviours from bulk_behav 
  *
  * @cond PYDOC
  * python usage : multiMAILx_LoadBehaviours()
  * @endcond
  */
  extern "C" void multiMAILx_LoadBehaviours(void);

 /**
  * @fn void multiMAILx_LoadModels(void)
  * @brief load models from models
  *
  * @cond PYDOC
  * python usage : multiMAILx_LoadModels()
  * @endcond
  */
  extern "C" void multiMAILx_LoadModels(void);

 /**
  * @fn void multiMAILx_PushProperties(void)
  * @brief gives to model the couple of model,behavior used at gauss point
  *
  * @cond PYDOC
  * python usage : multiMAILx_PushProperties()
  * @endcond
  */
  extern "C" void multiMAILx_PushProperties(void);

 /**
  * @fn void multiMAILx_IncrementStep(void)
  * @brief initializes the current d.o.f and some driven d.o.f values 
  *
  * @cond PYDOC
  * python usage : multiMAILx_IncrementStep()
  * @endcond
  */
  extern "C" void multiMAILx_IncrementStep(void);

 /**
  * @fn void multiMAILx_ComputeMass(int * ivector_in=nullptr, int ilength_in=0)
  * @brief compute elementary mass and inertia of a list of bodies
  *
  * @cond PYDOC
  * python usage : multiMAILx_ComputeMass(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute mass and inertia
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of multiMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void multiMAILx_ComputeMass(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void multiMAILx_ComputeBulk(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes elementary stiffness and viscosity matrices  of a list of bodies
  *
  * @cond PYDOC
  * python usage : multiMAILx_ComputeBulk(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute stiffness and viscosity matrices
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of multiMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void multiMAILx_ComputeBulk(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void multiMAILx_ComputeFext(int * ivector_in=nullptr, int ilength_in=0)
  * @brief compute elementary external forces of a list of bodies
  *
  * @cond PYDOC
  * python usage : multiMAILx_ComputeFext(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute external forces
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of multiMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void multiMAILx_ComputeFext(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void multiMAILx_AssembKT(int * ivector_in=nullptr, int ilength_in=0)
  * @brief assemble pseudo mass matrix and apply drvdof of a list of bodies
  *
  * @cond PYDOC
  * python usage : multiMAILx_AssembKT(i_list)
  * @param i_list (list of integer) : list of bodies to assemble pseudo mass matrix and apply drvdof
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of multiMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void multiMAILx_AssembKT(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void multiMAILx_AssembRHS(int * ivector_in=nullptr, int ilength_in=0)
  * @brief assembles right hand side of a list of bodies
  *
  * @cond PYDOC
  * python usage : multiMAILx_AssembRHS(i_list)
  * @param i_list (list of integer) : list of bodies to assemble right hand side
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of multiMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void multiMAILx_AssembRHS(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn double multiMAILx_ComputeResidueNorm(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes the norm of the residue of a list of bodies
  *
  * @cond PYDOC
  * python usage : norm = multiMAILx_ComputeResidueNorm(i_list)
  * @param i_list (list of integer) : list of bodies to compute the norm of the residue
  *        if omitted works on all objects
  * @return norm (double) : Residue Norm
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of mULTIMAILx
  * @param[in] length (int)     : size of vector_in
  * @return norm (double)       : Residue Norm
  * @endcond
  */
  extern "C" double multiMAILx_ComputeResidueNorm(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void multiMAILx_ComputeFreeState(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes free (of interactions) state of a list of bodies
  *
  * @cond PYDOC
  * python usage : multiMAILx_ComputeFreeState(i_list)
  * @param i_list (list of integer) : list of bodies to compute free state
  *        if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of multiMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void multiMAILx_ComputeFreeState(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void multiMAILx_ComputeDof(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes the current d.o.f knowing all the forces/fluxses (free + contact) of a list of bodies
  *
  * @cond PYDOC
  * python usage : multiMAILx_ComputeDof(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute current d.o.f
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of multiMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void multiMAILx_ComputeDof(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void multiMAILx_ComputeField(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes elementary fields  of a list of bodies
  *
  * @cond PYDOC
  * python usage : multiMAILx_ComputeField(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute elementary fields
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of multiMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void multiMAILx_ComputeField(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void multiMAILx_UpdateBulk(int * ivector_in=nullptr, int ilength_in=0)
  * @brief update begin elementary fields with current elementary fields of a list of bodies
  *
  * @cond PYDOC
  * python usage : multiMAILx_UpdateBulk(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute elementary fields
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of multiMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void multiMAILx_UpdateBulk(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void multiMAILx_UpdateDof(int * ivector_in=nullptr, int ilength_in=0)
  * @brief update begin d.o.f. with current d.o.f. of a list of bodies
  *
  * @cond PYDOC
  * python usage : multiMAILx_UpdateDof(i_list)
  * @param[in] i_list (list of integer) : list of bodies to update current d.o.f
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of multiMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void multiMAILx_UpdateDof(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn int multiMAILx_GetScalarFieldRank(int ibdyty, int iblmty, char* field_name);
  * @brief Get the rank of field of an element of a body from its name
  *
  * @cond PYDOC
  * python usage : f_rank = multiMAILx_GetScalarFieldRank(ibdyty, iblmty, name)
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
  extern "C" int multiMAILx_GetScalarFieldRank(int ibdyty, int blmty, char* name);

 /**
  * @fn void multiMAILx_SetScalarFieldByNode(int IdBody, int f_rank, double * rvector_in, int rlength_in)
  * @brief Update elementary fields through a nodal external field on a given body
  *
  * You need to declare this field in your MODELS.DAT
  *
  * @cond PYDOC
  * python usage : multiMAILx_SetScalarFieldByNode(IdBody, f_rank, f)
  * @param[in] IdBody (integer) : id of the concern body
  * @param[in] f_rank (integer) : rank of the field to set 
  * @param[in] f (double array) : value of the field
  * @endcond
  *
  * @cond CDOC
  * @param[in] IdBody (int)               : id of the concern body
  * @param[in] f_rank (int)               : rank of the field
  * @param[in] vector_in (double[length]) : value of the field
  * @param[in] length (int)               : size of the field
  * @endcond
  */
  extern "C" void multiMAILx_SetScalarFieldByNode(int IdBody, int f_rank, double * rvector_in, int rlength_in);

 /**
  * @fn void multiMAILx_SetScalarFieldByElement(int IdBody, int f_rank, double * rvector_in, int rlength_in)
  * @brief Update elementary scalar field through a element external field on a given body
  *
  * Field values are stored at Gauss point, on an element all Gauss point have the element value
  *
  * You need to declare this field in your MODELS.DAT
  *
  * @cond PYDOC
  * python usage : multiMAILx_SetScalarFieldByElement(IdBody, f_rank, f)
  * @param[in] IdBody (integer) : id of the concern body
  * @param[in] f_rank (integer) : rank of the field to set 
  * @param[in] f (double array) : value of the field
  * @endcond
  *
  * @cond CDOC
  * @param[in] IdBody (int)               : id of the concern body
  * @param[in] f_rank (int)               : rank of the field
  * @param[in] vector_in (double[length]) : value of the field
  * @param[in] length (int)               : size of the field
  * @endcond
  */
  extern "C" void multiMAILx_SetScalarFieldByElement(int IdBody, int f_rank, double * rvector_in, int rlength_in);

 /**
  * @fn int multiMAILx_GetVectorFieldRank(int ibdyty, int iblmty, char* field_name);
  * @brief Get the rank of field of an element of a body from its name
  *
  * @cond PYDOC
  * python usage : f_rank = multiMAILx_GetVectorFieldRank(ibdyty, iblmty, name)
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
  extern "C" int multiMAILx_GetVectorFieldRank(int ibdyty, int blmty, char* name);

 /**
  * @fn void multiMAILx_SetVectorFieldByNode(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2)
  * @brief Update elementary fields through a nodal external field on a given body
  *
  * Use the form functions of the elements and input values to compute and store field values
  * at Gauss points.
  *
  * You need to declare this field in your MODELS.DAT
  *
  * @cond PYDOC
  * python usage : multiMAILx_SetFieldByNode(IdBody, f_rank, f)
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
  extern "C" void multiMAILx_SetVectorFieldByNode(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void multiMAILx_SetVectorFieldByElement(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2)
  * @brief Update elementary fields through a nodal external field on a given body
  *
  * Use the form functions of the elements and input values to compute and store field values
  * at Gauss points.
  *
  * You need to declare this field in your MODELS.DAT
  *
  * @cond PYDOC
  * python usage : multiMAILx_SetFieldByElement(IdBody, f_rank, f)
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
  extern "C" void multiMAILx_SetVectorFieldByElement(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void multiMAILx_GetConnectivity(int idBody,  int ** i4_vector, int * i4_size)
  * @brief return connectivity of idBody elements
  *
  * @cond PYDOC
  * python usage : vector = multiMAILx_GetConnectivity(idBody)
  * @param[in] IdBody (integer) : id of the concerned body
  * @return    vector (integer)  : connectivity
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)     : id of the concerned body
  * @param[out] i4_vecto (int**) : xxx
  * @param[out] dim1 (int *)     : dimension
  * @endcond
  */
  extern "C" void multiMAILx_GetConnectivity(int idBody, int ** i4_vector, int * i4_size );

 /**
  * @fn void multiMAILx_GetCoor(int idBody,  double ** matrix_out, int * dim1, int * dim2)
  * @brief return node coordinates of idBody
  *
  * @cond PYDOC
  * python usage : array = multiMAILx_GetCoor(idBody)
  * @param[in] IdBody (integer)     : id of the concerned body
  * @return array (double 2D-array) : coordinates
  * @endcond
  *
  * @cond CDOC
  * @param[in]     IdBody (int)           : id of the concerned body
  * @param[in,out] matrix_out (double **) : xxx
  * @param[in]     dim1 (int *)           : matrix_out first dimension
  * @param[in]     dim2 (int *)           : matrix_out second dimension
  * @endcond
  */
  extern "C" void multiMAILx_GetCoor(int idBody, double ** matrix_out, int * dim1, int * dim2);

 /**
  * @fn void multiMAILx_GetAll(int idBody,  double ** matrix_out, int * dim1, int * dim2)
  * @brief return mechanical data computed for idBody
  *
  * @cond PYDOC
  * python usage : array = multiMAILx_GetAll(idBody)
  * @param[in] IdBody (integer)     : id of the concerned body
  * @return array (double 2D-array) : mechanical data
  * @endcond
  *
  * @cond CDOC
  * @param[in]     IdBody (int)           : id of the concerned body
  * @param[in,out] matrix_out (double **) : xxx
  * @param[in]     dim1 (int *)           : matrix_out first dimension
  * @param[in]     dim2 (int *)           : matrix_out second dimension
  * @endcond
  */
  extern "C" void multiMAILx_GetAll(int idBody, double ** matrix_out, int * dim1, int * dim2);

 /**
  * @fn void multiMAILx_GetElementsVolume(int idBody,  double** r8_vector, int* r8_size)
  * @brief return volume of elements
  *
  * @cond PYDOC
  * python usage : volumes = multiMAILx_GetElementsVolume(idBody)
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
  extern "C" void multiMAILx_GetElementsVolume(int idBody, double** r8_vector, int* r8_size );

 /**
  * @fn void multiMAILx_GetElementsNeighbor(int idBody, double tol, int max_neighbors, int** matrix_out, int* dim1, int* dim2)
  * @brief return elements in the tol-neighbor of an element of idBody
  *
  * @cond PYDOC
  * python usage : neighbors = multiMAILx_GetElementsNeighbor(idBody,tol,max_neighbors)
  * @param[in] IdBody (integer)     : id of the concerned body
  * @param[in] tol (double)         : tolerance
  * @return array (double 2D-array) : neighbor[nb_ele,max_neighbors]
  * @endcond
  *
  * @cond CDOC
  * @param[in]     IdBody (int)           : id of the concerned body
  * @param[in,out] matrix_out (int **)    : xxx
  * @param[in]     dim1 (int*)           : matrix_out first dimension
  * @param[in]     dim2 (int*)           : matrix_out second dimension
  * @endcond
  */
  extern "C" void multiMAILx_GetElementsNeighbor(int idBody, double tol, int max_neighbors, int** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void multiMAILx_GetPtrElementsEnergy(int idBody,  double** pointer_out, int* length)
  * @brief return pointer on energy of elements
  *
  * @cond PYDOC
  * python usage : energies = multiMAILx_GetPtrElementsEnergy(idBody)
  * @param[in] IdBody (integer) : id of the concerned body
  * @return energies[nb_ele] (double) : energy 
  * @endcond
  *
  * @cond CDOC
  * @param[in]     IdBody (int)           : id of the concerned body
  * @param[in,out] pointer_out (double**) : xxx
  * @param[in]     length (int*)          : dimension
  * @endcond
  */
  extern "C" void multiMAILx_GetPtrElementsEnergy(int idBody, double** pointer_out, int* length);

 /**
  * @fn void multiMAILx_ComputeElementsEnergy(int idBody)
  * @brief compute energy of elements
  *
  * @cond PYDOC
  * python usage : multiMAILx_ComputeElementsEnergy(idBody)
  * @endcond
  *
  * @cond CDOC
  * @param[in] IdBody (int) : id of the concerned body
  * @endcond
  */
  extern "C" void multiMAILx_ComputeElementsEnergy(int idBody);

 /**
  * @fn void multiMAILx_GetPtrElementsJacobian(int idBody,  double** pointer_out, int* length)
  * @brief return jacobian of elements
  *
  * @cond PYDOC
  * python usage : jacobians = multiMAILx_GetPtrElementsJacobian(idBody)
  * @param[in] IdBody (integer) : id of the concerned body
  * @return jacobians[nb_ele] (double)  : jacobian
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)           : id of the concerned body
  * @param[out] pointer_out (double**) : xxx
  * @param[out] length (int*)          : dimension
  * @endcond
  */
  extern "C" void multiMAILx_GetPtrElementsJacobian(int idBody, double** pointer_out, int* length);

 /**
  * @fn void multiMAILx_ComputeElementsJacobian(int idBody)
  * @brief compute jacobian of elements
  *
  * @cond PYDOC
  * python usage : multiMAILx_ComputeElementsJacobian(idBody)
  * @endcond
  *
  * @cond CDOC
  * @param[in] IdBody (int) : id of the concerned body
  * @endcond
  */
  extern "C" void multiMAILx_ComputeElementsJacobian(int idBody);

 /**
  * @fn void multiMAILx_GetPtrElementsVisibility(int idBody, int ** pointer_out, int * length)
  * @brief Get a pointer on the elements visibility vector
  *
  * @cond PYDOC
  * python usage : eviz = multiMAILx_GetPtrElementsVisibility(ibdyty)
  * @param[in] ibdyty (integer)  : rank of the multiMAILx
  * @return    eviz (int array)  : reference on the desired vector seen as a numpy array
  * @endcond
  *
  * @cond CDOC
  * @param[in]  idBody (int)            : rank of the RBDY3
  * @param[out] pointer_out (double **) : reference on the vector
  * @param[in]  length (int*)           : reference on the length of the out vector
  * @endcond
  */
  extern "C" void multiMAILx_GetPtrElementsVisibility(int idBody, int ** pointer_out, int * length);

 /**
  * @fn double multiMAILx_GetDeformationEnergy(int ivalue1, double * matrix_in, int dim1, int dim2)
  * @brief Get the deformation energy of a given displacement field
  *
  * @cond PYDOC
  * python usage : energy = multiMAILx_GetDeformationEnergy(id,displacement)
  *
  * @param ibdyty (integer)             : rank of considered body
  * @param displacement (double matrix) : displacement field
  * @return energy (double) : deformation energy
  * @endcond
  *
  * @cond CDOC
  * @param ivalue1 (integer)   : rank of considered body
  * @param matrix_in (double*) : displacement field
  * @param dim1 (integer)      : first dimension of matrix_in
  * @param dim2 (integer)      : second dimension of matrix_in
  * @return energy (double)    : deformation energy
  * @endcond
  */
  extern "C" double multiMAILx_GetDeformationEnergy(int ivalue1, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void multiMAILx_GetPtrBoundaryElements(int idBody, int ** i4_vector, int * i4_size)
  * @brief return boundary elements
  *
  * @cond PYDOC
  * python usage : vector = multiMAILx_GetPtrBoundaryElements(idBody)
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
  extern "C" void multiMAILx_GetPtrBoundaryElements(int idBody, int ** i4_vector, int * i4_size );


 /**
  * @fn void multiMAILx_CleanMemory(void)
  * @brief Free all memory allocated within multiMAILx module
  *
  * @cond PYDOC
  * python usage : multiMAILx_CleanMemory()
  * @endcond
  */
  extern "C" void multiMAILx_CleanMemory(void);

#endif /* wrap_multiMAILx_h */
