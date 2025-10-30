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

#ifndef wrap_poroMAILx_h
#define wrap_poroMAILx_h

 /**
  * @fn void poroMAILx_LoadModels(void)
  * @brief load from MAILx and models
  *
  * @cond PYDOC
  * python usage : poroMAILx_LoadModels()
  * @endcond
  */
  extern "C" void poroMAILx_LoadModels(void);

 /**
  * @fn void poroMAILx_LoadBehaviours(void)
  * @brief load from bulk_behav
  *
  * @cond PYDOC
  * python usage : pordMAILx_LoadBehaviours()
  * @endcond
  */
  extern "C" void poroMAILx_LoadBehaviours(void);
  
   /**
  * @fn void poroMAILx_PushProperties(void)
  * @brief declares to models couple (model,behav)
  *
  * @cond PYDOC
  * python usage : poroMAILx_PushProperties()
  * @endcond
  */
  extern "C" void poroMAILx_PushProperties(void);

 /**
  * @fn void poroMAILx_ReadDrivenDof(void)
  * @brief Read DRV_DOF.DAT
  *
  * @cond PYDOC
  * python usage : poroMAILx_ReadDrivenDof()
  * @endcond
  */
  extern "C" void poroMAILx_ReadDrivenDof(void);

 /**
  * @fn void poroMAILx_WriteDrivenDof(void)
  * @brief Write DRV_DOF.OUT
  *
  * @cond PYDOC
  * python usage : poroMAILx_WriteDrivenDof()
  * @endcond
  */
  extern "C" void poroMAILx_WriteDrivenDof(void);

 /**
  * @fn void poroMAILx_ReadIniDof(int num=0)
  * @brief Read DOF.INI
  *
  * If num <= 0 : DATBOX/DOF.INI file is read
  *
  * Else : OUTBOX/DOF.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniDof
  * last call
  *
  * @cond PYDOC
  * python usage : poroMAILx_ReadIniDof(num=0)
  * @param[in] num (integer) : which DOF file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which DOF file to read
  * @endcond
  *
  */
  extern "C" void poroMAILx_ReadIniDof(int num=0);

 /**
  * @fn void poroMAILx_ReadIniMecaDof(int num=0)
  * @brief Read DOF file
  *
  * If num <= 0 : DATBOX/DOF.INI file is read
  *
  * Else : OUTBOX/DOF.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniMecaDof
  * last call
  *
  * @cond PYDOC
  * python usage : poroMAILx_ReadIniMecaDof(num=0)
  * @param[in] num (integer) : which DOF file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which DOF file to read
  * @endcond
  *
  */
  extern "C" void poroMAILx_ReadIniMecaDof(int num=0);

 /**
  * @fn void poroMAILx_ReadIniGPV(int num=0)
  * @brief Read GPV file
  *
  * If num <= 0 : DATBOX/GPV.INI file is read
  *
  * Else : OUTBOX/GPV.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniGPV
  * last call
  *
  * @cond PYDOC
  * python usage : poroMAILx_ReadIniGPV(num=0)
  * @param[in] num (integer) : which GPV file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which GPV file to read
  * @endcond
  *
  */
  extern "C" void poroMAILx_ReadIniGPV(int num=0);

 /**
  * @fn void poroMAILx_ReadIniMecaGPV(int num=0)
  * @brief Read GPV file
  *
  * If num <= 0 : DATBOX/GPV.INI file is read
  * Else : OUTBOX/GPV.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniMecaGPV
  * last call
  *
  * @cond PYDOC
  * python usage : poroMAILx_ReadIniMecaGPV(num=0)
  * @param[in] num (integer) : which GPV file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which GPV file to read
  * @endcond
  *
  */
  extern "C" void poroMAILx_ReadIniMecaGPV(int num=0);

 /**
  * @fn void poroMAILx_WriteLastDof(void)
  * @brief Write ascii DOF.LAST file
  *
  * @cond PYDOC
  * python usage : poroMAILx_WriteLastDof()
  * @endcond
  */
  extern "C" void poroMAILx_WriteLastDof(void);

 /**
  * @fn void poroMAILx_ComputeMass(void)
  * @brief compute elementary mass and inertia of bodies
  *
  * @cond PYDOC
  * python usage : poroMAILx_ComputeMass()
  * @endcond
  */
  extern "C" void poroMAILx_ComputeMass(void);

 /**
  * @fn void poroMAILx_ComputeFext(void)
  * @brief compute elementary external forces
  *
  * @cond PYDOC
  * python usage : poroMAILx_ComputeFext()
  * @endcond
  */
  extern "C" void poroMAILx_ComputeFext(void);


 /**
  * @fn void poroMAILx_ComputeBulk(void)
  * @brief compute elementary stiffness
  *
  * @cond PYDOC
  * python usage : poroMAILx_ComputeBulk()
  * @endcond
  */
  extern "C" void poroMAILx_ComputeBulk(void);

  /**
  * @fn void poroMAILx_ComputeDamping(void)
  * @brief compute elemenatry damping
  *
  * @cond PYDOC
  * python usage : poroMAILx_ComputeDamping()
  * @endcond
  */
  extern "C" void poroMAILx_ComputeDamping(void);
  
  /**
  * @fn void poroMAILx_AssembKT(void)
  * @brief assembles matrice
  *
  * @cond PYDOC
  * python usage : poroMAILx_AssembKT()
  * @endcond
  */
  extern "C" void poroMAILx_AssembKT(void);
  
  /**
  * @fn void poroMAILx_AssembRHS(void)
  * @brief assembles RHS
  *
  * @cond PYDOC
  * python usage : poroMAILx_AssembRHS()
  * @endcond
  */
  extern "C" void poroMAILx_AssembRHS(void);
  
   /**
  * @fn void poroMAILx_ComputeFreeVelocity(void)
  * @brief computes free motion (without contact contribution)
  *
  * @cond PYDOC
  * python usage : poroMAILx_ComputeFreeVelocity()
  * @endcond
  */
  extern "C" void poroMAILx_ComputeFreeVelocity(void);

  /**
  * @fn void poroMAILx_ComputeDof(void)
  * @brief computes motion (free + contact)
  *
  * @cond PYDOC
  * python usage : poroMAILx_ComputeDof()
  * @endcond
  */
  extern "C" void poroMAILx_ComputeDof(void);
  
  /**
  * @fn void poroMAILx_DisplayOutDof(void)
  * @brief Display body degrees of freedom
  *
  * @cond PYDOC
  * python usage : poroMAILx_DisplayOutDof()
  * @endcond
  */
  extern "C" void poroMAILx_DisplayOutDof(void);

  /**
  * @fn void poroMAILx_UpdateDof(void)
  * @brief update begin dof with current dof
  *
  * @cond PYDOC
  * python usage : poroMAILx_UpdateDof()
  * @endcond
  */
  extern "C" void poroMAILx_UpdateDof(void); 

  /**
  * @fn void poroMAILx_UpdateBulk(void)
  * @brief update begin elementary fields with current elementary fields
  *
  * @cond PYDOC
  * python usage : poroMAILx_UpdateBulk(void)
  * @endcond
  */
  extern "C" void poroMAILx_UpdateBulk(void);

  /**
  * @fn void poroMAILx_ComputeGrad(void)
  * @brief apply elementary fields gradient
  *
  * @cond PYDOC
  * python usage : poroMAILx_ComputeGrad(void)
  * @endcond
  */
  extern "C" void poroMAILx_ComputeGrad(void);

 /**
  * @fn void poroMAILx_GetBodyVector(char * cvalue1_c, int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of a vector of a given body
  *
  * Possible values for datatype field are "X____", "Xbeg_",
  * "V____", "Vbeg_", "Vaux_", "Reac_", "Vfree", "Fext_", "Fint_"
  *
  * @cond PYDOC
  * Python usage : vector = poroMAILx_GetBodyVector(datatype, ibdyty)
  * @param datatype (string of size 5) : the vector to get
  * @param ibdyty (integer)            : rank of considered body
  * @return vector (double 2D-array)   : the desired vector
  * @endcond
  *
  * @cond CDOC
  * @param[in]  cvalue1 (char[5])     : the vector to get
  * @param[in]  ivalue1 (int)         : id of considered body
  * @param[out] matrix_out (double**) : the vector to get
  * @param[out] dim1 (int*)           : first dimension of matrix_out
  * @param[out] dim2 (int*)           : second dimension of matrix_out
  * @endcond
  *
  * - "Xbeg_": cumulated displacements over time at beginning of time step
  * - "X____": cumulated displacements over time in computed configuration
  * - "Vbeg_": velocity at beginning of time step
  * - "V____": velocity in computed configuration
  * - "VbALE": fluid velocity at beginning of time step
  * - "V_ALE": fluid velocity at beginning of time step
  * - "Vaux_": working array for velocity
  * - "Vfree": velocity free of contacts
  * - "Reac_": contact reaction force
  * - "Fext_": external forces
  * - "Fint_": internal forces
  * - "Pbeg_": pressure at beginning of time step
  * - "P____": pressure in computed configuration
  * - "Qext_": external fluxes
  * - "Qint_": internal luxces
  * - "NodId": 
  *
  */
  extern "C" void poroMAILx_GetBodyVector(char * cvalue1_c, int ivalue1, double** matrix_out, int* dim1, int* dim2);
  
   /**
  * @fn int poroMAILx_GetNbNodes(int ivalue)
  * @brief Get the number of nodes of a poroMAILx
  *
  * @cond PYDOC
  * python usage : nb_nodes = poroMAILx_GetNbNodes(ibdyty)
  * @param[in] ivalue (integer) : id of the poroMAILx
  * @return nb_nodes (integer) : number of nodes of a poroMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of nodes
  * @endcond
  */
  extern "C" int poroMAILx_GetNbNodes(int ivalue);
  
 /**
  * @fn int poroMAILx_GetNbElements(int ivalue)
  * @brief Get the number of elements of a poroMAILx
  *
  * @cond PYDOC
  * python usage : nb_elements = poroMAILx_GetNbElements(ibdyty)
  * @param[in] ivalue (integer) : id of the poroMAILx
  * @return nb_nodes (integer) : number of elements of a poroMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of elements
  * @endcond
  */
  extern "C" int poroMAILx_GetNbElements(int ivalue);

  /**
  * @fn void poroMAILx_IncrementStep(void)
  * @brief correction of the configuration parameter using the theta-method
  *
  * @cond PYDOC
  * python usage : poroMAILx_IncrementStep()
  * @endcond
  */
  extern "C" void poroMAILx_IncrementStep(void);

 /**
  * @fn void poroMAILx_WithoutRenumbering(void)
  * @brief skip renumbering of the unknowns using a rcc method 
  *
  * @cond PYDOC
  * python usage : poroMAILx_WithoutRenumbering()
  * @endcond
  */
  extern "C" int poroMAILx_WithoutRenumbering(void);

 /**
  * @fn void poroMAILx_BandStorage(void)
  * @brief use band matrix 
  *
  * @cond PYDOC
  * python usage : poroMAILx_BandStorage()
  * @endcond
  */
  extern "C" int poroMAILx_BandStorage(void);

 /**
  * @fn void poroMAILx_SparseStorage(void)
  * @brief use sparse matrix
  *
  * @cond PYDOC
  * python usage : poroMAILx_SparseStorage()
  * @endcond
  */
  extern "C" int poroMAILx_SparseStorage(void);

 /**
  * @fn void poroMAILx_ExplodedStorage(void)
  * @brief use element by element matrix 
  *
  * @cond PYDOC
  * python usage : poroMAILx_ExplodedStorage()
  * @endcond
  */
  extern "C" int poroMAILx_ExplodedStorage(void);

 /**
  * @fn void poroMAILx_DiagonalStorage(void)
  * @brief use diagonal matrix
  *
  * @cond PYDOC
  * python usage : poroMAILx_DiagonalStorage()
  * @endcond
  */
  extern "C" int poroMAILx_DiagonalStorage(void);

 /**
  * @fn void poroMAILx_SkylineStorage(void)
  * @brief use skyline matrix
  *
  * @cond PYDOC
  * python usage : poroMAILx_SkylineStorage()
  * @endcond
  */
  extern "C" int poroMAILx_SkylineStorage(void);

 /**
  * @fn void poroMAILx_FullStorage(void)
  * @brief use full matrix
  *
  * @cond PYDOC
  * python usage : poroMAILx_FullStorage()
  * @endcond
  */
  extern "C" int poroMAILx_FullStorage(void);

 /**
  * @fn void poroMAILx_SymmetricShape(void)
  * @brief assume matrix is symmetrical
  *
  * @cond PYDOC
  * python usage : poroMAILx_SymmetricShape()
  * @endcond
  */
  extern "C" int poroMAILx_SymmetricShape(void);

 /**
  * @fn void poroMAILx_UnspecifiedShape(void)
  * @brief does not assume any thing on matrix shape
  *
  * @cond PYDOC
  * python usage : poroMAILx_UnspecifiedShape()
  * @endcond
  */
  extern "C" int poroMAILx_UnspecifiedShape(void);

 /**
  * @fn void poroMAILx_SetMecaScalarFieldByNode(int IdBody, int f_rank, double * rvector_in, int rlength_in)
  * @brief Update an  external field on a given body
  *
  * @cond PYDOC
  * python usage : poroMAILx_SetMecaScalarFieldByNode(IdBody, f_rank, f)
  * @param[in] IdBody (integer) : id of the concern body
  * @param[in] f_rank (integer) : rank of the field to set 
  * @param[in] f (double array) : value of the field
  * @endcond
  *
  * \n You need to set this field in your models.dat\n 
  *
  * @cond CDOC
  * @param[in] IdBody (int)               : id of the concern body
  * @param[in] f_rank (int)               : rank of the field
  * @param[in] vector_in (double[length]) : value of the field
  * @param[in] length (int)               : size of the field
  * @endcond
  */
  extern "C" void poroMAILx_SetMecaScalarFieldByNode(int IdBody, int f_rank, double * rvector_in, int rlength_in);

 /**
  * @fn void poroMAILx_SetTherScalarFieldByNode(int IdBody, int f_rank, double * rvector_in, int rlength_in)
  * @brief Update an  external field on a given body
  *
  * @cond PYDOC
  * python usage : poroMAILx_SetTherieldByNode(IdBody, f_rank, f)
  * @param[in] IdBody (integer) : id of the concern body
  * @param[in] f_rank (integer) : rank of the field to set 
  * @param[in] f (double array) : value of the field
  * @endcond
  *
  * \n You need to set this field in your models.dat\n 
  *
  * @cond CDOC
  * @param[in] IdBody (int)               : id of the concern body
  * @param[in] f_rank (int)               : rank of the field
  * @param[in] vector_in (double[length]) : value of the field
  * @param[in] length (int)               : size of the field
  * @endcond
  */
  extern "C" void poroMAILx_SetTherScalarFieldByNode(int IdBody, int f_rank, double * rvector_in, int rlength_in);

 /**
  * @fn void poroMAILx_SetMecaScalarFieldByElement(int IdBody, int f_rank, double * rvector_in, int rlength_in)
  * @brief Update elementary scalar field through a element external field on a given body
  *
  * Field values are stored at Gauss point, on an element all Gauss point have the element value
  *
  * You need to declare this field in your MODELS.DAT
  *
  * @cond PYDOC
  * python usage : poroMAILx_SetMecaScalarFieldByElement(IdBody, f_rank, f)
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
  extern "C" void poroMAILx_SetMecaScalarFieldByElement(int IdBody, int f_rank, double * rvector_in, int rlength_in);

 /**
  * @fn void poroMAILx_SetTherScalarFieldByElement(int IdBody, int f_rank, double * rvector_in, int rlength_in)
  * @brief Update elementary scalar field through a element external field on a given body
  *
  * Field values are stored at Gauss point, on an element all Gauss point have the element value
  *
  * You need to declare this field in your MODELS.DAT
  *
  * @cond PYDOC
  * python usage : poroMAILx_SetTherScalarFieldByElement(IdBody, f_rank, f)
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
  extern "C" void poroMAILx_SetTherScalarFieldByElement(int IdBody, int f_rank, double * rvector_in, int rlength_in);

 /**
  * @fn int poroMAILx_GetMecaScalarFieldRank(int ibdyty, int iblmty, char* field_name);
  * @brief Get the rank of field of an element of a body from its name
  *
  * @cond PYDOC
  * python usage : f_rank = poroMAILx_GetMecaScalarFieldRank(ibdyty, iblmty, name)
  * @param[in] ibdyty (integer) : id of the concern body
  * @param[in] iblmty (integer) : id of the concern element
  * @param[in] name (string)    : name of the desired scalar field
  * @return f_rank (integer) : rank of the corresponding scalar field
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : id of the concern body
  * @param[in] iblmty (int) : id of the concern element
  * @param[in] name (char[30]) : name of the scalar field
  * @return (int) : rank of the corresponding scalar field
  * @endcond
  */
  extern "C" int poroMAILx_GetMecaScalarFieldRank(int ibdyty, int blmty, char* name);

 /**
  * @fn int poroMAILx_GetMecaVectorFieldRank(int ibdyty, int iblmty, char* field_name);
  * @brief Get the rank of field of an element of a body from its name
  *
  * @cond PYDOC
  * python usage : f_rank = poroMAILx_GetMecaVectorFieldRank(ibdyty, iblmty, name)
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
  extern "C" int poroMAILx_GetMecaVectorFieldRank(int ibdyty, int blmty, char* name);

 /**
  * @fn int poroMAILx_GetTherScalarFieldRank(int ibdyty, int iblmty, char* field_name);
  * @brief Get the rank of field of an element of a body from its name
  *
  * @cond PYDOC
  * python usage : f_rank = poroMAILx_GetTherScalarFieldRank(ibdyty, iblmty, name)
  * @param[in] ibdyty (integer) : id of the concern body
  * @param[in] iblmty (integer) : id of the concern element
  * @param[in] name (string)    : name of the desired scalar field
  * @return f_rank (integer) : rank of the corresponding scalar field
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : id of the concern body
  * @param[in] iblmty (int) : id of the concern element
  * @param[in] name (char[30]) : name of the scalar field
  * @return (int) : rank of the corresponding scalar field
  * @endcond
  */
  extern "C" int poroMAILx_GetTherScalarFieldRank(int ibdyty, int blmty, char* name);

 /**
  * @fn int poroMAILx_GetTherVectorFieldRank(int ibdyty, int iblmty, char* field_name);
  * @brief Get the rank of field of an element of a body from its name
  *
  * @cond PYDOC
  * python usage : f_rank = poroMAILx_GetTherVectorFieldRank(ibdyty, iblmty, name)
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
  extern "C" int poroMAILx_GetTherVectorFieldRank(int ibdyty, int blmty, char* name);

 /**
  * @fn void poroMAILx_SetMecaVectorFieldByNode(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2)
  * @brief Update elementary fields through a nodal external field on a given body
  *
  * Use the form functions of the elements and input values to compute and store field values
  * at Gauss points.
  *
  * You need to declare this field in your MODELS.DAT
  *
  * @cond PYDOC
  * python usage : poroMAILx_SetFieldByNode(IdBody, f_rank, f)
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
  extern "C" void poroMAILx_SetMecaVectorFieldByNode(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void poroMAILx_SetMecaVectorFieldByElement(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2)
  * @brief Update elementary fields through a nodal external field on a given body
  *
  * Use the form functions of the elements and input values to compute and store field values
  * at Gauss points.
  *
  * You need to declare this field in your MODELS.DAT
  *
  * @cond PYDOC
  * python usage : poroMAILx_SetVectorFieldByElement(IdBody, f_rank, f)
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
  extern "C" void poroMAILx_SetMecaVectorFieldByElement(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void poroMAILx_SetTherVectorFieldByNode(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2)
  * @brief Update elementary fields through a nodal external field on a given body
  *
  * Use the form functions of the elements and input values to compute and store field values
  * at Gauss points.
  *
  * You need to declare this field in your MODELS.DAT
  *
  * @cond PYDOC
  * python usage : poroMAILx_SetFieldByNode(IdBody, f_rank, f)
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
  extern "C" void poroMAILx_SetTherVectorFieldByNode(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void poroMAILx_SetTherVectorFieldByElement(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2)
  * @brief Update elementary fields through a nodal external field on a given body
  *
  * Use the form functions of the elements and input values to compute and store field values
  * at Gauss points.
  *
  * You need to declare this field in your MODELS.DAT
  *
  * @cond PYDOC
  * python usage : poroMAILx_SetFieldByElement(IdBody, f_rank, f)
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
  extern "C" void poroMAILx_SetTherVectorFieldByElement(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void poroMAILx_LoadALE(int IdBody)
  * @brief Apply an ALE Formulation in Fluid zone
  *
  * @cond PYDOC
  * python usage : poroMAILx_LoadALE(IdBody)
  * @endcond
  *
  * @cond CDOC
  * @param[in] IdBody (int) : id of the concern body
  * @endcond
  */
  extern "C" void poroMAILx_LoadALE(int IdBody);

 /**
  * @fn void poroMAILx_PutBodyVector(char * cvalue1_c, int ivalue1, double * matrix_in, int dim1, int dim2)
  * @brief Set a vector of a given body
  *
  * @cond PYDOC
  * python usage : poroMAILx_PutBodyVector(datatype, ibdyty, matrix)
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
  * - "Xbeg_": cumulated displacements over time at beginning of time step
  * - "X____": cumulated displacements over time in computed configuration
  * - "Vbeg_": velocity at beginning of time step
  * - "V____": velocity in computed configuration
  * - "VbALE": fluid velocity at beginning of time step
  * - "V_ALE": fluid velocity at beginning of time step
  * - "Raux_": working array for reaction
  * - "Vfree": velocity free of contacts
  * - "Reac_": contact reaction force
  * - "Fext_": external forces
  * - "Fint_": internal forces
  * - "Qext_": external fluxes
  * - "Qint_": internal luxces
  * - "Pbeg_": pressure at beginning of time step
  * - "P____": pressure in computed configuration
  */
  extern "C" void poroMAILx_PutBodyVector(char * cvalue1_c, int ivalue1, double * matrix_in, int dim1, int dim2);

 /**
  * @fn double poroMAILx_ComputeResidueNorm(void)
  * @brief computes the norm of the residue
  *
  * @cond PYDOC
  * python usage : norm = poroMAILx_ComputeResidueNorm()
  *
  * @return norm (double) : Residue Norm
  * @endcond
  *
  * @cond CDOC
  * @return norm (double) : Residue Norm
  * @endcond
  */
  extern "C" double poroMAILx_ComputeResidueNorm(void);

 /**
  * @fn void poroMAILx_GetStress(int ivalue1, double** matrix_out, int* dim1, int* dim2, int ivalue4=0)
  * @brief Get a copy of a stress of a given body
  *
  * @cond PYDOC
  * Python usage : stress = poroMAILx_GetStress(ibdyty,required_field=0)
  * @param ibdyty (integer)          : rank of considered body
  * @param required_field (integer)  : required additional field
  * @return matrix_out (double 2D-array) : the desired stress
  * @endcond
  *
  * @cond CDOC
  * @param[in]  ivalue1 (int)         : id of considered body
  * @param[out] matrix_out (double**) : the desired stress
  * @param[out] dim1 (int*)           : first dimension of matrix_out
  * @param[out] dim2 (int*)           : second dimension of matrix_out
  * @param[in]  ivalue4 (int)         : required field
  * @endcond
  */
  extern "C" void poroMAILx_GetStress(int ivalue1, double** matrix_out, int* dim1, int* dim2, int ivalue4=0);

 /**
  * @fn void poroMAILx_GetStrain(int ivalue1, double** matrix_out, int* dim1, int* dim2, int ivalue4=0)
  * @brief Get a copy of a strain of a given body
  *
  * @cond PYDOC
  * Python usage : strain = poroMAILx_GetStrain(ibdyty, required_field=0)
  * @param ibdyty (integer)          : rank of considered body
  * @param required_field (integer)  : required additional field
  * @return strain (double 2D-array) : the desired strain
  * @endcond
  *
  *
  * @cond CDOC
  * @param[in]  ivalue1 (int)         : id of considered body
  * @param[out] matrix_out (double**) : the desired strain
  * @param[out] dim1 (int*)           : first dimension of matrix_out
  * @param[out] dim2 (int*)           : second dimension of matrix_out
  * @param[in]  ivalue4 (int)         : required field
  * @endcond
  */
  extern "C" void poroMAILx_GetStrain(int ivalue1, double** matrix_out, int* dim1, int* dim2, int ivalue4=0);

 /**
  * @fn void poroMAILx_ComputeContactDetectionConfiguration(void)
  * @brief compute the contact detection configuration
  *
  * @cond PYDOC
  * python usage : poroMAILx_ComputeContactDetectionConfiguration()
  * @endcond
 */
 extern "C" void poroMAILx_ComputeContactDetectionConfiguration(void);

 /**
  * @fn void poroMAILx_SetPreconAllBodies(void)
  * @brief ask for precomputation of the W matrix on support node dofs of contactors for all bodies. Assumes bulk behaviour is linear. 
  *    
  * @cond PYDOC
  * python usage : poroMAILx_SetPreconAllBodies()
  * @endcond
  */
  extern "C" void poroMAILx_SetPreconAllBodies(void);

 /**
  * @fn void poroMAILx_ComputePreconW(void)
  * @brief compute the precon W on precon bodies
  *
  * @cond PYDOC
  * python usage : poroMAILx_ComputePreconW()
  * @endcond
  */
  extern "C" void poroMAILx_ComputePreconW(void);

 /**
  * @fn int poroMAILx_GetNbPoroMAILx(void)
  * @brief Get the number of poroMAILx
  *
  * @cond PYDOC
  * python usage : nb_poroMAILx = poroMAILx_GetNbPoroMAILx()
  *
  * @return nb_poroMAILx (integer) : number of poroMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of poroMAILx
  * @endcond
  */
  extern "C" int poroMAILx_GetNbPoroMAILx(void);

 /**
  * @fn void poroMAILx_GetCoor(int idBody,  double ** matrix_out, int * dim1, int * dim2)
  * @brief return node coordinates of idBody
  *
  * @cond PYDOC
  * python usage : array = poroMAILx_GetCoor(idBody)
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
  extern "C" void poroMAILx_GetCoor(int idBody, double ** matrix_out, int * dim1, int * dim2);

 /**
  * @fn void poroMAILx_GetAll(int idBody,  double ** matrix_out, int * dim1, int * dim2)
  * @brief return poro mechanical data computed for idBody
  *
  * @cond PYDOC
  * python usage : array = poroMAILx_GetAll(idBody)
  * @param[in] IdBody (integer)     : id of the concerned body
  * @return array (double 2D-array) : poro mechanical data
  * @endcond
  *
  * @cond CDOC
  * @param[in]     IdBody (int)           : id of the concerned body
  * @param[in,out] matrix_out (double **) : xxx
  * @param[in]     dim1 (int *)  i        : matrix_out first dimension
  * @param[in]     dim2 (int *)           : matrix_out second dimension
  * @endcond
  */
  extern "C" void poroMAILx_GetAll(int idBody, double ** matrix_out, int * dim1, int * dim2);

 /**
  * @fn void poroMAILx_GetGrad(int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of a grad P of a given body
  *
  * @cond PYDOC
  * Python usage : grad = poroMAILx_GetGrad(ibdyty)
  * @param ibdyty (integer)        : rank of considered body
  * @return grad (double 2D-array) : the desired grad
  * @endcond
  *
  *
  * @cond CDOC
  * @param[in]  ivalue1 (int)         : id of considered body
  * @param[out] matrix_out (double**) : the desired grad
  * @param[out] dim1 (int*)           : first dimension of matrix_out
  * @param[out] dim2 (int*)           : second dimension of matrix_out
  * @endcond
  */
  extern "C" void poroMAILx_GetGrad(int ivalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void poroMAILx_GetFlux(int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of a Darcy Flux of a given body
  *
  * @cond PYDOC
  * Python usage : flux = poroMAILx_GetFlux(ibdyty)
  * @param ibdyty (integer)        : rank of considered body
  * @return flux (double 2D-array) : the desired flux
  * @endcond
  *
  *
  * @cond CDOC
  * @param[in]  ivalue1 (int)         : id of considered body
  * @param[out] matrix_out (double**) : the desired flux
  * @param[out] dim1 (int*)           : first dimension of matrix_out
  * @param[out] dim2 (int*)           : second dimension of matrix_out
  * @endcond
  */
  extern "C" void poroMAILx_GetFlux(int ivalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void poroMAILx_GetInternal(int idBody,  double ** matrix_out, int * dim1, int * dim2)
  * @brief return internal mechanical data computed for idBody
  *
  * @cond PYDOC
  * python usage : array = poroMAILx_GetInternal(idBody)
  * @param[in] IdBody (integer)     : id of the concerned body
  * @return array (double 2D-array) : mechanical internal data
  * @endcond
  *
  * @cond CDOC
  * @param[in]     IdBody (int)           : id of the concerned body
  * @param[in,out] matrix_out (double **) : matrix of internal data
  * @param[in]     dim1 (int *)  i        : matrix_out first dimension
  * @param[in]     dim2 (int *)           : matrix_out second dimension
  * @endcond
  */
  extern "C" void poroMAILx_GetInternal(int idBody, double ** matrix_out, int * dim1, int * dim2);

 /**
  * @fn void poroMAILx_GetConnectivity(int idBody,  int ** i4_vector, int * i4_size)
  * @brief return connectivity of idBody elements
  *
  * @cond PYDOC
  * python usage : vector = poroMAILx_GetConnectivity(idBody)
  * @param[in] IdBody (integer) : id of the concerned body
  * @return    vector (integer) : connectivity
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)      : id of the concerned body
  * @param[out] i4_vector (int**) : xxx
  * @param[out] i4_size (int*)    : dimension of i4_vector
  * @endcond
  */
  extern "C" void poroMAILx_GetConnectivity(int idBody, int ** i4_vector, int * i4_size );

 /**
  * @fn void poroMAILx_SetVlocyDrivenDof(int IdBody, int f_dof, int f_node, double f_value)
  * @brief Apply Drv Dof on a given body
  *
  * @cond PYDOC
  * python usage : poroMAILx_SetVlocyDrivenDof(IdBody, f_dof, f_node, f_value)
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
  extern "C" void poroMAILx_SetVlocyDrivenDof(int IdBody, int f_dof, int f_noce , double f_value);

 /**
  * @fn void poroMAILx_AddFieldLoad(int IdBody, double * rvector_in, int rlength_in)
  * @brief Add elementary load through a nodal external field on a given body
  *
  * @cond PYDOC
  * python usage : poroMAILx_AddFieldLoad(IdBody, Ideriv, f)
  * @param[in] IdBody (integer) : id of the concern body
  * @param[in] f (double array) : value of the field
  * @endcond
  *
  * @cond CDOC
  * @param[in] IdBody (int)               : id of the concern body
  * @param[in] vector_in (double[length]) : value of the field
  * @param[in] length (int)               : size of the field
  * @endcond
  */
  extern "C" void poroMAILx_AddFieldLoad(int IdBody, double * rvector_in, int rlength_in);

 /**
  * @fn void poroMAILx_WriteOutDof(void)
  * @brief Write ascii DOF.OUT file. Can be activate only each N step
  *
  * @cond PYDOC
  * python usage : poroMAILx_WriteOutDof()
  * @endcond
  */
  extern "C" void poroMAILx_WriteOutDof(void);

 /**
  * @fn void poroMAILx_PostModels(void)
  * @brief load from MAILx and models for post
  *
  * @cond PYDOC
  * python usage : poroMAILx_PostModels()
  * @endcond
  */
  extern "C" void poroMAILx_PostModels(void);

/**
  * @fn void poroMAILx_CleanMemory(void)
  * @brief Free all memory allocated within poroMAILx module
  *
  * @cond PYDOC
  * python usage : poroMAILx_CleanMemory()
  * @endcond
  */
  extern "C" void poroMAILx_CleanMemory(void);

 /**
  * @fn void poroMAILx_CheckProperties(void)
  * @brief check if model and material are matching ; set material parameter if external model
  *
  * @cond PYDOC
  * python usage : poroMAILx_CheckProperties()
  * @endcond
  */
  extern "C" int poroMAILx_CheckProperties(void);

 /**
  * @fn void poroMAILx_GetNbGpByElem(char** string_vector, int* vector_size, int* string_size, , int** i4_vector, int* i4_size, int** i4_vector_2, int* i4_size_2)
  * @brief Get the list of finite elements for porox models and the associated number of Gauss Points for MECA and THER physics.
  *
  * Here memory is allocated within lmgc90 so that the pointer can be freely
  * modified by third parties without nasty effect on lmgc90 functioning.
  *
  * @cond PYDOC
  * python usage : names, meca_nb, ther_nb = poroMAILx_GetNbGpByElem()
  * @return names   (string list) : list of the finite elements
            meca_nb (integer list): list of the number of Gauss Points for MECA
            ther_nb (integer list): list of the number of Gauss Points for THER
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : list of finite elements
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @param[out] i4_vector (int**) : reference on the integer array holding the list of number of Gauss Points for MECA
  * @param[out] i4_size (int*) : reference on the size of the array referenced by i4_vector
  * @param[out] i4_vector_2 (int**) : reference on the integer array holding the list of number of Gauss Points for THER
  * @param[out] i4_size_2 (int*) : reference on the size of the array referenced by i4_vector_2
  * @endcond
  */
  extern "C" void poroMAILx_GetNbGpByElem(char** string_vector, int* vector_size, int* string_size, int** i4_vector, int* i4_size, int** i4_vector_2, int* i4_size_2);

#endif /* wrap_poroMAILx_h */
