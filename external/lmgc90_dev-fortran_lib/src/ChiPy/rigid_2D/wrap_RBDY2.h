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

#ifndef wrap_RBDY2_h
#define wrap_RBDY2_h

 /**
  * @fn void RBDY2_PutBodyInvMass(int ivalue1, double * rvector_in, int rlength_in)
  * @brief Set inv mass diagonal matrix of a given body. Overwrites the computed values
  *
  * @cond PYDOC
  * usage : RBDY2_PutBodyInvMass(ibdyty, inv_mass)
  * @param[in] ibdyty (integer)        : rank of RBDY2
  * @param[in] inv_mass (double array) : inv_mass of RBDY2 (size 3)
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)              : rank of RBDY2
  * @param[in] vector_in (double[length]) : inv_mass of RBDY2
  * @param[in] length (int)               : size of vector_in (must be 3)
  * @endcond
  */
  extern "C" void RBDY2_PutBodyInvMass(int ivalue1, double * rvector_in, int rlength_in);

 /**
  * @fn void RBDY2_PutBodyPreconW(int ivalue1, int ivalue2, double * rvector_in, int rlength_in)
  * @brief Put preconW of a given body
  *
  * @cond PYDOC
  * usage : RBDY2_PutBodyPreconW(ibdyty, idof, W)
  * @param[in] ibdyty (integer) : rank of RBDY2
  * @param[in] idof (integer) : corresponding dof to set
  * @param[in] W (double array) : 
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)              : rank of RBDY2
  * @param[in] ivalue2 (int)              : corresponding dof to set
  * @param[in] vector_in (double[length]) :  
  * @param[in] length (int)               : size of vector_in
  * @endcond
  */
  extern "C" void RBDY2_PutBodyPreconW(int ivalue1, int ivalue2, double * rvector_in, int rlength_in);

 /**
  * @fn void RBDY2_PutBodyVector(char * cvalue1, int ivalue1, double * rvector_in, int rlength_in)
  * @brief Set a vector of a given body
  *
  * Uses copy, and in case fo Fext, the operation is not
  * just setting but adding
  *
  * @cond PYDOC
  * usage : RBDY2_PutBodyVector(datatype, ibdyty, vector)
  * @param[in] datatype (string of size 5) : the vector to set
  * @param[in] ibdyty (integer)            : rank of body
  * @param[in] vector (double array)       : the new value \n
  * @endcond
  *
  * @cond CDOC
  * @param[in] cvalue1 (char[5])    : the vector to set
  * @param[in] ivalue1 (int)        : id of considered body
  * @param[in] vector_in (double *) : the new vector
  * @param[in] length (int)         : size of vector_in
  * @endcond
  *
  * Possible values for datatype field are: 
  * - "Coor0": reference coordinates
  * - "Coorb": coordinates at beginning of time step
  * - "Coor_": coordinates in computed configuration
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
  *
  */
  extern "C" void RBDY2_PutBodyVector(char * cvalue1, int ivalue1, double * rvector_in, int rlength_in);

 /**
  * @fn void RBDY2_PutAllBodyVector(char * cvalue1, double* matrix_in, int dim1, int dim2)
  * @brief Put an array of a vector of all RBDY2 bodies (visible and invisible)
  *
  * @cond PYDOC
  * python usage : RBDY2_PutAllBodyVector(datatype, matrix)
  * @param[in] datatype (string [5]) : the vector to set
  * @param[in] matrix (double array) : input matrix
  * @endcond
  *
  * @cond CDOC
  * @param[in]  cvalue1 (char[5])   : the vector to set
  * @param[out] matrix_in (double*) : the in matrix
  * @param[out] dim1 (int*)         : the first dimension of matrix_in
  * @param[out] dim2 (int*)         : the second dimension of matrix_in
  * @endcond
  *
  * Possible values for datatype field are:
  * ... see RBDY2_PutBodyVector
  */
  extern "C" void RBDY2_PutAllBodyVector(char * cvalue1, double* matrix_in, int dim1, int dim2);

 /**
  * @fn void RBDY2_GetBodyVector(char * cvalue1, int ivalue1, double** r8_vector, int* r8_size )
  * @brief Get a copy of a vector of a given RBDY2 body
  *
  * @cond PYDOC
  * usage : vector = RBDY2_GetBodyVector(datatype, ibdyty)
  * @param datatype (string of size 5) : the vector to get
  * @param ibdyty (integer)            : rank of considered body
  * @param vector (double array)       : the desired vector
  * @endcond
  *
  * @cond CDOC
  * @param[in]  cvalue1 (char[5])    : the vector to get
  * @param[in]  ivalue1 (int)        : rank of considered body
  * @param[out] r8_vector (double**) : the vector to get
  * @param[out] r8_size (int*)       : the size of r8_vector
  * @endcond
  *
  * Possible values for datatype field are: 
  * - "Coor0": reference coordinates
  * - "Coorb": coordinates at beginning of time step
  * - "Coorm": coordinates in detection configuration
  * - "Coor_": coordinates in computed configuration
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
  extern "C" void RBDY2_GetBodyVector(char * cvalue1, int ivalue1, double** r8_vector, int* r8_size);

 /**
  * @fn void RBDY2_GetAllBodyVector(char * cvalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get an array of a vector of all RBDY2 bodies (visible and invisible)
  *
  * @cond PYDOC
  * python usage : matrix = RBDY2_GetBodyVector(datatype, ibdyty)
  * @param[in] datatype (string [5]) : the vector to get
  * @return    matrix (double array) : output matrix
  * @endcond
  *
  * @cond CDOC
  * @param[in]  cvalue1 (char[5])     : the vector to get
  * @param[out] matrix_out (double**) : the out vector
  * @param[out] dim1 (int*)           : the first dimension of matrix_out
  * @param[out] dim2 (int*)           : the second dimension of matrix_out
  * @endcond
  *
  * Possible values for datatype field are:
  * ... see RBDY2_GetBodyVector
  */
  extern "C" void RBDY2_GetAllBodyVector(char * cvalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void RBDY2_GetPtrBodyVector(char * cvalue1, int ivalue1, double** pointer_out, int* length)
  * @brief Get a pointer on a vector of a given RBDY2 body
  *
  * @cond PYDOC
  * usage : vector_ptr = RBDY2_GetPtrVector(datatype, ibdyty)
  * @param[in] datatype (string of size 5) : the vector to get
  * @param[in] ibdyty (integer)            : rank of considered body
  * @param     vector_ptr (double array)   : reference on the desired vector viewed as a numpy array
  * @endcond
  *
  * @cond CDOC
  * @param[in]  cvalue1 (char[5])       : the vector to get
  * @param[in]  ivalue1 (int)           : id of considered body
  * @param[out] pointer_out (double **) : reference on the out vector
  * @param[out] length (int*)           : reference on the length of the out vector
  * @endcond
  *
  * Possible values for datatype field are: 
  * - "Coor0": reference coordinates
  * - "Xbeg_": cumulated displacements over time at beginning of time step
  * - "X____": cumulated displacements over time in computed configuration
  * - "Vbeg_": velocity at beginning of time step
  * - "V____": velocity in computed configuration
  * - "Vaux_": working array for velocity
  * - "Ireac": contact impulse
  * - "Iaux_": working array for impulste
  * - "Fext_": external forces
  *
  */
  extern "C" void RBDY2_GetPtrBodyVector(char * cvalue1, int ivalue1, double** pointer_out, int* length);

 /**
  * @fn double RBDY2_GetBodyInertia(int ivalue1)
  * @brief Get the inertia of a given RBDY2 body
  *
  * @cond PYDOC
  * usage : inertia = RBDY2_GetBodyInertia(ibdyty)
  * @param[in] ibdyty  (integer) : rank of considered body
  * @param     inertia (double)  : the inertia of desired body
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int)     : rank of considered body
  * @return    inertia (double) : the inertia
  * @endcond
  */
  extern "C" double RBDY2_GetBodyInertia(int ivalue1);

 /**
  * @fn void RBDY2_GetAllInertia(double** r8_vector, int* r8_size)
  * @brief Get the inertia of a all RBDY2 body
  *
  * @cond PYDOC
  * usage : inertia = RBDY2_GetAllInertia()
  * @param inertia (double array): the inertia of all bodies
  * @endcond
  *
  * @cond CDOC
  * @param[out] r8_vector (double**) : out vector
  * @param[out] r8_size (int*)       : size of out vector
  * @endcond
  */
  extern "C" void RBDY2_GetAllInertia(double** r8_vector, int* r8_size);

 /**
  * @fn void RBDY2_IncrementStep(void)
  * @brief increment values at the current time step (prediction)
  *
  * @cond PYDOC
  * usage : RBDY2_IncrementStep()
  * @endcond
  *
  */
  extern "C" void RBDY2_IncrementStep(void);

 /**
  * @fn void RBDY2_SetVlocyDrivenDof(int ibdyty, int idrvdof, double value)
  * @brief Override the value of an existing velocity driven dof ; use after IncrementStep 
  *
  * @cond PYDOC
  * usage : RBDY2_SetVlocyDrivenDof(ibdyty, idrvdof, value)
  * @param[in] ibdyty  (integer) : rank of considered
  * @param[in] idrvdof (integer) : index of velocity driven dof to set
  * @param[in] value   (real)    : new value of the velocity driven dof
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty  (int)    : rank of considered
  * @param[in] idrvdof (int)    : index of velocity driven dof to set
  * @param[in] value   (double) : new value of the velocity driven dof
  * @endcond
  */
  extern "C" void RBDY2_SetVlocyDrivenDof(int ibdyty, int idrvdof, double value);

 /**
  * @fn void RBDY2_ComputeDof(void)
  * @brief Compute current DOF of bodies in container
  *
  * @cond PYDOC
  * usage : RBDY2_ComputeDof()
  * @endcond
  *
  */
  extern "C" void RBDY2_ComputeDof(void);

 /**
  * @fn void RBDY2_UpdateDof(void)
  * @brief set current DOF as initial DOF of bodies in container
  *
  * @cond PYDOC
  * usage : RBDY2_UpdateDof()
  * @endcond
  *
  */
  extern "C" void RBDY2_UpdateDof(void);

 /**
  * @fn void RBDY2_ComputeFreeVelocity(void)
  * @brief Compute free velocity of bodies in container
  *
  * @cond PYDOC
  * usage : RBDY2_ComputeFreeVelocity()
  * @endcond
  *
  */
  extern "C" void RBDY2_ComputeFreeVelocity(void);

 /**
  * @fn void RBDY2_ComputeFext(void)
  * @brief  Compute impulse of external forces of bodies in container
  *
  * @cond PYDOC
  * usage : RBDY2_ComputeFext()
  * @endcond
  *
  */
  extern "C" void RBDY2_ComputeFext(void);

 /**
  * @fn void RBDY2_ComputeBulk(void)
  * @brief  Compute impulse of internal forces of bodies in container
  *
  * @cond PYDOC
  * usage : RBDY2_ComputeBulk()
  * @endcond
  *
  */
  extern "C" void RBDY2_ComputeBulk(void);

 /**
  * @fn bool RBDY2_CheckEquilibriumState(void)
  * @brief  check if all the RBDY2 rich an equilibrium state (velocity is almost equal to zero)
  *
  * @cond PYDOC
  * usage : isBalanced = RBDY2_CheckEquilibriumState()
  *
  * @return isBalanced (boolean) : True if in equilibrium state 
  * @endcond
  *
  * @cond CDOC
  * @return isBalanced (bool) : True if in equilibrium state 
  * @endcond
  */
  extern "C" bool RBDY2_CheckEquilibriumState(void);

 /**
  * @fn void RBDY2_GhostToInvisible(void)
  * @brief set bodies with ghost behaviour nickname as invisible
  *
  * @cond PYDOC
  * usage : RBDY2_GhostToInvisible()
  * @endcond
  *
  */
  extern "C" void RBDY2_GhostToInvisible(void);


 /**
  * @fn void RBDY2_FatalDamping(int * ivector_in=nullptr, int ilength_in=0)
  * @brief Nullify body current and initial velocities of a list of bodies
  *
  * This keyword must be between the ComputeDof and UpdateDof ones.
  *
  * @cond PYDOC
  * usage : RBDY2_FatalDamping(i_list)
  * @param[in] i_list (list of integer) : list of bodies to reset current velocity
  *            if omitted works on all objetcs
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of RBDY2
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void RBDY2_FatalDamping(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void RBDY2_PartialDamping(int nb_steps, double Vmax)
  * @brief Limit body velocity to Vmax value
  *
  * @cond PYDOC
  * usage : RBDY2_PartialDamping(nb_steps, Vmax)
  * @param[in] nb_steps (integer) : periodicity
  * @parma[in] Vmax (double)      : Vmax
  * @endcond
  *
  * @cond CDOC
  * @param[in] nb_steps (int) :
  * @parma[in] Vmax (double)  : Vmax
  * @endcond
  */
  extern "C" void RBDY2_PartialDamping(int nb_steps, double Vmax);

 /**
  * @fn void RBDY2_WriteLastDof(void)
  * @brief Write ascii DOF.LAST file
  *
  * @cond PYDOC
  * usage : RBDY2_WriteLastDof()
  * @endcond
  *
  */
  extern "C" void RBDY2_WriteLastDof(void);

 /**
  * @fn void RBDY2_WriteOutDof(int ivalue1=0, int ivalue2=0)
  * @brief Write ascii DOF.OUT file. Can be activated only each N steps
  *
  * @cond PYDOC
  * usage : RBDY2_WriteOutDof(ivalue1=0, ivalue2=0)
  * @param[in] ivalue1 (integer) : first body
  * @param[in] ivalue2 (integer) : last body
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int) :
  * @param[in] ivalue2 (int) :
  * @endcond
  * If 0 for ivalue1 and ivalue2, dofs of all bodies are written.
  */
  extern "C" void RBDY2_WriteOutDof(int ivalue1=0, int ivalue2=0);

 /**
  * @fn void RBDY2_DisplayOutDof(void)
  * @brief Display bodies degrees of freedom
  *
  * @cond PYDOC
  * usage : RBDY2_DisplayOutDof()
  * @endcond
  */
  extern "C" void RBDY2_DisplayOutDof(void);

 /**
  * @fn void RBDY2_WriteLastRnod(void)
  * @brief Write ascii Rnod.LAST file
  *
  * @cond PYDOC
  * usage : RBDY2_WriteLastRnod()
  * @endcond
  *
  */
  extern "C" void RBDY2_WriteLastRnod(void);

 /**
  * @fn void RBDY2_WriteOutRnod(void)
  * @brief Write ascii Rnod.OUT file. Can be activated only each N steps
  *
  * @cond PYDOC
  * usage : RBDY2_WriteOutRnod()
  * @endcond
  */
  extern "C" void RBDY2_WriteOutRnod(void);

 /**
  * @fn void RBDY2_DisplayOutRnod(void)
  * @brief display body forces
  *
  * @cond PYDOC
  * usage : RBDY2_DisplayOutRnod()
  * @endcond
  */
  extern "C" void RBDY2_DisplayOutRnod(void);

 /**
  * @fn void RBDY2_WriteBodies(void)
  * @brief Write BODIES.OUT file
  *
  * @cond PYDOC
  * usage : RBDY2_WriteBodies()
  * @endcond
  *
  */
  extern "C" void RBDY2_WriteBodies(void);

 /**
  * @fn void RBDY2_WriteBodies(void)
  * @brief Write DRV_DOF.OUT file
  *
  * @cond PYDOC
  * usage : RBDY2_WriteBodies()
  * @endcond
  *
  */
  extern "C" void RBDY2_WriteDrivenDof(void);

 /**
  * @fn void RBDY2_ReadBodies(void)
  * @brief Read BODIES.DAT file
  *
  * @cond PYDOC
  * usage : RBDY2_ReadBodies()
  * @endcond
  *
  * \n Initialize existing_entities variable in RBDY2\n 
  * Adds the number of found bodies to entity\n 
  *
  */
  extern "C" void RBDY2_ReadBodies(void);

 /**
  * @fn void RBDY2_ReadIniDof(int num=0)
  * @brief Read DOF file
  *
  * If num <= 0 : DATBOX/DOF.INI file is read
  * Else : OUTBOX/DOF.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniDof
  * last call
  *
  * @cond PYDOC
  * usage : RBDY2_ReadIniDof(num=0)
  * @param[in] num (integer) : which DOF file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which DOF file to read
  * @endcond
  *
  */
  extern "C" void RBDY2_ReadIniDof(int num=0);

 /**
  * @fn void RBDY2_ReadDrivenDof(void)
  * @brief Read DRV_DOF.DAT file
  *
  * @cond PYDOC
  * usage : RBDY2_ReadDrivenDof()
  * @endcond
  *
  */
  extern "C" void RBDY2_ReadDrivenDof(void);

 /**
  * @fn void RBDY2_LoadBehaviours(void)
  * @brief Load bulk behaviour id from bulk_behav module
  *
  * @cond PYDOC
  * usage : RBDY2_LoadBehaviours()
  * @endcond
  *
  */
  extern "C" void RBDY2_LoadBehaviours(void);

 /**
  * @fn void RBDY2_MP_LoadBehaviours(double disper, char * cflag)
  * @brief Load extra physical behaviour read in BULK_BEHAV.DAT file
  *
  * Must be used with THERMO_RIGID ELECTRO_RIGID THERMO_ELECTRO_RIGID behaviours
  *
  * @cond PYDOC
  * usage : RBDY2_MP_LoadBehaviours(disper)
  * @param[in] disper (double) : dispersion variable
  * @endcond
  *
  * @cond CDOC
  * @param[in] disper (double) : dispersion variable
  * @endcond
  *
  */
extern "C" void RBDY2_MP_LoadBehaviours(double disper, char * cflag);

 /**
  * @fn void RBDY2_UpdateWSvsT()
  * @brief update surface energy with temperature
  *
  * Must be used with THERMO_RIGID ELECTRO_RIGID THERMO_ELECTRO_RIGID behaviours
  *
  * @cond PYDOC
  * usage : RBDY2_UpdateWSvsT()
  * @endcond
  *
  */
  extern "C" void RBDY2_UpdateWSvsT(void);

 /**
  * @fn void RBDY2_UpdateWSvsTime()
  * @brief update surface energy with time
  *
  * Must be used with THERMO_RIGID ELECTRO_RIGID THERMO_ELECTRO_RIGID behaviours
  *
  * @cond PYDOC
  * usage : RBDY2_UpdateWSvsTime()
  * @endcond
  *
  */
  extern "C" void RBDY2_UpdateWSvsTime(void);

 /**
  * @fn void RBDY2_ComputeMass(void)
  * @brief Compute mass and inertia of bodies
  *
  * @cond PYDOC
  * usage : RBDY2_ComputeMass()
  * @endcond
  *
  */
  extern "C" void RBDY2_ComputeMass(void);

 /**
  * @fn void RBDY2_SetPeriodicCondition(double periode)
  * @brief define the space is X periodic [0,periode]
  *
  * The X variable reaches a value between 0 and periode
  *
  * @cond PYDOC
  * usage : RBDY2_SetPeriodicCondition(periode)
  * @param[in] period (double) : periode
  * @endcond
  *
  * @cond CDOC
  * @param[in] period (double) : periode
  * @endcond
  *
  */
  extern "C" void RBDY2_SetPeriodicCondition(double periode);

 /**
  * @fn void RBDY2_NullifyDisplacements(void)
  * @brief Set displacements equal to 0
  *
  * @cond PYDOC
  * usage : RBDY2_NullifyDisplacements()
  * @endcond
  *
  */
  extern "C" void RBDY2_NullifyDisplacements(void);

 /**
  * @fn void RBDY2_NullifyVelocities(void)
  * @brief Set velocity to 0
  *
  * @cond PYDOC
  * usage : RBDY2_NullifyVelocities()
  * @endcond
  *
  */
  extern "C" void RBDY2_NullifyVelocities(void);

 /**
  * @fn void RBDY2_MembraneBiaxialLoading( int down, int up, double thickness, double stress)
  * @brief Biaxial load of a sample using pseudo membrane 
  *
  * @cond PYDOC
  * usage : RBDY2_MembraneBiaxialLoading(down, up, thickness, stress)
  * @param[in] down      (integer) : rank of the lower body
  * @param[in] up        (integer) : rank of the upper body
  * @param[in] thickness (double)  : thickness of the membrane
  * @param[in] stress    (double)  : pressure on the membrane
  * @endcond
  *
  * @cond CDOC
  * @param[in] down      (int)    : rank of the lower body
  * @param[in] up        (int)    : rank of the upper body
  * @param[in] thickness (double) : thickness of the membrane
  * @param[in] stress    (double) : pressure on the membrane
  * @endcond
  */
  extern "C" void RBDY2_MembraneBiaxialLoading( int down,int up, double thickness, double stress);

 /**
  * @fn void RBDY2_BiaxialLoading(int down , double f_down, int right, double f_right, int up, double f_up,  int left, double f_left)
  * @brief Biaxial load of a sample using a rigid box 
  *
  * @cond PYDOC
  * usage : RBDY2_BiaxialLoading(down,f_down,right,f_right,up,f_up,left,f_left)
  * @param[in] down      (integer)      : rank of the lower body
  * @param[in] f_down    (double)       : pressure on the lower body
  * @param[in] right     (integer)      : rank of the right body
  * @param[in] f_right   (double)       : pressure on the right body
  * @param[in] up        (integer)      : rank of the upper body
  * @param[in] f_up      (double)       : pressure on the upper body
  * @param[in] left      (integer)      : rank of the left body
  * @param[in] f_left    (double)       : pressure on the left body
  * @endcond
  *
  * @cond CDOC
  * @param[in] down      (integer)      : rank of the lower body
  * @param[in] f_down    (double)       : pressure on the lower body
  * @param[in] right     (integer)      : rank of the right body
  * @param[in] f_right   (double)       : pressure on the right body
  * @param[in] up        (integer)      : rank of the upper body
  * @param[in] f_up      (double)       : pressure on the upper body
  * @param[in] left      (integer)      : rank of the left body
  * @param[in] f_left    (double)       : pressure on the left body
  * @endcond
  */
extern "C" void RBDY2_BiaxialLoading(int down , double f_down, int right, double f_right, int up, double f_up,  int left, double f_left);


 /**
  * @fn void RBDY2_SetYminBoundary(double rvalue1)
  * @brief define the boundary of command CHECK_OUT_OF_BOUNDS
  *
  * @cond PYDOC
  * usage : RBDY2_SetYminBoundary(inf_boundary)
  * @param[in] inf_boundary (double) : inferior boundary value
  * @endcond
  *
  * @cond CDOC
  * @param[in] rvalue1 (double) : inferior boundary value
  * @endcond
  */
  extern "C" void RBDY2_SetYminBoundary(double rvalue1);

 /**
  * @fn void RBDY2_SetYmaxBoundary(double rvalue1)
  * @brief define the boundary of command CHECK_OUT_OF_BOUNDS
  *
  * @cond PYDOC
  * usage : RBDY2_SetYmaxBoundary(up_boundary)
  * @param[in] up_boundary (double) : superior boundary value
  * @endcond
  *
  * @cond CDOC
  * @param[in] rvalue1 (double) : superior boundary value
  * @endcond
  */
  extern "C" void RBDY2_SetYmaxBoundary(double rvalue1);

 /**
  * @fn void RBDY2_SetXminBoundary(double rvalue1)
  * @brief define the boundary of command CHECK_OUT_OF_BOUNDS
  *
  * @cond PYDOC
  * usage : RBDY2_SetXminBoundary(left_boundary)
  * @param[in] left_boundary (double) : left boundary value
  * @endcond
  *
  * @cond CDOC
  * @param[in] rvalue1 (double) : left boundary value
  * @endcond
  */
  extern "C" void RBDY2_SetXminBoundary(double rvalue1);

 /**
  * @fn void RBDY2_SetXmaxBoundary(double rvalue1)
  * @brief define the boundary of command CHECK_OUT_OF_BOUNDS
  *
  * @cond PYDOC
  * usage : RBDY2_SetXmaxBoundary(right_boundary)
  * @param[in] right_boundary (double) : right boundary value
  * @endcond
  *
  * @cond CDOC
  * @param[in] rvalue1 (double) : right boundary value
  * @endcond
  */
  extern "C" void RBDY2_SetXmaxBoundary(double rvalue1);

 /**
  * @fn void RBDY2_SetEquilibriumNorm(char * norm_type , double tolerance)
  * @brief Initialization of data for the equilibrium state check
  *
  * You must precise the type of check test :
  * - Qvlcy : quadratic norm velocy
  * - Mvlcy : maximum   norm velocy
  *
  * @cond PYDOC
  * usage : RBDY2_CheckEquilibrium(norm_type , tolerance)
  * @param[in] norm_type (string of size 5) : norm type use for the equilibrium check
  * @param[in] tolerance (double)           : norm tolerance
  * @endcond
  *
  * @cond CDOC
  * @param[in] norm_type (char[5]) : norm type use for the equilibrium check
  * @param[in] tolerance (double)  : norm tolerance
  * @endcond
  *
  */
  extern "C" void RBDY2_SetEquilibriumNorm(char * norm_type, double tolerance);

 /**
  * @fn void RBDY2_AddDof2InBodies(void)
  * @brief Create a new BODIES.OUT file as combination of the last one and of the last DOF.OUT file
  *
  * @cond PYDOC
  * usage : RBDY2_AddDof2InBodies()
  * @endcond
  *
  */
  extern "C" void RBDY2_AddDof2InBodies(void);

 /**
  * @fn void RBDY2_InitFreeBoundary(double xmin, double xmax, double radius)
  * @brief 
  *
  * @cond PYDOC
  * usage : RBDY2_InitFreeBoundary(xmin, xmax, radius)
  * @param[in] xmin (double)   :
  * @param[in] xmax (double)   :
  * @param[in] radius (double) :
  * @endcond
  *
  * @cond CDOC
  * @param[in] xmin (double)   :
  * @param[in] xmax (double)   :
  * @param[in] radius (double) :
  * @endcond
  */
  extern "C" void RBDY2_InitFreeBoundary(double xmin, double xmax, double radius);

 /**
  * @fn void RBDY2_UpdateThermalStrain(void)
  * @brief 
  *
  * @cond PYDOC
  * usage : RBDY2_UpdateThermalStrain()
  * @endcond
  *
  */
  extern "C" void RBDY2_UpdateThermalStrain(void);

 /**
  * @fn int RBDY2_GetNbRBDY2(void)
  * @brief Get the number of RBDY2
  *
  * @cond PYDOC
  * usage : nb_rbdy2 = RBDY2_GetNbRBDY2()
  * @param nb_rbdy2 (integer) : number of RBDY2 in container
  * @endcond
  *
  * @cond CDOC
  * @return nb_rbdy2 (int) : number of RBDY2 in container
  * @endcond
  */
  extern "C" int RBDY2_GetNbRBDY2(void);

 /**
  * @fn double RBDY2_GetBodyArea(int ibdyty)
  * @brief Get the area (2D volume equivalent) of a given body
  *
  * @cond PYDOC
  * usage : area = GetBodyArea(ibdyty)
  * @param[in] ibdyty (integer) : rank of the body
  * @param     area   (double)  : area
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int)  : rank of the body
  * @return    area (double) : area
  * @endcond
  */
  extern "C" double RBDY2_GetBodyArea(int ibdyty);

 /**
  * @fn void RBDY2_GetAllArea(double** r8_vector, int* r8_size)
  * @brief Get the area of a all body (visible and invisible)
  *
  * @cond PYDOC
  * python usage : area = RBDY2_GetAllArea()
  * @return    area (double array) : masses of all RBDY2
  * @endcond
  *
  * @cond CDOC
  * @param[out] r8_vector (double**) : the output vector
  * @param[out] r8_size (int*)       : the size of output vector
  * @endcond
  */
  extern "C" void RBDY2_GetAllArea(double** r8_vector, int* r8_size);

 /**
  * @fn bool RBDY2_ComputePartialEquilibriumState(double abs_min, double abs_max, double ARGOUT_ARRAY1[2])
  * @brief Compute norms used to check if a part of the sample is in a equilibrium state
  *
  * Compute norms used to test if there is an equilibrium state between abs_min and abs_max
  *
  * Usefull in case of silos to access norms used to decide if the arch research must be activated
  *
  * @cond PYDOC
  * usage : Qnorm, Mnorm = RBDY2_CheckPartialEquilibriumState(abs_min, abs_max)
  * @param[in] abs_min (double)                   : min abscisse of sub domaine tested
  * @param[in] abs_max (double)                   : max abscisse of sub domaine tested
  * @param[out] Qnorm (double)                    : quadratric norm of the velocities of the grains
  * @param[out] Mnorm (double)                    : quadratric norm of the velocities of the grains
  * @endcond
  *
  * @cond CDOC
  * @param[in] abs_min (double)                : min abscisse of sub domaine tested
  * @param[in] abs_max (double)                : max abscisse of sub domaine tested
  * @param[out] Qnorm (double *)               : quadratric norm of the velocities of the grains
  * @param[out] Mnorm (double *)               : quadratric norm of the velocities of the grains
  * @endcond
  *
  */
  extern "C" void RBDY2_ComputePartialEquilibriumState(double abs_min, double abs_max, double ARGOUT_ARRAY1[2]);

 /**
  * @fn bool RBDY2_CheckPartialEquilibriumState(double abs_min, double abs_max)
  * @brief Check if a part of the sample is in a equilibrium state
  *
  * Test if there is an equilibrium state between abs_min and abs_max
  *
  * Usefull in case of silos to decide if the arch research must be activated
  *
  * @cond PYDOC
  * usage : isPartiallyEquilibriumed = RBDY2_CheckPartialEquilibriumState(abs_min, abs_max)
  * @param[in] abs_min (double)                   : min abscisse of sub domaine tested
  * @param[in] abs_max (double)                   : max abscisse of sub domaine tested
  * @param     isPartiallyEquilibriumed (boolean) : true if at partial equlibrium state, else false
  * @endcond
  *
  * @cond CDOC
  * @param[in] abs_min (double)                : min abscisse of sub domaine tested
  * @param[in] abs_max (double)                : max abscisse of sub domaine tested
  * @return    isPartiallyEquilibriumed (bool) : true if at partial equlibrium state, else false
  * @endcond
  *
  */
  extern "C" bool RBDY2_CheckPartialEquilibriumState(double abs_min, double abs_max);

 /**
  * @fn void RBDY2_SetBodiesInvisible(int * ivector_in, int ilength_in)
  * @brief Set a list of body to invisible state
  *
  * @cond PYDOC
  * usage : RBDY2_SetBodiesInvisible(list_bdy)
  * @param[in] list_bdy (integer array) : list of rank of bodies of the container
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int[length]) : list of rank of bodies of the container
  * @param[in] lenght (int)            : size of vector_in
  * @endcond
  */
  extern "C" void RBDY2_SetBodiesInvisible(int * ivector_in, int ilength_in);

 /**
  * @fn int RBDY2_IsVisible(int ibdyty)
  * @brief return if a body visible
  *
  * @cond PYDOC
  * usage : visible = RBDY2_IsVisible(ibdyty)
  * @param[in] ibdyty (integer)  : rank of body
  * @param     visible (integer) : 1 if body is visible, 0 else
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int)  : rank of body
  * @return    visible (int) : 1 if body is visible, 0 else
  * @endcond
  */
  extern "C" int RBDY2_IsVisible(int ibdyty);

 /**
  * @fn double RBDY2_GetBodyMass(int ibdyty)
  * @brief Get the mass of a body
  *
  * @cond PYDOC
  * usage : mass = RBDY2_GetBodyMass(ibdyty)
  * @param[in] ibdyty (integer) : rank of desired body
  * @param     mass (double)    : mass of body
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int)   : rank of desired body
  * @return     mass (double) : mass of body
  * @endcond
  */
  extern "C" double RBDY2_GetBodyMass(int ibdyty);

 /**
  * @fn void RBDY2_GetAllMass(double** r8_vector, int* r8_size)
  * @brief Get the mass of a all body (visible and invisible)
  *
  * @cond PYDOC
  * python usage : masses = RBDY2_GetAllMass()
  * @return    masses (double array) : masses of all RBDY2
  * @endcond
  *
  * @cond CDOC
  * @param[out] r8_vector (double**) : the output vector
  * @param[out] r8_size (int*)       : the size of output vector
  * @endcond
  */
  extern "C" void RBDY2_GetAllMass(double** r8_vector, int* r8_size);

 /**
  * @fn void RBDY2_CompCoor(void)
  * @brief Compute the position of bodies
  *
  * @cond PYDOC
  * usage : RBDY2_CompCoor()
  * @endcond
  *
  */
  extern "C" void RBDY2_CompCoor(void);
  
 /**
  * @fn double RBDY2_GetBodyDensity(int ibdyty)
  * @brief Get the density of a body
  *
  * @cond PYDOC
  * usage density = RBDY2_GetBodyDensity(ibdyty)
  * @param[in] ibdyty (integer) : rank of the RBDY2 in container
  * @param     density (double) : density of the RBDY2
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (in)      : rank of the RBDY2 in container
  * @return    density (double) : density of the RBDY2
  * @endcond
  */
  extern "C" double RBDY2_GetBodyDensity(int ibdyty);
  
 /**
  * @fn int RBDY2_GetNbContactor(int ibdyty)
  * @brief get the number of contactor of RBDY2
  *
  * @cond PYDOC
  * python usage : nb = RBDY2_GetNbContactor(ibdyty)
  *
  * @param[in] ibdyty (integer) : rank of the RBDY2 in container
  * @return nb (integer) : number of contactor attached to a RBDY2
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : rank of the RBDY2 in container
  * @return (int) number of RBDY2 in container
  * @endcond
  */
  extern "C" int RBDY2_GetNbContactor(int ibdyty);

 /**
  * @fn void RBDY2_GetContactorType(int ibdyty, char** c5, int itacty=1)
  * @brief Get the type of a contactor of a body
  *
  * @cond PYDOC
  * usage type = RBDY2_GetContactorType(ibdyty, itacty=1)
  * @param[in] ibdyty (integer) : rank of the RBDY2 in container
  * @param[in] itacty (integer) : rank of the contactor in the RBDY2 (default to 1)
  * @return type (string) : type of the contactor of the body
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : rank of the RBDY2 in container
  * @param[out] c5 (char*[5]) : type of the contactor of the body
  * @param[in] itacty (int) : rank of the contactor in the RBDY2 (default to 1)
  * @endcond
  */
  extern "C" void RBDY2_GetContactorType(int ibdyty, char** c5, int itacty=1);
  
 /**
  * @fn void RBDY2_GetContactorColor(int ibdyty, int itacty=1, char** c5)
  * @brief Get the color of the itacty contactor of a body ibdyty
  *
  * @cond PYDOC
  * usage color = RBDY2_GetContactorColor(ibdyty,itacty)
  * @param[in] ibdyty (integer) : rank of the RBDY2 in container
  * @param[in] itacty (integer) : rank of the contactor in the RBDY2 (default to 1)
  * @return color (string) : color of the contactor of the body
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : rank of the RBDY2 in container
  * @param[out] c5 (char*[5]) : color of the contactor of the body
  * @param[in] itacty (int) : rank of the contactor in the RBDY2 (default to 1)
  * @endcond
  */
  extern "C" void RBDY2_GetContactorColor(int ibdyty, char** c5, int itacty=1);
  
 /**
  * @fn void RBDY2_SetContactorColor(int ibdyty, char * cvalue1, int itacty=0)
  * @brief Set the color of a given contactor of a body (or all of them if not provided)
  *
  * @cond PYDOC
  * usage : RBDY2_SetContactorColor(ibdyty, itacty, color)
  * @param[in] ibdyty (integer)            : rank of the RBDY2
  * @param[in] color (string of size 5)    : the color 
  * @param[in] itacty (integer)            : rank of the contactor in the RBDY2 (default to 0 => all of them)
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int)                : rank of the RBDY2 in container
  * @param[in] cvalue1 (char[5])           : the color 
  * @param[in] itacty (int)                : rank of the contactor in the RBDY2 (default to 0 => all of them)
  * @endcond
  *
  *
  */
  extern "C" void RBDY2_SetContactorColor(int ibdyty, char * cvalue1, int itacty=0);




//---vt--- No documentation specific to python since it is meant to be used in c++ only
 /**
  * @fn void RBDY2_GetPtrMass(int ibdyty, double ** mass)
  * @brief Get a pointer onto the mass matrix of a body 
  *
  * @param[in] ibdyty(int) : index of the RBDY2
  * @param[out] mass(double**) : mass matrix of the RBDY2
  */
  extern "C" void RBDY2_GetPtrMass(int ibdyty, double ** mass);
  
 /**
  * @fn void RBDY2_GetVelocity(int ibdyty, double * velocity)
  * @brief Get the velocity of a body 
  *
  * @param[in] ibdyty(int) : index of the RBDY2
  * @param[out] velocity(double[6]) : velocity of the RBDY2
  */
  extern "C" void RBDY2_GetVelocity(int ibdyty, double * velocity);
 //---end vt---
  
 /**
  * @fn void RBDY2_getDrvVlocy(int ibdyty, int** i4_vector, int* i4_size, double** r8_vector, int* r8_size);
  * @brief Get the driven dof of a body
  *
  * @cond PYDOC
  * python usage : [drvdof_indices, drvdof_values] = RBDY2_getDrvVlocy(ibdyty)
  * @param[in] ibdyty (integer) : index of the RBDY2
  * @param drvdof_indices (integer array) : indices list of driven dof
  * @param drvdof_values  (real array) : values of the driven dof
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the RBDY2
  * @param[out] i4_vector (int**) : reference onto the indices list of driven dofs
  * @param[out] i4_size   (int*)  : size of the array referenced by i4_vector
  * @param[out] r8_vector (double**) : reference onto the values of driven dofs
  * @param[out] r8_size   (int*)  : size of the array referenced by r8_vector
  * @endcond
  */
  extern "C" void RBDY2_getDrvVlocy(int ibdyty, int** i4_vector, int* i4_size, double** r8_vector, int* r8_size);
  
 /**
  * @fn void RBDY2_computeDrvVlocy(int ibdyty, double * rvector_in, int rlength_in);
  * @brief Compute the value of the driven velocity of a body a current time
  *
  * In place replacement in the input array of the new value(s) of the driven velocity
  *
  * @cond PYDOC
  * python usage : RBDY2_computeDrvVlocy(ibdyty, values)
  * @param[in] ibdyty (integer) : index of the RBDY2
  * @param[in,out] values (double array) : numpy array, input old values of imposed velocity, output new ones
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : index of the RBDY2
  * @param[in,out] vector_in (double *) : input old values of imposed velocity, output new ones
  * @param[in] length (int) : size of vector_in array
  * @endcond
  */
  extern "C" void RBDY2_computeDrvVlocy(int ibdyty, double * rvector_in, int rlength_in);

 /**
  * @fn void RBDY2_SetVisible(int ibdyty);
  * @brief Set a given body as visible
  *
  * @cond PYDOC
  * python usage : RBDY2_SetVisible(ibdyt)
  * @param[in] ibdyty (integer) : index of the RBDY2
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : index of the RBDY2
  * @endcond
  */
  extern "C" void RBDY2_SetVisible(int ibdyty);

 /**
  * @fn void RBDY2_SetInvisible(int ibdyty);
  * @brief Set a given body as invisible
  *
  * @cond PYDOC
  * python usage : RBDY2_SetInvisible(ibdyty)
  * @param[in] ibdyty (integer) : index of the RBDY2
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : index of the RBDY2
  * @endcond
  */
  extern "C" void RBDY2_SetInvisible(int ibdyty);

 /**
  * @fn void  RBDY2_SetVisibleVlocyDrivenDof( int ibdyty, int iccdof )
  * @brief allows to (re)activate a given vlocydrivendof (i.e. which has been declared in preprocessing)
  *
  * @cond PYDOC
  * python usage : RBDY2_SetVisibleVlocyDrivenDof(ibdyty, iccdof)
  * @param[in] ibdyty(integer) : index of the RBDY2
  * @param[in] iccdof(integer) : index of the DOF to set visible
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the RBDY2
  * @param[in] iccdof(int) : index of the DOF to set visible
  * @endcond
 */
 extern "C" void RBDY2_SetVisibleVlocyDrivenDof( int ibdyty, int iccdof ) ;

 /**
  * @fn void  RBDY2_SetInvisibleVlocyDrivenDof( int ibdyty, int iccdof )
  * @brief allows to deactivate a given vlocydrivendof (i.e. which has been declared in preprocessing)
  *
  * @cond PYDOC
  * python usage : RBDY2_SetInvisibleVlocyDrivenDof(ibdyty, iccdof)
  * @param[in] ibdyty(integer) : index of the RBDY2
  * @param[in] iccdof(integer) : index of the DOF to set invisible
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the RBDY2
  * @param[in] iccdof(int) : index of the DOF to set invisible
  * @endcond
 */
 extern "C" void RBDY2_SetInvisibleVlocyDrivenDof( int ibdyty, int iccdof ) ;

/**
  * @fn void RBDY2_GetBulkBehavID(int ibdyty, int iblmty, char **blmID)
  * @brief return the ID of a given bulk of a given body
  *
  * @cond PYDOC
  * python usage : blmID = DISKx_GetBulkBehavID(ibdyty, iblmty)
  * @param[in] ibdyty (integer) : rank of a RBDY2
  * @param[in] iblmty (integer) : rank of a bulk of the giben RBDY2 (typically 1!)
  * @return    blmID (string)   : the bulk behav ID
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int)  : rank of a RBDY2
  * @param[in] iblmty (int)  : rank of a bulk of the giben RBDY2 (typically 1!)
  * @return    blmID (char*) : the bulk behav ID
  * @endcond
  */
  extern "C" void RBDY2_GetBulkBehavID(int ibdyty, int iblmty, char **c5);

 /**
  * @fn void RBDY2_GetBulkBehavNumber(int ibdyty)
  * @brief return the bulk ID of a given RBDY2
  *
  * @cond PYDOC
  * python usage : ibehav = RBDY2_GetBulkBehavNumber(ibdyty)
  * @param[in] ibdyty (integer) : rank of a RBDY2
  * @return    ibehav (integer) : the bulk behav number
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : rank of a RBDY2
  * @return     blmID (int) : the bulk behav number
  * @endcond
  */
  extern "C" int RBDY2_GetBulkBehavNumber(int ibdyty);


 /**
  * @fn void RBDY2_SetSurfaceSectors(int nbsect);
  * @brief Set the number of angular sectors of the surface of contactors 
  *
  * @cond PYDOC
  * python usage : RBDY2_SetSurfaceSectors(nbsect)
  * @param[in] nbsect (integer) : number of sectors
  * @endcond
  *
  * @cond CDOC
  * @param[in] nbsect (int) : number of sectors
  * @endcond
  */
  extern "C" void RBDY2_SetSurfaceSectors(int nbsect);

 /**
  * @fn void RBDY2_GetStress(int ibdyty, double** pointer_out, int* dim1, int* dim2 );
  * @brief Get the mean stress field of a rigid object 
  *
  * @cond PYDOC
  * python usage : matrix = RBDY2_GetStress(ibdyty)
  * @param[in] ibdyty (integer) : body to get stress of
  * @param[in] matrix (double array) : stress matrix
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int)
  * @param[out] matrix_out (double **) : the stress
  * @param[out] dim1 (int* ) : first dimension of matrix_out
  * @param[out] dim2 (int* ) : second dimension of matrix_out
  * @endcond
  */
  extern "C" void RBDY2_GetStress(int ibdyty, double** matrix_out, int* dim1, int* dim2 );

 /**
  * @fn void RBDY2_ModifyBody(int ibdyty, int itacty, double * rvector_in, int rlength_in)
  * @brief Modify a body tactor
  *
  * @cond PYDOC
  * usage : RBDY2_ModifyBody(ibdyty, itacty, vector)
  * @param[in] ibdyty (integer)            : rank of body
  * @param[in] itacty (integer)            : rank of tacty
  * @param[in] vector (double array)       : the new value \n
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)        : id of considered body
  * @param[in] ivalue2 (int)        : id of considered tacty
  * @param[in] vector_in (double *) : the new vector
  * @param[in] length (int)         : size of vector_in
  * @endcond
  */
  extern "C" void RBDY2_ModifyBody(int ivalue1, int ivalue2, double * rvector_in, int rlength_in);

 /**
  * @fn void RBDY2_SkipInvisible(void)
  * @brief skip invisible objects when writing BODIES.OUT
  *
  * @cond PYDOC
  * usage : RBDY2_SkipInvisible()
  * @endcond
  */
  extern "C" void RBDY2_SkipInvisible(void);

 /**
  * @fn void RBDY2_InitializeStresses(void)
  * @brief initialize stress for rigid bodies
  *
  * @cond PYDOC
  * usage : RBDY2_InitializeStresses()
  * @endcond
  */
  extern "C" void RBDY2_InitializeStresses(void);

/**
  * @fn void RBDY2_InitializeWS(double rvalue1)
  * @brief initialize WS for rigid bodies with a value between wsmin and wsmax ponderate by rvalue1
  *
  * @cond PYDOC
  * python usage : RBDY2_InitializeWS(double rvalue1)
  * @endcond
  */
  extern "C" void RBDY2_InitializeWS(double rvalue1);

/**
  * @fn void RBDY2_CleanMemory(void)
  * @brief Free all memory allocated within RBDY2 module
  *
  * @cond PYDOC
  * python usage : RBDY2_CleanMemory()
  * @endcond
  */
  extern "C" void RBDY2_CleanMemory(void);

 /**
  * @fn double RBDY2_GetThermalValue(int ibdyty, int itacty)
  * @brief Get temperature of rigid particle
  *
  * @cond PYDOC
  * usage : T = RBDY2_GetThermalValu(ibdyty, itacty)
  * @param[in] ibdyty (integer) : rank of body
  * @param[in] itacty (integer) : rank of tacty
  */
  extern "C" double RBDY2_GetThermalValue(int ibdyty, int itacty);

 /**
  * @fn double RBDY2_GetElectricalPotential(int ibdyty)
  * @brief Get electrical potential of rigid particle
  *
  * @cond PYDOC
  * usage : ep = RBDY2_GetElectricalPotential(ibdyty, itacty)
  * @param[in] ibdyty (integer) : rank of body
  * @return ep (double)         : electrical potential
  * @endcond
  */
  extern "C" double RBDY2_GetElectricalPotential(int ibdyty);

 /**
  * @fn double RBDY2_GetElectricalCurrent(int ibdyty)
  * @brief Get electrical potential of rigid particle
  *
  * @cond PYDOC
  * usage : ep = RBDY2_GetElectricalCurrent(ibdyty, itacty)
  * @param[in] ibdyty (integer) : rank of body
  * @return ep (double)         : electrical current
  * @endcond
  */
  extern "C" double RBDY2_GetElectricalCurrent(int ibdyty);

 /**
  * @fn double RBDY2_GetBetai(int ibdyty, int itacty)
  * @brief Get equivalent damage related to CZM interaction for rigid particle
  *
  * @cond PYDOC
  * usage : betai = RBDY2_GetBetai(ibdyty, itacty)
  * @param[in] ibdyty (integer) : rank of body
  * @param[in] itacty (integer) : rank of tacty
  * @return betai (double)      : equivalent damage
  * @endcond
  */
  extern "C" double RBDY2_GetBetai(int ibdyty, int itacty);

 /**
  * @fn integer RBDY2_GetPeriode(int ibdyty)
  * @brief Get the periode id (0, 1 or -1) for rigid particles
  *
  * @cond PYDOC
  * usage : iper = RBDY2_GetPeriode(ibdyty)
  * @param[in] ibdyty (integer) : rank of body
  * @return iper (integer)      : periode id
  * @endcond
  */
  extern "C" int RBDY2_GetPeriode(int ibdyty);

/**
  * @fn double RBDY2_GetAverageSurfaceEnerg(int ibdyty, int itacty)
  * @brief Get average Surface Energy for rigid particle
  *
  * @cond PYDOC
  * usage : SE = RBDY2_GetAverageSurfaceEnergy(ibdyty, itacty)
  * @param[in] ibdyty (integer) : rank of body
  * @param[in] itacty (integer) : rank of tacty
  * @return SE (double)         : average Surface Energy
  * @endcond
  */
  extern "C" double RBDY2_GetAverageSurfaceEnergy(int ibdyty, int itacty);


#endif /* wrap_RBDY2 */
