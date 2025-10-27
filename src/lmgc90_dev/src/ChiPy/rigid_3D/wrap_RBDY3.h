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

#ifndef wrap_RBDY3_h
#define wrap_RBDY3_h

 /**
  * @fn void RBDY3_IncrementStep(void)
  * @brief compute the current velocity and displacement
  *
  * @cond PYDOC
  * python usage : RBDY3_IncrementStep()
  * @endcond
 */
 extern "C" void RBDY3_IncrementStep(void);


 /**
  * @fn void RBDY3_SetVlocyDrivenDof(int ibdyty, int idrvdof, double value)
  * @brief Override the value of an existing velocity driven dof
  *
  * @cond PYDOC
  * usage : RBDY3_SetVlocyDrivenDof(ibdyty, idrvdof, value)
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
  extern "C" void RBDY3_SetVlocyDrivenDof(int ibdyty, int idrvdof, double value);

 /**
  * @fn void RBDY3_FatalDamping(int * ivector_in=nullptr, int ilength_in=0)
  * @brief Nullify body velocities (current and initial) of a list of bodies
  *
  * @cond PYDOC
  * python usage : RBDY3_FatalDamping(i_list)
  * @param[in] i_list (list of integer) : list of bodies to reset current velocity
  *            if omitted works on all objetcs
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of RBDY3
  * @param[in] length (int)     : size of vector_in
  * @endcond
 */
 extern "C" void RBDY3_FatalDamping(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void RBDY3_ComputeFext(void)
  * @brief compute external forces
  *
  * @cond PYDOC
  * python usage : RBDY3_ComputeFext()
  * @endcond
 */
 extern "C" void RBDY3_ComputeFext(void);

 /**
  * @fn void RBDY3_ComputeBulk(void)
  * @brief compute internal forces
  *
  * @cond PYDOC
  * python usage : RBDY3_ComputeBulk()
  * @endcond
 */
 extern "C" void RBDY3_ComputeBulk(void);
    
 /**
  * @fn void RBDY3_ComputeFreeVelocity(void)
  * @brief compute free velocity 
  *
  * @cond PYDOC
  * python usage : RBDY3_ComputeFreeVelocity()
  * @endcond
 */
 extern "C" void RBDY3_ComputeFreeVelocity(void);
    
 /**
  * @fn void RBDY3_ComputeDof(void)
  * @brief update current position and velocity
  *
  * @cond PYDOC
  * python usage : RBDY3_ComputeDof()
  * @endcond
 */
 extern "C" void RBDY3_ComputeDof(void);

 /**
  * @fn void RBDY3_UpdateDof(void)
  * @brief save d.o.f. of the end of the time step to d.o.f. of the begining of the next one
  *
  * @cond PYDOC
  * python usage : RBDY3_UpdateDof()
  * @endcond
 */
 extern "C" void RBDY3_UpdateDof(void);
    
 /**
  * @fn void RBDY3_ComputeContactDetectionConfiguration(void)
  * @brief compute the contact detection configuration
  *
  * @cond PYDOC
  * python usage : RBDY3_ComputeContactDetectionConfiguration()
  * @endcond
 */
 extern "C" void RBDY3_ComputeContactDetectionConfiguration(void);

 /**
  * @fn void RBDY3_WriteLastDof(void)
  * @brief write ascii DOF.LAST file
  *
  * @cond PYDOC
  * python usage : RBDY3_WriteLastDof()
  * @endcond
 */
 extern "C" void RBDY3_WriteLastDof(void);

 /**
  * @fn void RBDY3_WriteOutDof(int ifrom=0, int ito=0)
  * @brief write ascii DOF.OUT file. Can be activate only each N step
  *
  * @cond PYDOC
  * python usage : RBDY3_WriteOutDof(ifrom=0, ito=0)
  * @param[in] ifrom (integer) : begining of bodys' index that will be written
  * @param[in] ito   (integer) : end of bodys'index that will be written
  * @endcond
  *
  * @cond CDOC
  * @param[in] ifrom (int) : begining of bodys' index that will be written
  * @param[in] ito   (int) : end of bodys'index that will be written
  * @endcond
  * If 0 for ifrom and ito, dofs of all bodies are written.
 */
 extern "C" void RBDY3_WriteOutDof(int ifrom=0, int ito=0);

 /**
  * @fn void RBDY3_DisplayOutDof(void)
  * @brief display bodies degrees of freedom
  *
  * @cond PYDOC
  * python usage : RBDY3_DisplayOutDof()
  * @endcond
 */
 extern "C" void RBDY3_DisplayOutDof(void);

 /**
  * @fn void RBDY3_WriteLastRnod(void)
  * @brief write ascii Rnod.LAST file
  *
  * @cond PYDOC
  * python usage : RBDY3_WriteLastRnod()
  * @endcond
 */
 extern "C" void RBDY3_WriteLastRnod(void);

 /**
  * @fn void RBDY3_WriteOutRnod(void)
  * @brief write ascii Rnod.OUT file. Can be activate only each N step.
  *
  * @cond PYDOC
  * python usage : RBDY3_WriteOutRnod()
  * @endcond
 */
 extern "C" void RBDY3_WriteOutRnod(void);

 /**
  * @fn void  RBDY3_DisplayOutRnod(void)
  * @brief display body forces.
  *
  * @cond PYDOC
  * python usage : RBDY3_DisplayOutRnod()
  * @endcond
 */
 extern "C" void RBDY3_DisplayOutRnod(void);

 /**
  * @fn void RBDY3_WriteBodies(void)
  * @brief write BODIES.OUT file
  *
  * @cond PYDOC
  * python usage : RBDY3_WriteBodies()
  * @endcond
 */
 extern "C" void RBDY3_WriteBodies(void);

 /**
  * @fn void RBDY3_WriteDrivenDof(void)
  * @brief write DRV_DOF.OUT file
  *
  * @cond PYDOC
  * python usage : RBDY3_WriteDrivenDof()
  * @endcond
 */
 extern "C" void RBDY3_WriteDrivenDof(void);

 /**
  * @fn void RBDY3_ReadBodies(void)
  * @brief read BODIES.DAT file
  *
  * Initializes existing_entities variable in RBDY3
  *
  * Adds the number of found bodies to entity
  *
  * @cond PYDOC
  * python usage : RBDY3_ReadBodies()
  * @endcond
 */
 extern "C" void RBDY3_ReadBodies(void);

 /**
  * @fn void RBDY3_ReadIniDof(int num=0)
  * @brief Read DOF file
  *
  * If num <= 0 : DATBOX/DOF.INI file is read
  *
  * Else : OUTBOX/DOF.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniDof
  * last call
  *
  * @cond PYDOC
  * usage : RBDY3_ReadIniDof(num=0)
  * @param[in] num (integer) : which DOF file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which DOF file to read
  * @endcond
  *
  */
  extern "C" void RBDY3_ReadIniDof(int num=0);

 /**
  * @fn void RBDY3_ReadDrivenDof(void)
  * @brief read DRV_DOF.DAT file
  *
  * @cond PYDOC
  * python usage : RBDY3_ReadDrivenDof()
  * @endcond
 */
 extern "C" void RBDY3_ReadDrivenDof(void);


 /**
  * @fn void RBDY3_LoadBehaviours(void)
  * @brief Load bulk behaviour id from bulk_behav module
  *
  * @cond PYDOC
  * python usage : RBDY3_LoadBehaviours()
  * @endcond
 */
 extern "C" void RBDY3_LoadBehaviours(void);

 /**
  * @fn void RBDY3_ComputeMass(void)
  * @brief compute mass and inertia of bodies
  *
  * @cond PYDOC
  * python usage : RBDY3_ComputeMass()
  * @endcond
 */
 extern "C" void RBDY3_ComputeMass(void);

 /**
  * @fn void RBDY3_NewRotationScheme(void)
  * @brief active new rotation scheme FLAG
  *
  * @cond PYDOC
  * python usage : RBDY3_NewRotationScheme()
  * @endcond
 */
 extern "C" void RBDY3_NewRotationScheme(void);

 /**
  * @fn void RBDY3_SetZminBoundary(double Zmin)
  * @brief define the boundary of command CHECK_OUT_OF_BOUNDS
  *
  * @cond PYDOC
  * python usage : RBDY3_SetZminBoundary(Zmin)
  * @param[in] Zmin (real) : inferior boundary value
  * @endcond
  *
  * @cond CDOC
  * @param[in] Zmin (double) : inferior boundary value
  * @endcond
 */
 extern "C" void RBDY3_SetZminBoundary(double Zmin);

 /**
  * @fn void RBDY3_SetZmaxBoundary(double Zmax)
  * @brief define the boundary of command CHECK_OUT_OF_BOUNDS
  *
  * @cond PYDOC
  * python usage : RBDY3_SetZmaxBoundary(Zmax)
  * @param[in] Zmax (real) : superior boundary value
  * @endcond
  *
  * @cond CDOC
  * @param[in] Zmax (double) : superior boundary value
  * @endcond
 */
 extern "C" void RBDY3_SetZmaxBoundary(double Zmax);

 /**
  * @fn void RBDY3_SetYminBoundary(double Ymin)
  * @brief define the boundary of command CHECK_OUT_OF_BOUNDS
  *
  * @cond PYDOC
  * python usage : RBDY3_SetYminBoundary(Ymin)
  * @param[in] Ymin (real) : left boundary value
  * @endcond
  *
  * @cond CDOC
  * @param[in] Ymin (double) : left boundary value
  * @endcond
 */
 extern "C" void RBDY3_SetYminBoundary(double Ymin);

 /**
  * @fn void RBDY3_SetYmaxBoundary(double Ymax)
  * @brief define the boundary of command CHECK_OUT_OF_BOUNDS
  *
  * @cond PYDOC
  * python usage : RBDY3_SetYmaxBoundary(Ymax)
  * @param[in] Ymax (real) : right boundary value
  * @endcond
  *
  * @cond CDOC
  * @param[in] Ymax (double) : right boundary value
  * @endcond
 */
 extern "C" void RBDY3_SetYmaxBoundary(double Ymax);

 /**
  * @fn void RBDY3_SetXminBoundary(double Xmin)
  * @brief define the boundary of command CHECK_OUT_OF_BOUNDS
  *
  * @cond PYDOC
  * python usage : RBDY3_SetXminBoundary(Xmin)
  * @param[in] Xmin (real) : inferior boundary value
  * @endcond
  *
  * @cond CDOC
  * @param[in] Xmin (double) : inferior boundary value
  * @endcond
 */
 extern "C" void RBDY3_SetXminBoundary(double Xmin);

 /**
  * @fn void RBDY3_SetXmaxBoundary(double Xmax)
  * @brief define the boundary of command CHECK_OUT_OF_BOUNDS
  *
  * @cond PYDOC
  * python usage : RBDY3_SetXmaxBoundary(Xmax)
  * @param[in] Xmax (real) : front boundary value
  * @endcond
  *
  * @cond CDOC
  * @param[in] Xmax (double) : front boundary value
  * @endcond
 */
 extern "C" void RBDY3_SetXmaxBoundary(double Xmax);

 /**
  * @fn void RBDY3_SetXPeriodicCondition(double xperiod)
  * @brief set the period on X axis
  *
  * @cond PYDOC
  * python usage : RBDY3_SetXPeriodicCondition(xperiod)
  * @param[in] xperiod (real) : period on x axis
  * @endcond
  *
  * @cond CDOC
  * @param[in] xperiod (double) : period on x axis
  * @endcond
 */
 extern "C" void RBDY3_SetXPeriodicCondition(double xperiode);

 /**
  * @fn void RBDY3_SetYPeriodicCondition(double yperiod)
  * @brief set the periode on Y axis
  *
  * @cond PYDOC
  * python usage : RBDY3_SetYPeriodicCondition(yperiod)
  * @param[in] yperiod (real) : period on y axis
  * @endcond
  *
  * @cond CDOC
  * @param[in] yperiod (double) : period on y axis
  * @endcond
 */
 extern "C" void RBDY3_SetYPeriodicCondition(double Yperiode);

 /**
  * @fn void RBDY3_AvoidBodyRotation(void)
  * @brief kill rotation effect for RBDY3
  *
  * @cond PYDOC
  * python usage : RBDY3_AvoidBodyRotation()
  * @endcond
 */
 extern "C" void RBDY3_AvoidBodyRotation(void);


 /**
  * @fn void RBDY3_SkipInvisible(void)
  * @brief if a body is invisible, il will not be written in bodies.out and dof.out
  *
  * @cond PYDOC
  * python usage : RBDY3_SkipInvisible()
  * @endcond
 */
 extern "C" void RBDY3_SkipInvisible(void);

 /**
  * @fn void RBDY3_KeepIniDofOrder(void)
  * @brief numbering information as they are read
  *
  * @cond PYDOC
  * python usage : RBDY3_KeepIniDofOrder()
  * @endcond
 */
 extern "C" void RBDY3_KeepIniDofOrder(void);

 /**
  * @fn void void RBDY3_SetVisible(int ibdyty)
  * @brief rended a given RBDY3 visible
  *
  * @cond PYDOC
  * python usage : RBDY3_SetVisible(ibdyty)
  * @param[in] ibdyty(integer) : index of the RBDY3
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the RBDY3
  * @endcond
 */
 extern "C" void RBDY3_SetVisible(int ibdyty);

 /**
  * @fn void void RBDY3_SetInvisible(int ibdyty)
  * @brief rended a given RBDY3 invisible
  *
  * @cond PYDOC
  * python usage : RBDY3_SetInvisible(ibdyty)
  * @param[in] ibdyty(integer) : index of the RBDY3
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the RBDY3
  * @endcond
 */
 extern "C" void RBDY3_SetInvisible(int ibdyty);

 /**
  * @fn int RBDY3_IsVisible(int idbdy)
  * @brief return if a given body visible
  *
  * @cond PYDOC
  * python usage : visible = RBDY3_IsVisible(ibdyty)
  * @param[in] idbdy(integer) : id of the body we want visibility
  * @return visible (integer) : 1 if body is visible, 0 else
  * @endcond
  *
  * @cond CDOC
  * @param[in] idbdy(int) : id of the body we want visibility
  * @return (int) 1 if body is visible, 0 else
  * @endcond
  */
  extern "C" int RBDY3_IsVisible(int idbdy);

 /**
  * @fn void RBDY3_SetBodiesInvisible(int * ivector_in, int ilength_in)
  * @brief Set a list of body to invisible state
  *
  * @cond PYDOC
  * usage : RBDY3_SetBodiesInvisible(list_bdy)
  * @param[in] list_bdy (integer array) : list of rank of bodies of the container
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int[length]) : list of rank of bodies of the container
  * @param[in] lenght (int)            : size of vector_in
  * @endcond
  */
  extern "C" void RBDY3_SetBodiesInvisible(int * ivector_in, int ilength_in);

 /**
  * @fn void RBDY3_CompCoor(void)
  * @brief Compute the position of bodies
  *
  * @cond PYDOC
  * python usage : RBDY3_CompCoor()
  * @endcond
  */
  extern "C" void RBDY3_CompCoor(void);
  
 /**
  * @fn double RBDY3_GetBodyDensity(int ibdyty)
  * @brief Get the density of a given body 
  *
  * @cond PYDOC
  * python usage : density = RBDY3_GetBodyDensity(ibdyty)
  * @param[in] ibdyty(integer) : rank of the RBDY3
  * @return    density(double) : density of the RBDY3
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int)     : rank of the RBDY3
  * @return    density(double) : density of the RBDY3
  * @endcond
  */
  extern "C" double RBDY3_GetBodyDensity(int ibdyty);
  
 /**
  * @fn void RBDY3_GetBodyInertia(int ibdyty, double ** r8_vector, int * r8_size)
  * @brief Get the principal inertia of a given RBDY3
  *
  * @cond PYDOC
  * python usage : inertia = RBDY3_GetBodyInertia(ibdyty)
  * @param[in] ibdyty (integer)       : rank of the RBDY3
  * @return    inertia (double array) : inertia vector of the desired RBDY3
  * @endcond
  *
  * @cond CDOC
  * @param[in]  ibdyty (int)         : index of the RBDY3
  * @param[out] r8_vector (double**) : mass matrix of the RBDY3
  * @param[out] r8_size (int*)       : length of r8_vector
  * @endcond
  */
  extern "C" void RBDY3_GetBodyInertia(int ibdyty, double ** r8_vector, int * r8_size);
  
 /**
  * @fn void RBDY3_GetAllInertia(double** matrix_out, int* dim1, int* dim2)
  * @brief Get the inertia of a all RBDY3 body
  *
  * @cond PYDOC
  * usage : inertia = RBDY3_GetAllInertia()
  * @param inertia (double array): the inertia of all bodies
  * @endcond
  *
  * @cond CDOC
  * @param[out] matrix_out (double**) : out matrix
  * @param[out] dim1 (int*)           : first size of out matrix (3)
  * @param[out] dim2 (int*)           : second size of out matrix (nb_bodies)
  * @endcond
  */
  extern "C" void RBDY3_GetAllInertia(double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void RBDY3_CollectBodiesDotOUT(void)
  * @brief 
  *
  * @cond PYDOC
  * python usage : RBDY3_CollectBodiesDotOUT()
  * @endcond
  */
  extern "C" void RBDY3_CollectBodiesDotOUT(void);

 /**
  * @fn void RBDY3_AppendToBodiesDotOUT(void)
  * @brief 
  *
  * @cond PYDOC
  * python usage : RBDY3_AppendToBodiesDotOUT()
  * @endcond
  */
  extern "C" void RBDY3_AppendToBodiesDotOUT(void);

 /**
  * @fn void RBDY3_RebuildBodiesDotDAT(void)
  * @brief 
  *
  * @cond PYDOC
  * python usage : RBDY3_RebuildBodiesDotDAT()
  * @endcond
  */
  extern "C" void RBDY3_RebuildBodiesDotDAT(void);

 /**
  * @fn void RBDY3_PutBodyVector(char * cvalue1, int ivalue1, double * rvector_in, int rlength_in)
  * @brief Set a vector of a RBDY3 body
  *
  * uses copy, and in case of Fext, the operation is not
  * just setting but adding
  *
  * @cond PYDOC
  * python usage : RBDY3_PutBodyVector(datatype, ibdyty, vector)
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
  */
  extern "C" void RBDY3_PutBodyVector(char * cvalue1, int ivalue1, double * rvector_in, int rlength_in);

 /**
  * @fn void RBDY3_PutAllBodyVector(char * cvalue1, double* matrix_in, int dim1, int dim2)
  * @brief Put an array of a vector of all RBDY3 bodies (visible and invisible)
  *
  * @cond PYDOC
  * python usage : RBDY3_PutAllBodyVector(datatype, matrix)
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
  * ... see RBDY3_PutBodyVector
  */
  extern "C" void RBDY3_PutAllBodyVector(char * cvalue1, double* matrix_in, int dim1, int dim2);

 /**
  * @fn void RBDY3_GetBodyVector(char * cvalue1, int ivalue1, double** r8_vector, int* r8_size)
  * @brief Get a copy of a vector of a RBDY3 body
  *
  * @cond PYDOC
  * python usage : vector = RBDY3_GetBodyVector(datatype, ibdyty)
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
  * - "Coor_": coordinates in computed configuration
  * - "Coorb": coordinates at beginning of time step
  * - "Coorm": coordinates in detection configuration
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
  extern "C" void RBDY3_GetBodyVector(char * cvalue1, int ivalue1, double** r8_vector, int* r8_size);

 /**
  * @fn void RBDY3_GetAllBodyVector(char * cvalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get an array of a vector of all RBDY3 bodies (visible and invisible)
  *
  * @cond PYDOC
  * python usage : matrix = RBDY3_GetBodyVector(datatype, ibdyty)
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
  * ... see RBDY3_GetBodyVector
  */
  extern "C" void RBDY3_GetAllBodyVector(char * cvalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void RBDY3_GetPtrBodyVector(char * cvalue1, int ivalue1, double ** pointer_out, int * length)
  * @brief Get a pointer on a vector of a RBDY3 body
  *
  * @cond PYDOC
  * python usage : vector_ptr = RBDY3_GetPtrBodyVector(datatype, ibdyty)
  * @param[in] datatype (string [5])     : the vector to set
  * @param[in] ibdyty (integer)          : rank of the RBDY3
  * @return    vector_ptr (double array) : reference on the desired vector seen as a numpy array
  * @endcond
  *
  * @cond CDOC
  * @param[in]  cvalue1 (char[5])       : the vector to get
  * @param[in]  ivalue1 (int)           : rank of the RBDY3
  * @param[out] pointer_out (double **) : reference on the vector
  * @param[in]  length (int*)           : reference on the length of the out vector
  * @endcond
  *
  * Possible values for datatype field are: 
  * - "Coor0": reference coordinates
  * - "X____": cumulated displacements over time in computed configuration
  * - "Xbeg_": cumulated displacements over time at beginning of time step
  * - "V____": velocity in computed configuration
  * - "Vbeg_": velocity at beginning of time step
  * - "Vaux_": working array for velocity
  * - "Ireac": contact impulse
  * - "Iaux_": working array for impulste
  * - "Fext_": external forces
  *
  */
  extern "C" void RBDY3_GetPtrBodyVector(char * cvalue1, int ivalue1, double ** pointer_out, int * length);

 /**
  * @fn void RBDY3_PutBodyMatrix(char * cvalue1, int ivalue1, double * matrix_in, int dim1, int dim2)
  * @brief Set a matrix of a RBDY3 body
  *
  * Uses copy
  *
  * @cond PYDOC
  * python usage : RBDY3_PutBodyMatrix(datatype, ibdyty, matrix)
  * @param[in] datatype (string [5]) : the vector to set
  * @param[in] ibdyty (integer)      : rank of the RBDY3
  * @param[in] matrix (double array) : a matrix
  * @endcond
  *
  * @cond CDOC
  * @param[in] cvalue1 (char[5])    : the vector to set
  * @param[in] ivalue1 (int)        : rank of the RBDY3
  * @param[in] matrix_in (double *) : the new matrix
  * @param[in] dim1 (int)           : the first dimension of the new matrix
  * @param[in] dim2 (int)           : the second dimension of the new matrix
  * @endcond
  *
  * Possible values for datatype field are:
  * - "IFbeg": inertia frame at beginning of time step
  * - "IFTT_": inertia frame in detection configuration
  * - "IF___": inertia frame in computed configuration
  *
  */
  extern "C" void RBDY3_PutBodyMatrix(char * cvalue1, int ivalue1, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void RBDY3_GetBodyMatrix(char * cvalue1, int ivalue1, double** matrix_out, int* dim1, int* dim2 )
  * @brief Get a copy of a matrix of a RBDY3 body
  *
  * Uses copy
  *
  * @cond PYDOC
  * python usage : matrix = RBDY3_GetBodyMatrix(datatype, ibdyty)
  * @param[in] datatype (string [5]) : the vector to get
  * @param[in] ibdyty (integer)      : rank of the RBDY3
  * @return    matrix (double array) : output matrix
  * @endcond
  *
  * @cond CDOC
  * @param[in]  cvalue1 (char[5])     : the matrix to get
  * @param[in]  ivalue1 (int)         : rank of the RBDY3
  * @param[out] matrix_out (double**) : the out matrix
  * @param[out] dim1 (int *)          : the first dimension of matrix_out
  * @param[out] dim2 (int *)          : the second dimension of matrix_out
  * @endcond
  *
  * Possible values for datatype field are:
  * - "IFref": inertia frame in reference configuration
  * - "IFbeg": inertia frame at beginning of time step
  * - "IFTT_": inertia frame in detection configuration
  * - "IF___": inertia frame in computed configuration
  */
  extern "C" void RBDY3_GetBodyMatrix(char * cvalue1, int ivalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void RBDY3_GetAllRData(double** matrix_out, int* dim1, int* dim2 )
  * @brief Get a copy of a real data of all rbdy3
  *
  * In this order : coor, frame, vlocy, spin, fext, reac
  *
  * @cond PYDOC
  * python usage : rdata = RBDY3_GetAllRData()
  * @return    rdata (double array) : output matrix
  * @endcond
  *
  * @cond CDOC
  * @param[out] matrix_out (double**) : the out matrix
  * @param[out] dim1 (int *)          : the number of bodies
  * @param[out] dim2 (int *)          : the number of fields
  * @endcond
  */
  extern "C" void RBDY3_GetAllRData(double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn int RBDY3_GetNbRBDY3(void)
  * @brief get the number of RBDY3
  *
  * @cond PYDOC
  * python usage : nb_RBDY3 = RBDY3_GetNbRBDY3()
  * @return nb_RBDY3 (integer) : number of RBDY3 in container
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of RBDY3 in container
  * @endcond
  */
  extern "C" int RBDY3_GetNbRBDY3();

 /**
  * @fn void RBDY3_setNb(int nb_bdyty)
  * @brief set the number of RBDY3
  *
  * @cond PYDOC
  * python usage : RBDY3_setNbR(nb_bdyty)
  * @param[in] nb_bdyty (integer) : number of RBDY3 in container
  * @endcond
  *
  * @cond CDOC
  * @param[in] nb_bdyty (int) : number of RBDY3 in container
  * @endcond
  */
  extern "C" void RBDY3_setNb(int nb_bdyty);

 /**
  * @fn void RBDY3_addOne(double cooref[3], int nb_tacty, int nb_v_ddof, int nb_f_ddof)
  * @brief set one of RBDY3
  *
  * @cond PYDOC
  * python usage : RBDY3_addOne(cooref, nb_tacty, nb_v_ddof, nb_f_ddof)
  * @param[in] cooref (double array [3]) : reference coordinates of the RBDY3
  * @param[in] nb_tacty  (integer)       : the number of contactor on the RBDY3
  * @param[in] nb_v_ddof (integer)       : the number of velocity driven dof on the RBDY3
  * @param[in] nb_f_ddof (integer)       : the number of force driven dof on the RBDY3
  * @endcond
  *
  * @cond CDOC
  * @param[in] nb_bdyty (int) : number of RBDY3 in container
  * @param[in] cooref (double [3]) : reference coordinates of the RBDY3
  * @param[in] nb_tacty  (int)     : the number of contactors on the RBDY3
  * @param[in] nb_v_ddof (int)     : the number of velocity driven dof on the RBDY3
  * @param[in] nb_f_ddof (int)     : the number of force driven dof on the RBDY3
  * @endcond
  */
  extern "C" void RBDY3_addOne(double  rvec3[3], int nb_tacty, int nb_v_ddof, int nb_f_ddof);

 /**
  * @fn void RBDY3_setOneTactor(int ibdyty, int itacty, char * tacID, char * color, double volume, double vec3[3], double rvec9[9], double rvec3[3], int * ivector_in, int ilength_in, double * rvector_in, int rlength_in)
  * @brief set one of RBDY3
  *
  * @cond PYDOC
  * python usage : RBDY3_setOneTactor(i_bdyty, i_tacty, tacID, color, volume, inertia, frame, shift, idata, rdata)
  * @param[in] i_bdyty (integer)             : id of RBDY3 to set
  * @param[in] i_tacty (integer)             : id of contactor of RBDY3 to set
  * @param[in] tacID   (char[5])             : shape of contactor
  * @param[in] color   (char[5])             : color of contactor
  * @param[in] volume  (double)              : volume of contactor
  * @param[in] inertia (double array [3]   ) : inertia of contactor
  * @param[in] frame   (double array [3][3]) : frame of contactor
  * @param[in] shift   (double array [3]   ) : shift of contactor
  * @param[in] idata   (integer array)       : integer anonymous data
  * @param[in] rdata   (double  array)       : real anonymous data
  * @endcond
  *
  * @cond CDOC
  * @param[in] i_bdyty    (int)          : id of RBDY3 to set
  * @param[in] i_tacty    (int)          : id of contactor of RBDY3 to set
  * @param[in] tacID      (char * )      : shape of contactor (must be of size 5)
  * @param[in] color      (char * )      : color of contactor (must be of size 5)
  * @param[in] volume     (double)       : volume of contactor
  * @param[in] vec3       (double[3]   ) : inertia of contactor
  * @param[in] rmat33     (double[3][3]) : frame of contactor
  * @param[in] rvec3      (double[3]   ) : shift of contactor
  * @param[in] ivector_in (int * )       : integer anonymous data
  * @param[in] ilength_in (int)          : size of ivector_in
  * @param[in] rvector_in (double *)     : real anonymous data
  * @param[in] rlength_in (int)          : size of rvector_in
  * @endcond
  */
  extern "C" void RBDY3_setOneTactor(int ibdyty, int itacty, char * tacID, char * color, double volume, double vec3[3], double rmat33[3][3], double rvec3[3], int * ivector_in, int ilength_in, double * rvector_in, int rlength_in);

 /**
  * @fn void RBDY3_setBulk(int ibdyty, char * behav, double avrd, double vec3[3], double rmat33[3][3])
  * @brief set the bulk of an RBDY3
  *
  * @cond PYDOC
  * python usage : RBDY3_setBulk(i_bdyty, behav, avrd, inertia)
  * @param[in] i_bdyty (integer)             : id of RBDY3 to set
  * @param[in] behav   (char[5])             : behaviour name
  * @param[in] avrd    (double)              : a radius allowing to compute volume...
  * @param[in] inertia (double array [3]   ) : inertia (principal values)
  * @param[in] frame   (double array [3][3]) : inertial frame
  * @endcond
  *
  * @cond CDOC
  * @param[in] i_bdyty (int)          : id of RBDY3 to set
  * @param[in] behav   (char * )      : behaviour name (must be of size 5)
  * @param[in] color   (char * )      : color of contactor (must be of size 5)
  * @param[in] avrd    (double)       : a radius allowing to compute volume...
  * @param[in] vec3    (double[3]   ) : inertia (principal values)
  * @param[in] rmat33  (double[3][3]) : inertial frame
  * @endcond
  */
  extern "C" void RBDY3_setBulk(int ibdyty, char * behav, double avrd, double vec3[3], double rmat33[3][3]);

 /**
  * @fn void RBDY3_addDrvDof(int i_bdyty, bool vlocy, int i_dof, double values[6], double * rmatrix_in=nullptr, int rdim1=0, int rdim2=0)
  * @brief set one of RBDY3
  *
  * @cond PYDOC
  * python usage : RBDY3_addDrvDof(i_bdyty, vlocy, i_dof, values, rdata=None)
  * @param[in] i_bdyty (integer)            : id of RBDY3 in which to add driven dof
  * @param[in] vlocy   (boolean)            : if velocity driven dof or force one
  * @param[in] i_dof   (integer)            : index of dof of the RBDY3 which is driven
  * @param[in] predef  (double array [6]  ) : values of predefined driven dof (must always be present but may be ignored)
  * @param[in] evol    (double array [2,n]) : (optional) evolution array of [time,driven_values], if present, predef is ignored
  * @endcond
  *
  * @cond CDOC
  * @param[in] i_bdyty    (int)       : id of RBDY3 to set
  * @param[in] vlocy      (_bool)     : if driven dof is velocity or force
  * @param[in] i_dof      (int)       : index of dof of the RBDY3 which is driven
  * @param[in] rvec6      (double[6]) : values of predefined driven dof, ignored if rmatrix_in is not null
  * @param[in] rmatrix_in (double *)  : a matrix of evolution which contain any number of [time, value]
  * @param[in] rdim1      (int)       : first size of rmatrix_in (must be 2)
  * @param[in] rdim2      (int)       : second size of rmatrix_in (the number of times defined)
  * @endcond
  */
  extern "C" void RBDY3_addDrvDof(int i_bdyty, bool vlocy, int i_dof, double rvec6[6], double * rmatrix_in=nullptr, int rdim1=0, int rdim2=0);

 /**
  * @fn void RBDY3_synchronize(void)
  * @brief Update entity list from the RBDY3
  */
  extern "C" void RBDY3_synchronize(void);

 /**
  * @fn double RBDY3_GetMass(int ibdyty)
  * @brief Get the mass of a body 
  *
  * @cond PYDOC
  * python usage : mass = RBDY3_GetMass(ibdyty)
  * @param[in] ibdyty (integer) : rank of the RBDY3
  * @return    mass (double)    : mass of the RBDY3
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int)  : rank of the RBDY3
  * @return    mass(double) : mass of the RBDY3
  * @endcond
  */
  extern "C" double RBDY3_GetMass(int ibdyty);
  
 /**
  * @fn void RBDY3_GetAllMass(double** r8_vector, int* r8_size)
  * @brief Get the mass of a all body (visible and invisible)
  *
  * @cond PYDOC
  * python usage : masses = RBDY3_GetAllMass()
  * @return    masses (double array) : masses of all RBDY3
  * @endcond
  *
  * @cond CDOC
  * @param[out] r8_vector (double**) : the output vector
  * @param[out] r8_size (int*)       : the size of output vector
  * @endcond
  */
  extern "C" void RBDY3_GetAllMass(double** r8_vector, int* r8_size);

 //---vt---
 /**
  * @fn void RBDY3_GetPtrMass(int ibdyty, double ** mass)
  * @brief Get a pointer onto the mass matrix of a body 
  *
  * @param[in] ibdyty(int) : index of the RBDY3
  * @param[out] mass(double**) : mass matrix of the RBDY3
  */
  extern "C" void RBDY3_GetPtrMass(int ibdyty, double ** mass);
  
 /**
  * @fn void RBDY3_GetVelocity(int ibdyty, double * velocity)
  * @brief Get the velocity of a body 
  *
  * @param[in] ibdyty(int) : index of the RBDY3
  * @param[out] velocity(double[6]) : velocity of the RBDY3
  */
  extern "C" void RBDY3_GetVelocity(int ibdyty, double * velocity);
  
  /* /\** */
  /* * @fn void RBDY3_CompGlobInertia(int ibdyty, double * GlobInert) */
  /* * @brief compute the global inertia */
  /* * */
  /* * @param[in] int ibdyty : index of the RBDY3 */
  /* * @param[out] double * GlobInert : Global Inertia vector of the RBDY3 */
  /* *\/ */
  /* extern "C" void RBDY3_CompGlobInertia(int ibdyty, double * GlobInert); */

  /**
  * @fn void RBDY3_GetGlobInertia(int ibdyty, double** matrix_out, int* dim1, int* dim2)
  * @brief Get the global inertia
  *
  * @cond PYDOC
  * usage : inertia = RBDY3_GetGlobInertia(ibdyty)
  * @param[in] ibdyty  (integer)         : id of desired RBDY3
  * @return    inertia (double 2D array) : the inertia matrix
  * @endcond
  *
  * @cond CDOC
  * @param[in]     ibdyty (int)           : id of RBDY3
  * @param[in,out] matrix_out (double **) : reference on the inetia matrix
  * @param[in]     dim1 (int*)            : the first dimension of matrix_out array (3)
  * @param[in]     dim2 (int*)            : the second dimension of matrix_out array (3)
  * @endcond
  */
  extern "C" void RBDY3_GetGlobInertia(int ibdyty, double ** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void RBDY3_GetBehavior(int ibdyty, char** c5)
  * @brief Get the type of the nickname of the behavior
  *
  * @cond PYDOC
  * usage name = RBDY3_GetBehavior(ibdyty)
  * @param[in] ibdyty (integer) : rank of the RBDY3 in container
  * @return type (string) : nickname
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : rank of the RBDY3 in container
  * @param[out] c5 (char*[5]) : nickname
  * @endcond
  */
  extern "C" void RBDY3_GetBehavior(int ibdyty, char** c5);

 /**
  * @fn int RBDY3_GetNbContactor(int ibdyty)
  * @brief get the number of contactor of RBDY3
  *
  * @cond PYDOC
  * python usage : nb = RBDY3_GetNbContactor(ibdyty)
  *
  * @param[in] ibdyty (integer) : rank of the RBDY3 in container
  * @return nb (integer) : number of contactor attached to a RBDY3
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : rank of the RBDY3 in container
  * @return (int) number of RBDY3 in container
  * @endcond
  */
  extern "C" int RBDY3_GetNbContactor(int ibdyty);

 /**
  * @fn void RBDY3_GetContactorType(int ibdyty, char** c5, int itacty=1)
  * @brief Get the type of the itacty contactor of a body ibdyty
  *
  * @cond PYDOC
  * usage type = RBDY3_GetContactorType(ibdyty,itacty=1)
  * @param[in] ibdyty (integer) : rank of the RBDY3 in container
  * @param[in] itacty (integer) : rank of the contactor in the RBDY3 (default to 1)
  * @return type (string) : type of the contactor of the body
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : rank of the RBDY3 in container
  * @param[in] itacty (int) : rank of the contactor in the RBDY3 (default to 1)
  * @param[out] c5 (char*[5]) : type of the contactor of the body
  * @endcond
  */
  extern "C" void RBDY3_GetContactorType(int ibdyty, char** c5, int itacty=1);

 /**
  * @fn void RBDY3_SetContactorColor(int ibdyty, char * cvalue1, int itacty=0)
  * @brief Set the color of a given contactor of a body (all of them in not specified)
  *
  * @cond PYDOC
  * usage : RBDY3_SetContactorColor(ibdyty, itacty, color)
  * @param[in] ibdyty (integer)            : rank of the RBDY3
  * @param[in] color (string of size 5)    : the color 
  * @param[in] itacty (integer)            : rank of the contactor in the RBDY3 (default to 0)
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int)                : rank of the RBDY3 in container
  * @param[in] cvalue1 (char[5])           : the color 
  * @param[in] itacty (int)                : rank of the contactor in the RBDY3 (default to 0)
  * @endcond
  *
  *
  */
  extern "C" void RBDY3_SetContactorColor(int ibdyty, char * cvalue1, int itacty=0);


/**
  * @fn void RBDY3_GetContactorColor(int ibdyty, char** c5, int itacty=1)
  * @brief Get the color of the itacty contactor of a body ibdyty
  *
  * @cond PYDOC
  * usage color = RBDY3_GetContactorColor(ibdyty,itacty=1)
  * @param[in] ibdyty (integer) : rank of the RBDY3 in container
  * @param[in] itacty (integer) : rank of the contactor in the RBDY3
  * @return color (string) : color of the contactor of the body
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : rank of the RBDY3 in container
  * @param[in] itacty (int) : rank of the contactor in the RBDY3
  * @param[out] c5 (char*[5]) : color of the contactor of the body
  * @endcond
  */
  extern "C" void RBDY3_GetContactorColor(int ibdyty, char** c5, int itacty=1);
  
 /**
  * @fn void RBDY3_getDrvVlocy(int ibdyty, int** i4_vector, int* i4_size, double** r8_vector, int* r8_size);
  * @brief Get the driven dof of a body
  *
  * @cond PYDOC
  * python usage : [drvdof_indices, drvdof_values] = RBDY3_getDrvVlocy(ibdyty)
  * @param[in] ibdyty (integer) : index of the RBDY3
  * @param drvdof_indices (integer array) : indices list of driven dof
  * @param drvdof_values  (real array) : values of the driven dof
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the RBDY3
  * @param[out] i4_vector (int**) : reference onto the indices list of driven dofs
  * @param[out] i4_size   (int*)  : size of the array referenced by i4_vector
  * @param[out] r8_vector (double**) : reference onto the values of driven dofs
  * @param[out] r8_size   (int*)  : size of the array referenced by r8_vector
  * @endcond
  */
  extern "C" void RBDY3_getDrvVlocy(int ibdyty, int** i4_vector, int* i4_size, double** r8_vector, int* r8_size);
  
 /**
  * @fn void RBDY3_computeDrvVlocy(int ibdyty, double * rvector_in, int rlength_in);
  * @brief Compute the value of the driven velocity of a body at current time
  *
  * In place replacement in the input array of the new value(s) of the driven velocity
  *
  * @cond PYDOC
  * python usage : RBDY3_computeDrvVlocy(ibdyty, values)
  * @param[in] ibdyty (integer) : index of the RBDY3
  * @param[in,out] values (double array) : numpy array, input old values of imposed velocity, output new ones
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : index of the RBDY3
  * @param[in,out] vector_in (double *) : input old values of imposed velocity, output new ones
  * @param[in] length (int) : size of vector_in array
  * @endcond
  */
  extern "C" void RBDY3_computeDrvVlocy(int ibdyty, double * rvector_in, int rlength_in);

 /**
  * @fn void RBDY3_LoadThreadNetwork(void);
  * @brief read thread structure for textile structure 
  *
  *
  * @cond PYDOC
  * python usage : RBDY3_LoadThreadNetwork(void); 
  * @endcond
  *
  * @cond CDOC
  * @endcond
  */
  extern "C" void RBDY3_LoadThreadNetwork(void);


 /**
  * @fn void RBDY3_SetInvisibleSmallObjects(double radius)
  * @brief Set the objects to invisible if their average radius is less than radius
  *
  * @cond PYDOC
  * python usage : RBDY3_SetInvisibleSmallObjects(radius)
  * @param[in] radius (double) : radius threshold
  * @endcond
  *
  * @cond CDOC
  * @param[in] radius (double) : radius threshold
  * @endcond
  */
  extern "C" void RBDY3_SetInvisibleSmallObjects(double radius);

 /**
  * @fn void  RBDY3_SetVisibleVlocyDrivenDof(int ibdyty, int iccdof)
  * @brief rended a given Velocy DOF visible
  *
  * @cond PYDOC
  * python usage : RBDY3_SetVisibleVlocyDrivenDof(ibdyty, iccdof)
  * @param[in] ibdyty(integer) : index of the RBDY3
  * @param[in] iccdof(integer) : index of the DOF to set visible
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the RBDY3
  * @param[in] iccdof(int) : index of the DOF to set visible
  * @endcond
 */
 extern "C" void RBDY3_SetVisibleVlocyDrivenDof(int ibdyty, int iccdof);

 /**
  * @fn void void RBDY3_SetInvisibleVlocyDrivenDof(int ibdyty, int iccdof)
  * @brief rended a given Velocy DOF invisible
  *
  * @cond PYDOC
  * python usage : RBDY3_SetInvisibleVlocyDrivenDof(ibdyty, iccdof)
  * @param[in] ibdyty(integer) : index of the RBDY3
  * @param[in] iccdof(integer) : index of the DOF to set invisible
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : index of the RBDY3
  * @param[in] iccdof(int) : index of the DOF to set invisible
  * @endcond
 */
 extern "C" void RBDY3_SetInvisibleVlocyDrivenDof(int ibdyty, int iccdof);


 /**
  * @fn void RBDY3_PartialDamping(int nb_steps, double Vmax)
  * @brief Limit body velocity to Vmax value
  *
  * @cond PYDOC
  * usage : RBDY3_PartialDamping(nb_steps, Vmax)
  * @param[in] nb_steps (integer) : periodicity
  * @parma[in] Vmax (double)      : Vmax
  * @endcond
  *
  * @cond CDOC
  * @param[in] nb_steps (int) :
  * @parma[in] Vmax (double)  : Vmax
  * @endcond
  */
  extern "C" void RBDY3_PartialDamping(int nb_steps, double Vmax);

 /**
  * @fn void RBDY3_GetVolume(int ibdyty, double * res)
  * @brief Get volume of a body
  *
  * @cond PYDOC
  * usage : volume = RBDY3_GetVolume(ibdyty)
  * @param[in] ibdyty (integer) : RBDY3 id
  * @return volume (double)     : volume
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : RBDY3 id
  * @parma[out] res (double*)  : volume
  * @endcond
  */
  extern "C" void RBDY3_GetVolume(int ibdyty, double * res);

 /**
  * @fn void RBDY3_GetAllVolume(double** r8_vector, int* r8_size)
  * @brief Get the area of a all body (visible and invisible)
  *
  * @cond PYDOC
  * python usage : area = RBDY3_GetAllVolume()
  * @return    area (double array) : masses of all RBDY2
  * @endcond
  *
  * @cond CDOC
  * @param[out] r8_vector (double**) : the output vector
  * @param[out] r8_size (int*)       : the size of output vector
  * @endcond
  */
  extern "C" void RBDY3_GetAllVolume(double** r8_vector, int* r8_size);

 /**
  * @fn void RBDY3_RenumVisibleBodies(void)
  * @brief give a new numerotation of visible bodies 
  *
  * @cond PYDOC
  * python usage : RBDY3_RenumVisibleBodies()
  * @endcond
 */
 extern "C" void RBDY3_RenumVisibleBodies(void);

 /**
  * @fn void RBDY3_GetBulkBehavNumber(int ibdyty)
  * @brief return the bulk number of a given RBDY3
  *
  * @cond PYDOC
  * python usage : ibehav = RBDY3_GetBulkBehavNumber(ibdyty)
  * @param[in] ibdyty (integer) : rank of a RBDY3
  * @return    ibehav (integer) : the bulk behav number
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int) : rank of a RBDY3
  * @return     blmID (int) : the bulk behav number
  * @endcond
  */
  extern "C" int RBDY3_GetBulkBehavNumber(int ibdyty);

/**
   * @fn void RBDY3_CleanMemory(void)
   * @brief Free all memory allocated within RBDY3 module
   *
   * @cond PYDOC
   * python usage : RBDY3_CleanMemory()
   * @endcond
   */
   extern "C" void RBDY3_CleanMemory(void);


//
//  Multi physique
//

 /**
  * @fn void RBDY3_LoadMpBehaviours(double disper)
  * @brief read extra physical behaviour in BULK_BEHAV.DAT file.
  *
  * Must be used for THERMO_RIGID ELECTRO_RIGID and 
  * THERMO_ELECTRO_RIGID behaviour
  *
  * @cond PYDOC
  * python usage : RBDY3_LoadMpBehaviours(disper)
  * @param[in] disper(double) : some dispersion coefficient
  * @endcond
  *
  * @cond CDOC
  * @param[in] disper(double) : some dispersion coefficient
  * @endcond
 */
 extern "C" void RBDY3_LoadMpBehaviours(double disper);

 /**
  * @fn void RBDY3_IncrementWSvsT(void)
  * @brief
  *
  * @cond PYDOC
  * python usage : RBDY3_IncrementWSvsT()
  * @endcond
 */
 extern "C" void RBDY3_IncrementWSvsT(void);

 /**
  * @fn void RBDY3_UpdateGAMMAvsT(void)
  * @brief
  *
  * @cond PYDOC
  * python usage : RBDY3_UpdateGAMMAvsT()
  * @endcond
 */
 extern "C" void RBDY3_UpdateGAMMAvsT(void);

 /**
  * @fn double RBDY3_GetThermalValue(int ibdyty, int itacty)
  * @brief Get temperature of rigid particle
  *
  * @cond PYDOC
  * usage : T = RBDY3_GetThermalValu(ibdyty, itacty)
  * @param[in] ibdyty (integer)            : rank of body
  * @param[in] itacty (integer)            : rank of tacty
  *"
 */
 extern "C" double RBDY3_GetThermalValue(int ibdyty, int itacty);

 /**
  * @fn void RBDY3_SetEquilibriumNorm(char * norm_type , double tolerance)
  * @brief Initialization of data for the equilibrium state check
  *
  * You must precise the type of check test :
  * - Qvlcy : quadratic norm velocy
  * - Mvlcy : maximum   norm velocy
  *
  * @cond PYDOC
  * usage : RBDY3_CheckEquilibrium(norm_type , tolerance)
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
 extern "C" void RBDY3_SetEquilibriumNorm(char * norm_type, double tolerance);

 /**
  * @fn bool RBDY3_CheckEquilibriumState(void)
  * @brief  check if all the RBDY3 rich an equilibrium state (velocity is almost equal to zero)
  *
  * @cond PYDOC
  * usage : isBalanced = RBDY3_CheckEquilibriumState()
  *
  * @return isBalanced (boolean) : True if in equilibrium state 
  * @endcond
  *
  * @cond CDOC
  * @return isBalanced (bool) : True if in equilibrium state 
  * @endcond
 */
 extern "C" bool RBDY3_CheckEquilibriumState(void);

//
//  A partir de là : à virer ! 
//

 /**
  * @fn void RBDY3_InitFreeBoundary(double xmin, double xmax, double ymin, double y_max, double radius)
  * @brief 
  *
  * @cond PYDOC
  * python usage : RBDY3_InitFreeBoundary(xmin, xmax, ymin, ymax, radius)
  * @param[in] xmin   (real) :
  * @param[in] xmax   (real) :
  * @param[in] ymin   (real) :
  * @param[in] ymax   (real) :
  * @param[in] radius (real) :
  * @endcond
  *
  * @cond CDOC
  * @param[in] xmin   (double) :
  * @param[in] xmax   (double) :
  * @param[in] ymin   (double) :
  * @param[in] ymax   (double) :
  * @param[in] radius (double) :
  * @endcond
  */
  extern "C" void RBDY3_InitFreeBoundary(double xmin, double xmax, double ymin, double ymax, double radius);

/**
  * @fn void RBDY3_TriaxialLoading(int num_down, int num_right, int num_up, int num_left, int num_front, int num_rear, int nb_loads, double * rvector_in, int rlength_in)
  * @brief Triaxial load of a sample using a rigid box
  * @cond PYDOC
  * python usage : TriaxialLoading(num_down, num_right, num_up, num_left, num_front, num_rear, nb_loads, loads)
  * @param[in] num_down  (integer) :
  * @param[in] num_right (integer) :
  * @param[in] num_up    (integer) :
  * @param[in] num_left  (integer) :
  * @param[in] num_front (integer) :
  * @param[in] num_rear  (integer) :
  * @param[in] nb_loads  (integer) : the number of walls you want to load with a pressure (1 to 6)
  * @param[in] loads     (array)   : loads(2,nb_loads): load(1,i) contains which wall is loaded (1==down, 2==right, 3==up, 4==left, 5==front, 6==rear) and load(2,i) contains the amplitude of the stress (a positive value means compression).
  *
  */
  extern "C" void RBDY3_TriaxialLoading(int num_down , int num_right, int num_up, int num_left, int num_front, int num_rear, int nb_loads, double * rvector_in, int rlength_in);


 /**
  * @fn int RBDY3_GetDofStatus(int ibdyty)
  * @brief Get dof status 
  *
  * @cond PYDOC
  * python usage : status = RBDY3_GetDofStatus(ibdyty)
  * @param[in] ibdyty(integer) : rank of the RBDY3
  * @return    status(integer) : dof status of the RBDY3
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : rank of the RBDY3
  * @return    status(int) : dof status of the RBDY3
  * @endcond
  */
  extern "C" int RBDY3_GetDofStatus(int ibdyty);



#endif /* wrap_RBDY3_h */
