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

#ifndef wrap_therMAILx_h
#define wrap_therMAILx_h

 /**
  * @fn int therMAILx_GetNbTherMAILx(void)
  * @brief Get the number of therMAILx
  *
  * @cond PYDOC
  * python usage : nb_therMAILx = therMAILx_GetNbTherMAILx()
  *
  * @return nb_therMAILx (integer) : number of therMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of therMAILx
  * @endcond
  */
  extern "C" int therMAILx_GetNbTherMAILx(void);

 /**
  * @fn int therMAILx_GetNbNodes(int ivalue)
  * @brief Get the number of nodes of a therMAILx
  *
  * @cond PYDOC
  * python usage : nb_nodes = therMAILx_GetNbNodes(ibdyty)
  * @param[in] ivalue (integer) : id of the therMAILx
  * @return nb_nodes (integer) : number of nodes of a therMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of nodes
  * @endcond
  */
  extern "C" int therMAILx_GetNbNodes(int ivalue);

 /**
  * @fn int therMAILx_GetNbElements(int ivalue)
  * @brief Get the number of nodes of a therMAILx
  *
  * @cond PYDOC
  * python usage : nb_nodes = therMAILx_GetNbElements(ibdyty)
  * @param[in] ivalue (integer) : id of the therMAILx
  * @return nb_nodes (integer) : number of nodes of a therMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of nodes
  * @endcond
  */
  extern "C" int therMAILx_GetNbElements(int ivalue);

 /**
  * @fn int therMAILx_GetNbDofs(int)
  * @brief Get the number of dofs for the therMAILX 
  *
  * @cond PYDOC
  * python usage : nb_dofs = therMAILx_GetNbDofs(int ibdyty)
  *
  * @return nb_dofs (integer) : number of dofs of the body for the model
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of therMAILx
  * @endcond
  */

  extern "C" int therMAILx_GetNbDofs(int);

 /**
  * @fn void therMAILx_IncrementStep(void)
  * @brief initializes current dof 
  *
  * @cond PYDOC
  * python usage : therMAILx_IncrementStep()
  * @endcond
  */
  extern "C" void therMAILx_IncrementStep(void);

 /**
  * @fn void therMAILx_ComputeConductivity(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes the elementary conductivity matrices of a list of bodies
  *
  * If the input list is empty, the conductivities of all bodies will be computed
  *
  * @cond PYDOC
  * python usage : therMAILx_ComputeConductivity(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute conductivities
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of therMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void therMAILx_ComputeConductivity(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void therMAILx_ComputeCapacity(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes the elemetary capacity matrices
  *
  * @cond PYDOC
  * python usage : therMAILx_ComputeCapacity(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute conductivities
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of therMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void therMAILx_ComputeCapacity(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void therMAILx_ComputeConvection(int * ivector_in=nullptr, int ilength_in=0)
  * @brief compute elementary convection terms
  *
  * @cond PYDOC
  * python usage : therMAILx_ComputeConvection(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute conductivities
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of therMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void therMAILx_ComputeConvection(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void therMAILx_ComputeInternalFlux(int * ivector_in=nullptr, int ilength_in=0)
  * @brief compute elementary internal flux
  *
  * @cond PYDOC
  * python usage : therMAILx_ComputeInternalFlux(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute conductivities
  *            if omitted works on all objects
  * @endcond
  * 
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of therMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void therMAILx_ComputeInternalFlux(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void therMAILx_ComputeExternalFlux(int * ivector_in=nullptr, int ilength_in=0)
  * @brief compute elementary external flux
  *
  * @cond PYDOC
  * python usage : therMAILx_ComputeExternalFlux(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute conductivities
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of therMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void therMAILx_ComputeExternalFlux(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void therMAILx_AssembThermKT(int * ivector_in=nullptr, int ilength_in=0)
  * @brief assembles elementary matrices
  *
  * @cond PYDOC
  * python usage : therMAILx_AssembKT(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute conductivities
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of therMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void therMAILx_AssembThermKT(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void therMAILx_AssembThermRHS(int * ivector_in=nullptr, int ilength_in=0)
  * @brief assembles elementary vectors
  *
  * @cond PYDOC
  * python usage : therMAILx_AssembRHS(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute conductivities
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of therMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void therMAILx_AssembThermRHS(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void therMAILx_ComputeThermDof(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes current dof
  *
  * @cond PYDOC
  * python usage : therMAILx_ComputeThermDof(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute conductivities
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of therMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void therMAILx_ComputeThermDof(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void therMAILx_ComputeThermFields(int * ivector_in=nullptr, int ilength_in=0)
  * @brief computes elementary fields 
  *
  *
  * @cond PYDOC
  * python usage : therMAILx_ComputeThermFields(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute conductivities
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of therMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void therMAILx_ComputeThermFields(int * ivector_in=nullptr, int ilength_in=0);


 /**
  * @fn void therMAILx_UpdateThermDof(int * ivector_in=nullptr, int ilength_in=0)
  * @brief update begin dof with current dof
  *
  * @cond PYDOC
  * python usage : therMAILx_UpdateThermDof(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute conductivities
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of therMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void therMAILx_UpdateThermDof(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void therMAILx_UpdateThermBulk(int * ivector_in=nullptr, int ilength_in=0)
  * @brief update begin elementary fields with current elementary fields
  *
  *
  * @cond PYDOC
  * python usage : therMAILx_UpdateThermBulk(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute conductivities
  *            if omitted works on all objects
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of therMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void therMAILx_UpdateThermBulk(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn double therMAILx_ComputeResidueNorm(int * ivector_in=nullptr, int ilength_in=0)
  * @brief compute the residue of the thermal equation
  *
  * @cond PYDOC
  * python usage : norm = therMAILx_ComputeResidueNorm(i_list)
  * @param[in] i_list (list of integer) : list of bodies to compute conductivities
  *            if omitted works on all objects
  * @return norm (double) : value of the norm
  * @endcond
  *
  * @cond CDOC
  * @return (double) value of the norm
  * @param[in] vector_in (int*) : list of indices of therMAILx
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" double therMAILx_ComputeResidueNorm(int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void therMAILx_ReadDrivenDof(void)
  * @brief Read DRV_DOF.DAT
  *
  * @cond PYDOC
  * python usage : therMAILx_ReadDrivenDof()
  * @endcond
  */
  extern "C" void therMAILx_ReadDrivenDof(void);

 /**
  * @fn void therMAILx_WriteDrivenDof(void)
  * @brief Write DRV_DOF.OUT
  *
  * @cond PYDOC
  * python usage : therMAILx_WriteDrivenDof()
  * @endcond
  */
  extern "C" void therMAILx_WriteDrivenDof(void);

 /**
  * @fn void therMAILx_LoadModels(void)
  * @brief loads models frol models module
  *
  * @cond PYDOC
  * python usage : therMAILx_LoadModels()
  * @endcond
  */
  extern "C" void therMAILx_LoadModels(void);

 /**
  * @fn void therMAILx_LoadBehaviours(void)
  * @brief loads bulk behaviors parameters from bulk_behav module
  *
  * @cond PYDOC
  * python usage : therMAILx_LoadBehaviours()
  * @endcond
  */
  extern "C" void therMAILx_LoadBehaviours(void);

 /**
  * @fn void therMAILx_ReadIniDof(int num=0)
  * @brief Read DOF file
  *
  * If num <= 0 : DATBOX/DOF.INI file is read
  * Else : OUTBOX/DOF.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniDof
  * last call
  *
  * @cond PYDOC
  * python usage : therMAILx_ReadIniDof(num=0)
  * @param[in] num (integer) : which DOF file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which DOF file to read
  * @endcond
  *
  */
  extern "C" void therMAILx_ReadIniDof(int num=0);

 /**
  * @fn void therMAILx_ReadIniGPV(int num=0)
  * @brief Read GPV file
  *
  * If num <= 0 : DATBOX/GPV.INI file is read
  *
  * Else : OUTBOX/GPV.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniGPV
  * last call
  *
  * @cond PYDOC
  * python usage : therMAILx_ReadIniGPV(num=0)
  * @param[in] num (integer) : which GPV file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which GPV file to read
  * @endcond
  *
  */
  extern "C" void therMAILx_ReadIniGPV(int num=0);

 /**
  * @fn void therMAILx_WriteLastDof(void)
  * @brief Write ascii DOF.LAST file
  *
  * @cond PYDOC
  * python usage : therMAILx_WriteLastDof()
  * @endcond
  */
  extern "C" void therMAILx_WriteLastDof(void);

 /**
  * @fn void therMAILx_WriteOutDof(void)
  * @brief Write ascii DOF.OUT file. Can be activate only each N step
  *
  * @cond PYDOC
  * python usage : therMAILx_WriteOutDof()
  * @endcond
  */
  extern "C" void therMAILx_WriteOutDof(void);

 /**
  * @fn void therMAILx_DisplayOutDof(void)
  * @brief Display body degrees of freedom
  *
  * @cond PYDOC
  * python usage : therMAILx_DisplayOutDof()
  * @endcond
  */
  extern "C" void therMAILx_DisplayOutDof(void);

 /**
  * @fn void therMAILx_PutBodyVector(char * cvalue1_c, int ivalue1, double * matrix_in, int dim1, int dim2)
  * @brief Set a vector of a given body
  *
  * Uses copy
  *
  * @cond PYDOC
  * python usage : therMAILx_PutBodyVector(datatype, ibdyty, matrix)
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
  * - "T____": Temperature in computed configuration
  * - "Tbeg_": Temperature at beginning of time step
  * - "Taux_": Temperature in working array
  * - "Fext_": external flux
  * - "Fint_": internal flux
  *
  */
  extern "C" void therMAILx_PutBodyVector(char * cvalue1_c, int ivalue1, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void therMAILx_GetBodyVector(char * cvalue1_c, int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of a vector of a given body
  *
  * Uses copy
  *
  * @cond PYDOC
  * python usage : vector = therMAILx_GetBodyVector(datatype, ibdyty)
  * @param datatype (string of size 5) : the vector to get
  * @param ibdyty (integer)            : rank of considered body
  * @return vector (double 2D-array)   : the desired vector
  * @endcond
  *
  * @cond CDOC
  * @param[in]  cvalue1 (char[5])     : the vector to get
  * @param[in]  ivalue1 (int)         : id of considered body
  * @param[out] matrix_out (double**) : the data to get
  * @param[out] dim1 (int*)           : first dimension of matrix_out
  * @param[out] dim2 (int*)           : second dimension of matrix_out
  * @endcond
  *
  * Possible values for datatype field are:
  * - "Coor0": reference coordinates
  * - "T____": Temperature in computed configuration
  * - "Tbeg_": Temperature at beginning of time step
  * - "Taux_": Temperature in working array
  * - "Fext_": external flux
  * - "Fint_": internal flux
  *
  */
  extern "C" void therMAILx_GetBodyVector(char * cvalue1_c, int ivalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn int therMAILx_GetScalarFieldRank(int ibdyty, int iblmty, char* field_name);
  * @brief Get the rank of field of an element of a body from its name
  *
  * @cond PYDOC
  * python usage : f_rank = therMAILx_GetScalarFieldRank(ibdyty, iblmty, name)
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
  extern "C" int therMAILx_GetScalarFieldRank(int ibdyty, int blmty, char* name);

 /**
  * @fn void therMAILx_SetScalarFieldByNode(int IdBody, int f_rank, double * rvector_in, int rlength_in)
  * @brief Update an  external field on a given body
  *
  * You need to set this field in your models.dat
  *
  * @cond PYDOC
  * python usage : therMAILx_SetScalarFieldByNode(IdBody, f_rank, f)
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
  extern "C" void therMAILx_SetScalarFieldByNode(int IdBody, int f_rank, double * rvector_in, int rlength_in);

 /**
  * @fn void therMAILx_SetScalarFieldByElement(int IdBody, int f_rank, double * rvector_in, int rlength_in)
  * @brief Update elementary scalar field through a element external field on a given body
  *
  * Field values are stored at Gauss point, on an element all Gauss point have the element value
  *
  * You need to declare this field in your MODELS.DAT
  *
  * @cond PYDOC
  * python usage : therMAILx_SetScalarFieldByElement(IdBody, f_rank, f)
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
  extern "C" void therMAILx_SetScalarFieldByElement(int IdBody, int f_rank, double * rvector_in, int rlength_in);

 /**
  * @fn int therMAILx_GetVectorFieldRank(int ibdyty, int iblmty, char* field_name);
  * @brief Get the rank of field of an element of a body from its name
  *
  * @cond PYDOC
  * python usage : f_rank = therMAILx_GetVectorFieldRank(ibdyty, iblmty, name)
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
  extern "C" int therMAILx_GetVectorFieldRank(int ibdyty, int blmty, char* name);

 /**
  * @fn void therMAILx_SetVectorFieldByNode(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2)
  * @brief Update elementary fields through a nodal external field on a given body
  *
  * Use the form functions of the elements and input values to compute and store field values
  * at Gauss points.
  *
  * You need to declare this field in your MODELS.DAT
  *
  * @cond PYDOC
  * python usage : therMAILx_SetFieldByNode(IdBody, f_rank, f)
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
  extern "C" void therMAILx_SetVectorFieldByNode(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void therMAILx_SetVectorFieldByElement(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2)
  * @brief Update elementary fields through a nodal external field on a given body
  *
  * Use the form functions of the elements and input values to compute and store field values
  * at Gauss points.
  *
  * You need to declare this field in your MODELS.DAT
  *
  * @cond PYDOC
  * python usage : therMAILx_SetFieldByElement(IdBody, f_rank, f)
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
  extern "C" void therMAILx_SetVectorFieldByElement(int ibdyty, int f_rank, double * matrix_in, int dim1, int dim2);

 /**
  * @fn void therMAILx_AddSource(int ivalue1, int ivalue2)
  * @brief Add a volumic source into a given body
  *
  * @cond PYDOC
  * python usage : therMAILx_AddSource(ibdyty, ifield)
  * @param[in] ibdyty (integer)            : rank of body
  * @param[in] ifield (integer)            : rank of field
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)        : id of considered body
  * @param[in] ivalue2 (int)        : id of considered field
  * @endcond
  */
  extern "C" void therMAILx_AddSource(int ivalue1, int ivalue2);

 /**
  * @fn void therMAILx_AddNodalFieldDivergence(int ivalue1, int ivalue2)
  * @brief Add the divergence of a field to external flux
  *
  * @cond PYDOC
  * python usage : therMAILx_AddNodalFieldDivergence(ibdyty, ifield)
  * @param[in] ibdyty (integer)            : rank of body
  * @param[in] ifield (integer)            : rank of field
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)        : id of considered body
  * @param[in] ivalue2 (int)        : id of considered field
  * @endcond
  */
  extern "C" void therMAILx_AddNodalFieldDivergence(int ivalue1, int ivalue2);

 /**
  * @fn void therMAILx_PushProperties(void)
  * @brief declares to module model the couples (model,behavior) used
  *
  * @cond PYDOC
  * python usage : therMAILx_PushProperties()
  * @endcond
  */
  extern "C" void therMAILx_PushProperties(void);

 /**
  * @fn void therMAILx_WithoutRenumbering(void)
  * @brief skip renumbering of the unknowns using a rcc method 
  *
  * @cond PYDOC
  * python usage : therMAILx_WithoutRenumbering()
  * @endcond
  */
  extern "C" int therMAILx_WithoutRenumbering(void);

 /**
  * @fn void therMAILx_BandStorage(void)
  * @brief use band matrix 
  *
  * @cond PYDOC
  * python usage : therMAILx_BandStorage()
  * @endcond
  */
  extern "C" int therMAILx_BandStorage(void);

 /**
  * @fn void therMAILx_SparseStorage(void)
  * @brief use sparse matrix
  *
  * @cond PYDOC
  * python usage : therMAILx_SparseStorage()
  * @endcond
  */
  extern "C" int therMAILx_SparseStorage(void);

 /**
  * @fn void therMAILx_ExplodedStorage(void)
  * @brief use element by element matrix 
  *
  * @cond PYDOC
  * python usage : therMAILx_ExplodedStorage()
  * @endcond
  */
  extern "C" int therMAILx_ExplodedStorage(void);

 /**
  * @fn void therMAILx_DiagonalStorage(void)
  * @brief use diagonal matrix
  *
  * @cond PYDOC
  * python usage : therMAILx_DiagonalStorage()
  * @endcond
  */
  extern "C" int therMAILx_DiagonalStorage(void);

 /**
  * @fn void therMAILx_SkylineStorage(void)
  * @brief use skyline matrix
  *
  * @cond PYDOC
  * python usage : therMAILx_SkylineStorage()
  * @endcond
  */
  extern "C" int therMAILx_SkylineStorage(void);

 /**
  * @fn void therMAILx_FullStorage(void)
  * @brief use full matrix
  *
  * @cond PYDOC
  * python usage : therMAILx_FullStorage()
  * @endcond
  */
  extern "C" int therMAILx_FullStorage(void);

 /**
  * @fn void therMAILx_SymmetricShape(void)
  * @brief assume matrix is symmetrical
  *
  * @cond PYDOC
  * python usage : therMAILx_SymmetricShape()
  * @endcond
  */
  extern "C" int therMAILx_SymmetricShape(void);

 /**
  * @fn void therMAILx_UnspecifiedShape(void)
  * @brief does not assume any thing on matrix shape
  *
  * @cond PYDOC
  * python usage : therMAILx_UnspecifiedShape()
  * @endcond
  */
  extern "C" int therMAILx_UnspecifiedShape(void);



   /**
  * @fn void therMAILx_GetGrad( int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of a gradient of a given body
  *
  * @cond PYDOC
  * Python usage : grad_T = therMAILx_GetGrad(ibdyty)
  * @param ibdyty (integer)          : rank of considered body
  * @return grad_T (double 2D-array) : the desired gradient
  * @endcond
  *
  * @cond CDOC
  * @param[in]  ivalue1 (int)         : id of considered body
  * @param[out] matrix_out (double**) : desired gradient
  * @param[out] dim1 (int*)           : the first dimension of matrix_out
  * @param[out] dim2 (int*)           : the second dimension of matrix_out
  * @endcond
  */
  extern "C" void therMAILx_GetGrad(int ivalue1, double** matrix_out, int* dim1, int* dim2);

   /**
  * @fn void therMAILx_GetFlux( int ivalue1, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of a gradient of a given body
  *
  * @cond PYDOC
  * Python usage : Flux_T = therMAILx_GetFlux(ibdyty)
  * @param ibdyty (integer)       : rank of considered body
  * @return Flux_T (double array) : the desired flux
  * @endcond
  *
  * @cond CDOC
  * @param[in]  ivalue1 (int)         : id of considered body
  * @param[out] matrix_out (double**) : the vector to get, must be of the right size
  * @param[out] dim1 (int*)           : first dimension of matrix_out
  * @param[out] dim2 (int*)           : second dimension of matrix_out
  * @endcond
  */
  extern "C" void therMAILx_GetFlux(int ivalue1, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void therMAILx_InitializeElementaryFlux(void)
  * @brief set elementary flux to 0
  *
  * @cond PYDOC
  * python usage : therMAILx_InitializeElementaryFlux()
  * @endcond
  */
  extern "C" void therMAILx_InitializeElementaryFlux(void);

 /**
  * @fn void therMAILx_GetCoor(int idBody,  double ** matrix_out, int * dim1, int * dim2)
  * @brief return node coordinates of idBody
  *
  * @cond PYDOC
  * python usage : array = therMAILx_GetCoor(idBody)
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
  extern "C" void therMAILx_GetCoor(int idBody, double ** matrix_out, int * dim1, int * dim2);

 /**
  * @fn void therMAILx_GetConnectivity(int idBody,  int ** i4_vector, int * i4_size)
  * @brief return connectivity of idBody elements
  *
  * @cond PYDOC
  * python usage : vector = therMAILx_GetConnectivity(idBody)
  * @param[in] IdBody (integer) : id of the concerned body
  * @return    vector (integer)  : connectivity
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)      : id of the concerned body
  * @param[out] i4_vector (int**) : connectivities
  * @param[out] i4_size (int *)   : size of i4_vector
  * @endcond
  */
  extern "C" void therMAILx_GetConnectivity(int idBody, int ** i4_vector, int * i4_size );

 /**
  * @fn void therMAILx_GetAll(int idBody,  double ** matrix_out, int * dim1, int * dim2)
  * @brief return mechanical data computed for idBody
  *
  * @cond PYDOC
  * python usage : array = therMAILx_GetAll(idBody)
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
  extern "C" void therMAILx_GetAll(int idBody, double ** matrix_out, int * dim1, int * dim2);

 /**
  * @fn void therMAILx_GetGpCoor(int idBody,  double ** matrix_out, int * dim1, int * dim2)
  * @brief return Gauss points coordinates of idBody
  *
  * @cond PYDOC
  * python usage : array = therMAILx_GetGpCoor(idBody)
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
  extern "C" void therMAILx_GetGpCoor(int idBody, double ** matrix_out, int * dim1, int * dim2);

 /**
  * @fn void therMAILx_GetGpField(int idBody,int idEle, int idGp, int idField, double** r8_vector, int* r8_size)
  * @brief return field values stored at a gp
  *
  * @cond PYDOC
  * python usage : field = therMAILx_GetGpField(idBody,idEle,idGp,idField)
  * @param[in] IdBody  (integer)  : id of the concerned body
  * @param[in] IdEle   (integer)  : id of the concerned element
  * @param[in] IdGp    (integer)  : id of the concerned gauss point
  * @param[in] IdField (integer)  : id of the concerned field
  * @return field (double array)  : field value
  * @endcond
  *
  * @cond CDOC
  * @param[in]  IdBody (int)         : id of the concerned body
  * @param[in]  IdEle  (int)         : id of the concerned element
  * @param[in]  IdGp   (int)         : id of the concerned gauss point 
  * @param[in]  IdField(int)         : id of the concerned gauss point 
  * @param[out] r8_vector (double**) : xxx
  * @param[out] r8_size (int*)       : dimension
  * @endcond
  */
  extern "C" void therMAILx_GetGpField(int idBody, int idEle, int idGp, int idField, double** r8_vector, int* r8_size );


 // elucider a quoi ca sert ... 

 /**
  * @fn void therMAILx_TrialAssembThermKT(void)
  * @brief [experimental]  assembles elementary matrices
  *
  * @cond PYDOC
  * python usage : therMAILx_AssembThermKT()
  * @endcond
  */
  extern "C" void therMAILx_TrialAssembThermKT(void);

 /**
  * @fn void therMAILx_TrialAssembThermRHS(void)
  * @brief [experimental]  assembles elementary vectors
  *
  * @cond PYDOC
  * python usage : therMAILx_AssembThermRHS()
  * @endcond
  */
  extern "C" void therMAILx_TrialAssembThermRHS(void);

/**
  * @fn void therMAILx_CleanMemory(void)
  * @brief Free all memory allocated within therMAILx module
  *
  * @cond PYDOC
  * python usage : therMAILx_CleanMemory()
  * @endcond
  */
  extern "C" void therMAILx_CleanMemory(void);


 /**
  * @fn void therMAILx_CheckProperties(void)
  * @brief check if model and material are matching ; set material parameter if external model
  *
  * @cond PYDOC
  * python usage : therMAILx_CheckProperties()
  * @endcond
  */
  extern "C" int therMAILx_CheckProperties(void);

 /**
  * @fn void therMAILx_GetNbGpByElem(char** string_vector, int* vector_size, int* string_size, , int** i4_vector, int* i4_size)
  * @brief Get the list of finite elements for therx models and the associated number of Gauss Points.
  *
  * Here memory is allocated within lmgc90 so that the pointer can be freely
  * modified by third parties without nasty effect on lmgc90 functioning.
  *
  * @cond PYDOC
  * python usage : names, nb_gps = therMAILx_GetNbGpByElem()
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
  extern "C" void therMAILx_GetNbGpByElem(char** string_vector, int* vector_size, int* string_size, int** i4_vector, int* i4_size);

 /**
  * @fn int therMAILx_GetNbGp(int ibdyty, int iblmty)
  * @brief Get the number of Gauss points of an element of a therMAILx
  *
  * @cond PYDOC
  * python usage : nb_gp = therMAILx_GetNbElements(ibdyty, iblmty)
  * @param[in] ibdyty (integer) : id of the therMAILx
  * @param[in] iblmty (integer) : id of the element
  * @return nb_gp (integer) : number of Gauss point of an element of a therMAILx
  * @endcond
  *
  * @cond CDOC
  * @return (int) number of Gauss points
  * @endcond
  */
  extern "C" int therMAILx_GetNbGp(int ibdyty, int iblmty);

#endif /* wrap_therMAILx_h */
