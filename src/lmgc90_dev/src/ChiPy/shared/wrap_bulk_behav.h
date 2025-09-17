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
 *==========================================================================*/

#ifndef wrap_bulk_behav_h
#define wrap_bulk_behav_h

/**
 * @fn void bulk_behav_ReadBehaviours(void)
 * @brief read gravity and behaviors from DATBOX/BULK_BEHAV.DAT file
 *
 * @cond PYDOC
 * python usage : bulk_behav_ReadBehaviours()
 * @endcond
 */
 extern "C" void bulk_behav_ReadBehaviours(void);

/**
 * @fn void bulk_behav_WriteBehaviours(void)
 * @brief write gravity and behaviors to OUTBOX/BULK_BEHAV.OUT file
 *
 * @cond PYDOC
 * python usage : bulk_behav_WriteBehaviours()
 * @endcond
 */
 extern "C" void bulk_behav_WriteBehaviours(void);

/**
 * @fn void bulk_behav_CollectOutBulkBehav(void)
 * @brief read gravity and behaviors from OUTBOX/BULK_BEHAV.OUT file
 *
 * @cond PYDOC
 * python usage : bulk_behav_CollectOutBulkBehav()
 * @endcond
 */
 extern "C" void bulk_behav_CollectOutBulkBehav(void);

/**
 * @fn void bulk_behav_CleanOutBulkBehav(void)
 * @brief write (replacing) gravity and behaviors to OUTBOX/BULK_BEHAV.OUT file
 *
 * @cond PYDOC
 * python usage : bulk_behav_CleanOutBulkBehav()
 * @endcond
 */
 extern "C" void bulk_behav_CleanOutBulkBehav(void);

/**
 * @fn void bulk_behav_AppendOutBulkBehav(void)
 * @brief write (appending) gravity and behaviors to OUTBOX/BULK_BEHAV.OUT file
 *
 * @cond PYDOC
 * python usage : bulk_behav_AppendOutBulkBehav()
 * @endcond
 */
 extern "C" void bulk_behav_AppendOutBulkBehav(void);

/**
 * @fn void bulk_behav_RebuildInBulkBehav(void)
 * @brief write (replace) gravity and behaviors to DATBOX/BULK_BEHAV.DAT file
 *
 * @cond PYDOC
 * python usage : bulk_behav_RebuildInBulkBehav()
 * @endcond
 */
 extern "C" void bulk_behav_RebuildInBulkBehav(void);

/**
 * @fn void bulk_behav_GetGravity(double ** r8_vector, int * r8_size)
 * @brief get the gravity acceleration used
 *
 * @cond PYDOC
 * python usage : gravity = bulk_behav_GetGravity()
 * @return    gravity (double array) : gravity vector
 * @endcond
 *
 * @cond CDOC
 * @param[out] r8_vector (double**) : gravity vector
 * @param[out] r8_size (int*)       : size of r8_vector, should be 3
 * @endcond
 */

extern "C" void bulk_behav_GetGravity(double ** r8_vector, int * r8_size);

/**
 * @fn void bulk_behav_SetGravity(double * rvector_in, int rlength_in)
 * @brief set the gravity acceleration to be used
 *
 * @cond PYDOC
 * python usage : bulk_behav_SetGravity(gravity)
 * @param[in] gravity (double array) : gravity vector (size 3)
 * @endcond
 *
 * @cond CDOC
 * @param[in] vector_in (double*) : gravity vector
 * @param[in] length (int)        : size of vector_in, must be 3
 * @endcond
 */

extern "C" void bulk_behav_SetGravity(double * rvector_in, int rlength_in);

/**
 * @fn void bulk_behav_SetConductivity(char * cvalue1_c, int ivalue, double rvalue)
 * @brief set the conductivity parameter to be used
 *
 * @cond PYDOC
 * python usage : bulk_behav_SetConductivity(cvalue ,ivalue, rvalue)
 * @param[in] cvalue (string of size 5)  : nickname of bulk behaviour
 * @param[in] ivalue (integer)           : type of parameter: 0 = constant, 1 = field
 * @param[in] rvalue (real)              : conductivity value
 * @endcond
 *
 * @cond CDOC
 * @param[in] cvalue (char[5])       : nickname of bulk behaviour
 * @param[in] ivalue (int)           : type of parameter: 0 = constant, 1 = field
 * @param[in] rvalue (double)        : conductivity value
 * @endcond
 */

extern "C" void bulk_behav_SetConductivity(char * cvalue1_c, int ivalue, double rvalue);

/**
 * @fn void bulk_behav_SetCapacity(char * cvalue1_c, int ivalue, double rvalue)
 * @brief set the Capacity parameter to be used
 *
 * @cond PYDOC
 * python usage : bulk_behav_SetCapacity(cvalue ,ivalue, rvalue)
 * @param[in] cvalue (string of size 5)  : nickname of bulk behaviour
 * @param[in] ivalue (integer)           : type of parameter: 0 = constant, 1 = field
 * @param[in] rvalue (real)              : Capacity value
 * @endcond
 *
 * @cond CDOC
 * @param[in] cvalue (char[5])       : nickname of bulk behaviour
 * @param[in] ivalue (int)           :  type of parameter: 0 = constant, 1 = field
 * @param[in] rvalue (double)        : Capacity value
 * @endcond
 */

extern "C" void bulk_behav_SetCapacity(char * cvalue1_c, int ivalue, double rvalue);

/**
 * @fn void bulk_behav_SetBiot(char * cvalue1_c, int ivalue, double rvalue)
 * @brief set the Biot parameter to be used
 *
 * @cond PYDOC
 * python usage : bulk_behav_SetBiot(cvalue ,ivalue, rvalue)
 * @param[in] cvalue (string of size 5)  : nickname of bulk behaviour
 * @param[in] ivalue (integer)           : type of parameter: 0 = constant, 1 = field
 * @param[in] rvalue (real)              : Biot value
 * @endcond
 *
 * @cond CDOC
 * @param[in] cvalue (char[5])       : nickname of bulk behaviour
 * @param[in] ivalue (int)           : type of parameter: 0 = constant, 1 = field
 * @param[in] rvalue (double)        : Biot value
 * @endcond
 */

extern "C" void bulk_behav_SetBiot(char * cvalue1_c, int ivalue, double rvalue);

/**
 * @fn void bulk_behav_SetExternalFlux(char * cvalue1_c, int ivalue, double rvalue)
 * @brief set the External Flux  parameter to be used
 *
 * @cond PYDOC
 * python usage : bulk_behav_SetExternalFlux(cvalue ,ivalue, rvalue)
 * @param[in] cvalue (string of size 5) : nickname of bulk behaviour
 * @param[in] ivalue (integer)          : type of parameter: 0 = constant, 1 = field
 * @param[in] rvalue (real)             : External Flux value
 * @endcond
 *
 * @cond CDOC
 * @param[in] cvalue (char[5])       : nickname of bulk behaviour
 * @param[in] ivalue (int)           : type of parameter: 0 = constant, 1 = field
 * @param[in] rvalue (double)        : External Flux value
 * @endcond
 */

extern "C" void bulk_behav_SetExternalFlux(char * cvalue1_c, int ivalue, double rvalue);

/**
 * @fn void bulk_behav_SetDensity(char * cvalue1_c, double rvalue)
 * @brief set the Density parameter to be used
 *
 * @cond PYDOC
 * python usage : bulk_behav_SetDensity(cvalue , rvalue)
 * @param[in] cvalue (string of size 5) : nickname of bulk behaviour
 * @param[in] rvalue (real)             : Density value
 * @endcond
 *
 * @cond CDOC
 * @param[in] cvalue (char[5])       : nickname of bulk behaviour
 * @param[in] rvalue (double)        : Density value
 * @endcond
 */

extern "C" void bulk_behav_SetDensity(char * cvalue1_c, double rvalue);

/**
 * @fn int bulk_behav_GetNbBulkBehav(void);
 * @brief get the number of bulk laws
 *
 * @cond PYDOC
 * python usage : nb_bulk_behav = bulk_behav_GetNbBulkBehav()
 * @param[out] nb_bulk_behav (integer) : number of bulk behaviour in lmgc90
 * @endcond
 *
 * @cond CDOC
 * @return (int) : number of bulk behaviour in lmgc90
 * @endcond
 */
 extern "C" int bulk_behav_GetNbBulkBehav(void);

/**
 * @fn void bulk_behav_GetBulkBehav(int i_bb, char** string_out, int* string_size, int* real_size, char** c5);
 * @brief get a given bulk law
 *
 * @cond PYDOC
 * python usage : lawty, behav = bulk_behav_GetBulkBehav(i_bb)
 * @param[in] i_bb (integer)  : index of the desired bulk_behav
 * @param[out] lawty (string) : type of the bulk law
 * @param[out] behav (string) : name of the bulk law
 * @param[out] param (real vector) : parameters of the law
 * @endcond
 *
 * @cond CDOC
 * @param[in] i_tbb (int) : number of the desired bulk behaviour
 * @param[out] string_out (char **) : type of the bulk law
 * @param[out] string_size (int * ) : size of string_out
 * @param[out] real_size   (int * ) : max size of string pointed by string_out on Fortran side
 * @param[out] c5 (char**) : name of the bulk law
 * @endcond
 */
 extern "C" void bulk_behav_GetBulkBehav(int i_bb, char** string_out, int* string_size, int* real_size, char** c5);

/**
 * @fn void bulk_behav_CleanMemory(void)
 * @brief Free all memory allocated within bulk_behav module
 *
 * @cond PYDOC
 * python usage : bulk_behav_CleanMemory()
 * @endcond
 */
 extern "C" void bulk_behav_CleanMemory(void);

#endif /* bulk_behav_h */
