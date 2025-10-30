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

#ifndef wrap_PTPT2_h
#define wrap_PTPT2_h

 /**
  * @fn void PTPT2_SelectProxTactors(int reset)
  * @brief contact detection between PT2Dx tactors
  *
  * @cond PYDOC
  * python usage : PTPT2_SelectProxTactors(reset=0)
  * param[in] reset (integer) : if not 0, detection is skipped but
  * the boxes will be computed anew at next call
  * @endcond
  *
  * First recup coordinate prediction, then proceed to a box selection 
  * to found rough contact list and finally compute the final contact list.
  */
  extern "C" void PTPT2_SelectProxTactors(int reset=0);

 /**
  * @fn void PTPT2_WriteLastVlocRloc(void)
  * @brief write last local values of all PTPT2 contacts
  *
  * @cond PYDOC
  * python usage : PTPT2_WriteLastVlocRloc()
  * @endcond
  *
  * \n the values written are relative velocity, forces and local frame\n 
  */
  extern "C" void PTPT2_WriteLastVlocRloc(void);

 /**
  * @fn void PTPT2_WriteOutVlocRloc(void)
  * @brief write local values of all PTPT2 contacts
  *
  * @cond PYDOC
  * python usage : PTPT2_WriteOutVlocRloc()
  * @endcond
  *
  * \n the values written are relative velocity, forces and local frame\n 
  */
  extern "C" void PTPT2_WriteOutVlocRloc(void);

 /**
  * @fn void PTPT2_DisplayOutVlocRloc(void)
  * @brief display local values of all PTPT2 contacts
  *
  * @cond PYDOC
  * python usage : PTPT2_DisplayOutVlocRloc()
  * @endcond
  *
  * \n the values displayed are relative velocity, forces and local frame\n 
  */
  extern "C" void PTPT2_DisplayOutVlocRloc(void);

 /**
  * @fn void PTPT2_DisplayProxTactors(void)
  * @brief display contacts
  *
  * @cond PYDOC
  * python usage : PTPT2_DisplayProxTactors()
  * @endcond
  */
  extern "C" void PTPT2_DisplayProxTactors(void);

 /**
  * @fn void PTPT2_ReadIniVlocRloc(int num=0)
  * @brief Read VlocRloc file
  *
  * If num <= 0 : DATBOX/VlocRloc.INI file is read
  * Else : OUTBOX/VlocRloc.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniVlocRloc
  * last call
  *
  * @cond PYDOC
  * usage : PTPT2_ReadIniVlocRloc(num=0)
  * @param[in] num (integer) : which VlocRloc file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which VlocRloc file to read
  * @endcond
  *
  */
  extern "C" void PTPT2_ReadIniVlocRloc(int num=0);

/**
  * @fn void PTPT2_LoadNetwork(void)
  * @brief read a PTPT2 network from a file
  *
  * @cond PYDOC
  * python usage : PTPT2_LoadNetwork()
  * @endcond
  */
  extern "C" void PTPT2_LoadNetwork(void);

 /**
  * @fn void PTPT2_SetTolerance(double tol)
  * @brief set the maximum violation for a point to point link
  *
  * @cond PYDOC
  * python usage : PTPT2_SetTolerance(tol)
  * @endcond
  *
  */
  extern "C" void PTPT2_SetTolerance(double tol);

/**
  * @fn void PTPT2_SetExplicitLocalFrame(void)
  * @brief local frame is computed only once at the first step
  *
  * @cond PYDOC
  * python usage : PTPT2_SetExplicitLocalFrame()
  * @endcond
  */
  extern "C" void PTPT2_SetExplicitLocalFrame(void);
  
/**
 * @fn void PTPT2_LoadParams(void)
 * @brief read a PTPT2 surface and l0 from a file
 *
 * @cond PYDOC
 * python usage : PTPT2_LoadParams()
 * @endcond
 */
 extern "C" void PTPT2_LoadParams(void);

 /**
  * @fn void PTPT2_UseCurrentNonuc0(int no0)
  * @brief Use GetCoor or value given from file insted of computing nonuc0 from reference coordinates
  *
  * @cond PYDOC
  * python usage : PTPT2_UseCurrentNonuc0(to_use)
  * param[in] to_use (integer) : 1 to activate, 0 to deactivate feature
  * @endcond
  *
  * @cond CDOC
  * @param[in] to_use (int) : 1 to activate, 0 to deactivate feature
  * @endcond
  */
  extern "C" void PTPT2_UseCurrentNonuc0(int no0);

 /**
  * @fn void PTPT2_CleanMemory(void)
  * @brief Free all memory allocated within PTPT2 module
  *
  * @cond PYDOC
  * python usage : PTPT2_CleanMemory()
  * @endcond
  */
  extern "C" void PTPT2_CleanMemory(void);

#endif /* wrap_PTPT2_h */
