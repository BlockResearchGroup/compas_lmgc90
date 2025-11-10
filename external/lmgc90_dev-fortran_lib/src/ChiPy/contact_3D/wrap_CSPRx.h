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

#ifndef wrap_CSPRx_h
#define wrap_CSPRx_h

 /**
  * @fn void CSPRx_SelectProxTactors(int reset=0)
  * @brief contact detection between CSxxx and PRxxx tactors
  *
  * First recup coordinate prediction, then proceed to a box selection to found rough
  * contact list and finally compute the final contact list.
  *
  * @cond PYDOC
  * python usage : CSPRx_SelectProxTactors(int reset=0)

  * @param[in] reset (integer) : if not 0, detection is skipped but the boxes will be computed anew at next call
  * @endcond
  */
  extern "C" void CSPRx_SelectProxTactors(int reset=0);

 /**
  * @fn void CSPRx_WriteLastVlocRloc(void)
  * @brief write last local values of all CSPRx contacts
  *
  * The values written are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : CSPRx_WriteLastVlocRloc()
  * @endcond
  *
  * The values written are relative velocity, forces and local frame
  */
  extern "C" void CSPRx_WriteLastVlocRloc(void);

 /**
  * @fn void CSPRx_WriteOutVlocRloc(void)
  * @brief write local values of all CSPRx contacts
  *
  * The values written are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : CSPRx_WriteOutVlocRloc()
  * @endcond
  */
  extern "C" void CSPRx_WriteOutVlocRloc(void);

 /**
  * @fn void CSPRx_DisplayOutVlocRloc(void)
  * @brief display local values of all CSPRx contacts
  *
  * The values displayed are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : CSPRx_DisplayOutVlocRloc()
  * @endcond
  */
  extern "C" void CSPRx_DisplayOutVlocRloc(void);

 /**
  * @fn void CSPRx_DisplayProxTactors(void)
  * @brief display contacts
  *
  * @cond PYDOC
  * python usage : CSPRx_DisplayProxTactors()
  * @endcond
  */
  extern "C" void CSPRx_DisplayProxTactors(void);

 /**
  * @fn void CSPRx_ReadIniVlocRloc(int num=0)
  * @brief Read VlocRloc file
  *
  * - If num <= 0 : DATBOX/VlocRloc.INI file is read
  * - Else : OUTBOX/VlocRloc.OUT.num is read, num being
  *   +the parameter used in TimeEvolution_ReadIniVlocRloc last call
  *
  * @cond PYDOC
  * usage : CSPRx_ReadIniVlocRloc(num=0)

  * @param[in] num (integer) : which VlocRloc file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which VlocRloc file to read
  * @endcond
  *
  */
  extern "C" void CSPRx_ReadIniVlocRloc(int num=0);

 /**
  * @fn void CSPRx_Trim(void)
  * @brief trim contact (only node face contact)
  *
  * @cond PYDOC
  * python usage : CSPRx_Trim()
  * @endcond
  */
  extern "C" void CSPRx_Trim(void);

 /**
  * @fn void CSPRx_GetInfo(int icdan, int ** i4_vector, int * i4_size )
  * @brief return contact info for the icdan CSPRx contact
  *
  * @cond PYDOC
  * python usage : a = CSPRx_GetInfo(icdan)

  * @param[in] icdan (integer)  : contact identifiant
  * @return    a (array integer) : info array
  * @endcond
  *
  * @cond CDOC
  * @param[in] icdan (int)  : contact identifiant
  * @return    a     (int*) : info array
  * @endcond
  */
  extern "C" void CSPRx_GetInfo(int icdan, int ** i4_vector, int * i4_size);

 /**
  * @fn void CSPRx_Smoothing(void)
  * @brief smooth contact reaction
  *
  * @cond PYDOC
  * python usage : CSPRx_Smmothing()
  * @endcond
  */
  extern "C" void CSPRx_Smoothing(void);

 /**
  * @fn void CSPRx_AddReac(void)
  * @brief add contact force to body Reac
  *
  * @cond PYDOC
  * python usage : CSPRx_AddReac()
  * @endcond
  */
  extern "C" void CSPRx_AddReac(void);

/**
  * @fn void CSPRx_CleanMemory(void)
  * @brief Free all memory allocated within CSPRx module
  *
  * @cond PYDOC
  * python usage : CSPRx_CleanMemory()
  * @endcond
  */
  extern "C" void CSPRx_CleanMemory(void);

#endif /* wrap_CSPRx_h */
