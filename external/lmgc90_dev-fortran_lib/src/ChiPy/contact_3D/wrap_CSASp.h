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

#ifndef wrap_CSASp_h
#define wrap_CSASp_h

 /**
  * @fn void CSASp_SelectProxTactors(int reset=0,int use_external=0)
  * @brief contact detection between CSxxx and ASpxx tactors
  *
  * First recup coordinate prediction, then proceed to a box selection to found rough
  * contact list and finally compute the final contact list.
  *
  * If reset not equal to 0, the initialization flag is reset and detection skipped
  *
  * @cond PYDOC
  * python usage : CSASp_SelectProxTactors(reset=0,use_external=0)

  * @param[in] reset (integer)        : if not 0, detection is skipped but the boxes will be computed anew at next call
  * @param[in] use_external (integer) : if not 0, external detection is used
  * @endcond
  */
  extern "C" void CSASp_SelectProxTactors(int reset=0,int use_external=0);

 /**
  * @fn void CSASp_WriteLastVlocRloc(void)
  * @brief write last local values of all CSASp contacts
  *
  * The values written are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : CSASp_WriteLastVlocRloc()
  * @endcond
  */
  extern "C" void CSASp_WriteLastVlocRloc(void);

 /**
  * @fn void CSASp_WriteOutVlocRloc(void)
  * @brief write local values of all CSASp contacts
  *
  * The values written are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : CSASp_WriteOutVlocRloc()
  * @endcond
  */
  extern "C" void CSASp_WriteOutVlocRloc(void);

 /**
  * @fn void CSASp_DisplayOutVlocRloc(void)
  * @brief display local values of all CSASp contacts
  *
  * The values displayed are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : CSASp_DisplayOutVlocRloc()
  * @endcond
  */
  extern "C" void CSASp_DisplayOutVlocRloc(void);

 /**
  * @fn void CSASp_DisplayProxTactors(void)
  * @brief display contacts
  *
  * @cond PYDOC
  * python usage : CSASp_DisplayProxTactors()
  * @endcond
  */
  extern "C" void CSASp_DisplayProxTactors(void);

 /**
  * @fn void CSASp_ReadIniVlocRloc(int num=0)
  * @brief Read VlocRloc file
  *
  * - If num <= 0 : DATBOX/VlocRloc.INI file is read
  * - Else : OUTBOX/VlocRloc.OUT.num is read, num being
  *   +the parameter used in TimeEvolution_ReadIniVlocRloc last call
  *
  * @cond PYDOC
  * usage : CSASp_ReadIniVlocRloc(num=0)

  * @param[in] num (integer) : which VlocRloc file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which VlocRloc file to read
  * @endcond
  *
  */
  extern "C" void CSASp_ReadIniVlocRloc(int num=0);

 /**
  * @fn void CSASp_SkipAutoContact(void)
  * @brief avoid CSxxx/ASpxx contact detection when they belong to the same entity
  *
  * @cond PYDOC
  * python usage : CSASp_SkipAutoContact()
  * @endcond
  *
  */
  extern "C" void CSASp_SkipAutoContact(void);

 /**
  * @fn void CSASp_SetNonSymmetricDetection(void);
  * @brief this function allows non symetric detection i.e. only one 
  * interaction is kept when two bodies with candidate and antagonist 
  * contactors see each other
  *
  * @cond PYDOC
  * python usage : CSASp_SetNonSymmetricDetection()
  * @endcond
  */
 extern "C" void CSASp_SetNonSymmetricDetection(void);

 /**
  * @fn void CSASp_Trim(void)
  * @brief trim contact (only contact within surface - not with extremities)
  *
  * @cond PYDOC
  * python usage : CSASp_Trim()
  * @endcond
  */
  extern "C" void CSASp_Trim(void);

 /**
  * @fn void CSASp_SetTrimAngle(double angle)
  * @brief set the trim angle  (only contact within surface - not with extremities)
  *
  * @cond PYDOC
  * python usage : CSASp_SetTrimAngle(angle)
  * @param[in] angle (real) : angle in degree - default 87 deg
  * @endcond
  */
  extern "C" void CSASp_SetTrimAngle(double angle);

 /**
  * @fn void CSASp_AddReac(void)
  * @brief add contact force to body Reac
  *
  * @cond PYDOC
  * python usage : CSASp_AddReac()
  * @endcond
  */
  extern "C" void CSASp_AddReac(void);

 /**
  * @fn void CSASp_AssumeOldFiles(void)
  * @brief to read file with the CSpxx rank instead of CSxxx one
  *
  * @cond PYDOC
  * python usage : CSASp_AssumeOldFiles()
  * @endcond
  */
  extern "C" void CSASp_AssumeOldFiles(void);

/**
  * @fn void CSASp_CleanMemory(void)
  * @brief Free all memory allocated within CSASp module
  *
  * @cond PYDOC
  * python usage : CSASp_CleanMemory()
  * @endcond
  */
  extern "C" void CSASp_CleanMemory(void);

#endif /* wrap_CSASp_h */
