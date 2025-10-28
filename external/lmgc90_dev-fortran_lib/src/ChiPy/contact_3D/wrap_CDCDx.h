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

#ifndef wrap_CDCDx_h
#define wrap_CDCDx_h

 /**
  * @fn void CDCDx_SelectProxTactors(int reset=0)
  * @brief contact detection between CYLND and CYLND tactors
  *
  * First recup coordinate prediction, then proceed to a box selection to found rough
  * contact list and finally compute the final contact list 
  *
  * @cond PYDOC
  * python usage : CDCDx_SelectProxTactors(reset=0)
  *
  * @param[in] reset (integer) : if not 0, detection is skipped but the boxes will be computed anew at next call
  * @endcond
  *
  */
  extern "C" void CDCDx_SelectProxTactors(int reset=0);

 /**
  * @fn void CDCDx_SmoothForceComputation(void)
  * @brief computes smooth contact forces (if any)
  *
  * @cond PYDOC
  * python usage : CDCDx_SmoothForceComputation()
  * @endcond
  */
  extern "C" void CDCDx_SmoothForceComputation(void);

 /**
  * @fn void CDCDx_WriteLastVlocRloc(void)
  * @brief write last local values of all CDCDx contacts
  *
  * The values written are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : CDCDx_WriteLastVlocRloc()
  * @endcond
  */
  extern "C" void CDCDx_WriteLastVlocRloc(void);

 /**
  * @fn void CDCDx_WriteOutVlocRloc(void)
  * @brief write local values of all CDCDx contacts
  *
  * The values written are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : CDCDx_WriteOutVlocRloc()
  * @endcond
  */
  extern "C" void CDCDx_WriteOutVlocRloc(void);

 /**
  * @fn void CDCDx_DisplayOutVlocRloc(void)
  * @brief display local values of all CDCDx contacts
  *
  * The values displayed are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : CDCDx_DisplayOutVlocRloc()
  * @endcond
  */
  extern "C" void CDCDx_DisplayOutVlocRloc(void);

 /**
  * @fn void CDCDx_DisplayProxTactors(void)
  * @brief display detected contacts
  *
  * @cond PYDOC
  * python usage : CDCDx_DisplayProxTactors()
  * @endcond
  */
  extern "C" void CDCDx_DisplayProxTactors(void);

 /**
  * @fn void CDCDx_ReadIniVlocRloc(int num=0)
  * @brief Read VlocRloc file
  *
  * - If num <= 0 : DATBOX/VlocRloc.INI file is read
  * - Else : OUTBOX/VlocRloc.OUT.num is read,
  *   + num being the parameter used in TimeEvolution_ReadIniVlocRloc last call
  *
  * @cond PYDOC
  * python usage : CDCDx_ReadIniVlocRloc(num=0)
  *
  * @param[in] num (integer) : which VlocRloc file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which VlocRloc file to read
  * @endcond
  */
  extern "C" void CDCDx_ReadIniVlocRloc(int num=0);

 /**
  * @fn void CDCDx_SetXPeriodicCondition(double xperiod)
  * @brief initialise data for simulation using periodic condition along X
  *
  * @cond PYDOC
  * python usage : CDCDx_SetXPeriodicCondition(xperiod)
  *
  * @param[in] xperiod (double) : period on x axis
  * @endcond
  *
  * @cond CDOC
  * @param[in] xperiod (double) : period on x axis
  * @endcond
  */
  extern "C" void CDCDx_SetXPeriodicCondition(double xperiod);

 /**
  * @fn void CDCDx_SetYPeriodicCondition(double yperiod)
  * @brief initialise data for simulation using periodic condition along Y
  *
  * @cond PYDOC
  * python usage : CDCDx_SetYPeriodicCondition(yperiod)
  *
  * @param[in] yperiod (double) : period on y axis
  * @endcond
  *
  * @cond CDOC
  * @param[in] yperiode (double) : period on y axis
  * @endcond
  */
  extern "C" void CDCDx_SetYPeriodicCondition(double yperiod);

 /**
  * @fn void CDCDx_SetNumberInterByContact(int nb_interactions)
  * @brief define the number of interaction by contact (experimental)
  *
  * @cond PYDOC
  * python usage : CDCDx_SetNumberInterByContact(nb_interactions)
  *
  * @param[in] nb_interactions (integer) : number of interactions per contact
  * @endcond
  *
  * @cond CDOC
  * @param[in] nb_interactions (int) : number of interactions per contact
  * @endcond
  */
  extern "C" void CDCDx_SetNumberInterByContact(int nb_interactions);

 /**
  * @fn void CDCDx_SetContactRadius(double radius)
  * @brief define the contact radius (experimental)
  *
  * @cond PYDOC
  * python usage : CDCDx_SetContactRadius(radius)
  *
  * @param[in] radius (double) : contact radius
  * @endcond
  *
  * @cond CDOC
  * @param[in] radius (double) : contact radius
  * @endcond
  */
  extern "C" void CDCDx_SetContactRadius(double radius);

/**
  * @fn void CDCDx_CleanMemory(void)
  * @brief Free all memory allocated within CDCDx module
  *
  * @cond PYDOC
  * python usage : CDCDx_CleanMemory()
  * @endcond
  */
  extern "C" void CDCDx_CleanMemory(void);

#endif /* wrap_CDCDx_h */
