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

#ifndef wrap_SPSPx_h
#define wrap_SPSPx_h

 /**
  * @fn void SPSPx_SelectProxTactors(int reset=0)
  * @brief contact detection between SPxxx and SPxxx tactors
  *
  * @cond PYDOC
  * python usage : SPSPx_SelectProxTactors(reset=0)
  * param[in] reset (integer) : if not 0, detection is skipped but
  * the boxes will be computed anew at next call
  * @endcond
  *
  * First recup coordinate prediction, then proceed to a box selection to found rough
  * contact list and finally compute the final contact list.
  */
  extern "C" void SPSPx_SelectProxTactors(int reset=0);

 /**
  * @fn void SPSPx_SmoothForceComputation(void)
  * @brief recup values of local contact forces of the last time step
  *
  * @cond PYDOC
  * python usage : SPSPx_SmoothForceComputation()
  * @endcond
  */
  extern "C" void SPSPx_SmoothForceComputation(void);

 /**
  * @fn void SPSPx_WriteLastVlocRloc(void)
  * @brief write last local values of all SPSPx contacts
  *
  * @cond PYDOC
  * python usage : SPSPx_WriteLastVlocRloc()
  * @endcond
  *
  * \n the values written are relative velocity, forces and local frame\n 
  */
  extern "C" void SPSPx_WriteLastVlocRloc(void);

 /**
  * @fn void SPSPx_WriteOutVlocRloc(void)
  * @brief write local values of all SPSPx contacts
  *
  * @cond PYDOC
  * python usage : SPSPx_WriteOutVlocRloc()
  * @endcond
  *
  * \n the values written are relative velocity, forces and local frame\n 
  */
  extern "C" void SPSPx_WriteOutVlocRloc(void);

 /**
  * @fn void SPSPx_DisplayOutVlocRloc(void)
  * @brief display local values of all SPSPx contacts
  *
  * @cond PYDOC
  * python usage : SPSPx_DisplayOutVlocRloc()
  * @endcond
  *
  * \n the values displayed are relative velocity, forces and local frame\n 
  */
  extern "C" void SPSPx_DisplayOutVlocRloc(void);

 /**
  * @fn void SPSPx_DisplayProxTactors(void)
  * @brief display contacts
  *
  * @cond PYDOC
  * python usage : SPSPx_DisplayProxTactors()
  * @endcond
  */
  extern "C" void SPSPx_DisplayProxTactors(void);

 /**
  * @fn void SPSPx_ReadIniVlocRloc(int num=0)
  * @brief Read VlocRloc file
  *
  * If num <= 0 : DATBOX/VlocRloc.INI file is read
  * Else : OUTBOX/VlocRloc.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniVlocRloc
  * last call
  *
  * @cond PYDOC
  * usage : SPSPx_ReadIniVlocRloc(num=0)
  * @param[in] num (integer) : which VlocRloc file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which VlocRloc file to read
  * @endcond
  *
  */
  extern "C" void SPSPx_ReadIniVlocRloc(int num=0);

 /**
  * @fn void SPSPx_SetXPeriodicCondition(double xperiod)
  * @brief initialise data for simulation using periodic condition
  *
  * @cond PYDOC
  * python usage : SPSPx_SetXPeriodicCondition(xperiod)
  * @param[in] xperiod (real) : period on x axis
  * @endcond
  *
  * @cond CDOC
  * @param[in] xperiod (double) : period on x axis
  * @endcond
  */
  extern "C" void SPSPx_SetXPeriodicCondition(double xperiod);

 /**
  * @fn void SPSPx_SetYPeriodicCondition(double yperiod)
  * @brief initialise data for simulation using periodic condition
  *
  * @cond PYDOC
  * python usage : SPSPx_SetYPeriodicCondition(yperiod)
  * @param[in] yperiod (real) : period on y axis
  * @endcond
  *
  * @cond CDOC
  * @param[in] yperiod (double) : period on y axis
  * @endcond
  */
  extern "C" void SPSPx_SetYPeriodicCondition(double yperiod);

 /**
  * @fn void SPSPx_SetNumberInterByContact(int nb_interactions)
  * @brief define the number of interaction by contact (experimental)
  *
  * @cond PYDOC
  * python usage : SPSPx_SetNumberInterByContact(nb_interactions)
  * @param[in] nb_interactions (integer) : number of interactions per contact
  * @endcond
  *
  * @cond CDOC
  * @param[in] nb_interactions (int) : number of interactions per contact
  * @endcond
  */
  extern "C" void SPSPx_SetNumberInterByContact(int nb_interactions);

 /**
  * @fn void SPSPx_SetContactRadius(double radius)
  * @brief define the contact radius (experimental)
  *
  * @cond PYDOC
  * python usage : SPSPx_SetContactRadius(radius)
  * @param[in] radius (real) : contact radius
  * @endcond
  *
  * @cond CDOC
  * @param[in] radius (double) : contact radius
  * @endcond
  */
  extern "C" void SPSPx_SetContactRadius(double radius);

 /**
  * @fn void SPSPx_FdSelectProxTactors(void)
  * @brief contact detection between SPHER and SPHER tactors
  *
  * @cond PYDOC
  * python usage : SPSPx_FdSelectProxTactors()
  * @endcond
  *
  * First recup coordinate prediction, then proceed to a box selection to found rough
  * contact list and finally compute the final contact list.
  */
  extern "C" void SPSPx_FdSelectProxTactors(void);

/**
  * @fn void SPSPx_CleanMemory(void)
  * @brief Free all memory allocated within SPSPx module
  *
  * @cond PYDOC
  * python usage : SPSPx_CleanMemory()
  * @endcond
  */
  extern "C" void SPSPx_CleanMemory(void);

#endif /* wrap_SPSPx_h */
