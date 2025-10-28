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

#ifndef wrap_SPPLx_h
#define wrap_SPPLx_h

 /**
  * @fn void SPPLx_SelectProxTactors(int reset=0)
  * @brief contact detection between SPxxx and PLxxx tactors
  *
  * @cond PYDOC
  * python usage : SPPLx_SelectProxTactors(reset=0)
  * param[in] reset (integer) : if not 0, detection is skipped but
  * the boxes will be computed anew at next call
  * @endcond
  *
  * First recup coordinate prediction, then proceed to a box selection to found rough
  * contact list and finally compute the final contact list.
  */
  extern "C" void SPPLx_SelectProxTactors(int reset=0);

 /**
  * @fn void SPPLx_SmoothForceComputation(void)
  * @brief compute smooth contact law (in any)
  *
  * @cond PYDOC
  * python usage : SPPLx_SmoothForceComputation()
  * @endcond
  */
  extern "C" void SPPLx_SmoothForceComputation(void);

 /**
  * @fn void SPPLx_WriteLastVlocRloc(void)
  * @brief write last local values of all SPPLx contacts
  *
  * @cond PYDOC
  * python usage : SPPLx_WriteLastVlocRloc()
  * @endcond
  *
  * \n the values written are relative velocity, forces and local frame\n 
  */
  extern "C" void SPPLx_WriteLastVlocRloc(void);

 /**
  * @fn void SPPLx_WriteOutVlocRloc(void)
  * @brief write local values of all SPPLx contacts
  *
  * @cond PYDOC
  * python usage : SPPLx_WriteOutVlocRloc()
  * @endcond
  *
  * \n the values written are relative velocity, forces and local frame\n 
  */
  extern "C" void SPPLx_WriteOutVlocRloc(void);

 /**
  * @fn void SPPLx_DisplayOutVlocRloc(void)
  * @brief display local values of all SPPLx contacts
  *
  * @cond PYDOC
  * python usage : SPPLx_DisplayOutVlocRloc()
  * @endcond
  *
  * \n the values displayed are relative velocity, forces and local frame\n 
  */
  extern "C" void SPPLx_DisplayOutVlocRloc(void);

 /**
  * @fn void SPPLx_DisplayProxTactors(void)
  * @brief display contacts
  *
  * @cond PYDOC
  * python usage : SPPLx_DisplayProxTactors()
  * @endcond
  */
  extern "C" void SPPLx_DisplayProxTactors(void);

 /**
  * @fn void SPPLx_ReadIniVlocRloc(int num=0)
  * @brief Read VlocRloc file
  *
  * If num <= 0 : DATBOX/VlocRloc.INI file is read
  * Else : OUTBOX/VlocRloc.OUT.num is read, num being
  * the parameter used in TimeEvolution_ReadIniVlocRloc
  * last call
  *
  * @cond PYDOC
  * usage : SPPLx_ReadIniVlocRloc(num=0)
  * @param[in] num (integer) : which VlocRloc file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which VlocRloc file to read
  * @endcond
  *
  */
  extern "C" void SPPLx_ReadIniVlocRloc(int num=0);

/**
  * @fn void SPPLx_CleanMemory(void)
  * @brief Free all memory allocated within SPPLx module
  *
  * @cond PYDOC
  * python usage : SPPLx_CleanMemory()
  * @endcond
  */
  extern "C" void SPPLx_CleanMemory(void);

#endif /* wrap_SPPLx_h */
