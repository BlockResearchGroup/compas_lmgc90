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

#ifndef wrap_DKDKx_h
#define wrap_DKDKx_h

 /**
  * @fn void DKDKx_SelectProxTactors(int reset)
  * @brief contact detection between CLxxx and JCxxx tactors
  *
  * First recup coordinate prediction, then proceed to a box selection 
  * to found rough contact list and finally compute the final contact list.
  *
  * @cond PYDOC
  * python usage : DKDKx_SelectProxTactors(reset=0)
  *
  * @param[in] reset (integer) : if not 0, detection is skipped but the boxes will be computed anew at next call
  * @endcond
  */
  extern "C" void DKDKx_SelectProxTactors(int reset=0);

 /**
  * @fn void DKDKx_SmoothForceComputation(void)
  * @brief explicit computation of contact forces
  *
  * @cond PYDOC
  * python usage : DKDKx_SmoothForceComputation()
  * @endcond
  */
  extern "C" void DKDKx_SmoothForceComputation(void);

 /**
  * @fn void DKDKx_UseVaVDetection(int nb)
  * @brief allow to increase the number of contact for a pair cd/an.
  *
  * @cond PYDOC
  * python usage : DKDKx_UseVaVDetection(nb)
  *
  * @param[in] nb (integer) : number of contact points for a couple (cd,an)
  * @endcond
  *
  * @cond CDOC
  * @param[in] nb (int) :
  * @endcond
  */
  extern "C" void DKDKx_UseVaVDetection(int nb);

 /**
  * @fn void DKDKx_WriteLastVlocRloc(void)
  * @brief write last local values of all DKDKx contacts
  *
  * The values written are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : DKDKx_WriteLastVlocRloc()
  * @endcond
  */
  extern "C" void DKDKx_WriteLastVlocRloc(void);

 /**
  * @fn void DKDKx_WriteOutVlocRloc(void)
  * @brief write local values of all DKDKx contacts
  *
  * The values written are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : DKDKx_WriteOutVlocRloc()
  * @endcond
  */
  extern "C" void DKDKx_WriteOutVlocRloc(void);

 /**
  * @fn void DKDKx_DisplayOutVlocRloc(void)
  * @brief display local values of all DKDKx contacts
  *
  * The values displayed are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : DKDKx_DisplayOutVlocRloc()
  * @endcond
  */
  extern "C" void DKDKx_DisplayOutVlocRloc(void);

 /**
  * @fn void DKDKx_DisplayProxTactors(void)
  * @brief display contacts
  *
  * @cond PYDOC
  * python usage : DKDKx_DisplayProxTactors()
  * @endcond
  */
  extern "C" void DKDKx_DisplayProxTactors(void);

 /**
  * @fn void DKDKx_ReadIniVlocRloc(int num=0)
  * @brief Read VlocRloc file
  *
  * - If num <= 0 : DATBOX/VlocRloc.INI file is read
  * - Else : OUTBOX/VlocRloc.OUT.num is read, num being
  *   + the parameter used in TimeEvolution_ReadIniVlocRloc last call
  *
  * @cond PYDOC
  * python usage : DKDKx_ReadIniVlocRloc(num=0)
  *
  * @param[in] num (integer) : which VlocRloc file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which VlocRloc file to read
  * @endcond
  *
  */
  extern "C" void DKDKx_ReadIniVlocRloc(int num=0);

 /**
  * @fn void DKDKx_SetPeriodicCondition(double period)
  * @brief initialize data for simulation using periodic condition
  *
  * @cond PYDOC
  * python usage : DKDKx_SetPeriodicCondition(period)
  *
  * @param[in] period (double) : value of the period
  * @endcond
  *
  * @cond CDOC
  * @param[in] period (double) : value of the period
  * @endcond
  */
  extern "C" void DKDKx_SetPeriodicCondition(double period);

/**
  * @fn void DKDKx_SetFrictionModel(char * cflag)
  * @brief initialize data for simulation using evolutive local friction
  *
  * @cond PYDOC
  * python usage : DKDKx_SetFrictionModel(cflag)
  * @param[in] cflag (char) : model to use ('min', 'max' or 'ave')
  * @endcond
  *
  * @cond CDOC
  * @param[in] cflag (char) : model to use ('min', 'max' or 'ave')
  * @endcond
  */
  extern "C" void DKDKx_SetFrictionModel(char * cflag);

 /**
  * @fn void DKDKx_SetSurfaceSectors(int nbsect)
  * @brief Set the number of angular sectors of the surface of contactors 
  *
  * @cond PYDOC
  * python usage : DKDKx_SetSurfaceSectors(nbsect)
  *
  * @param[in] nbsect (integer) : number of sectors
  * @endcond
  *
  * @cond CDOC
  * @param[in] nbsect (int) : number of sectors
  * @endcond
  */
  extern "C" void DKDKx_SetSurfaceSectors(int nbsect);

 /**
  * @fn void DKDKx_UpdateSurfaceEnergySector(void)
  * @brief update surface energy sector
  *
  * @cond PYDOC
  * python usage : DKDKx_UpdateSurfaceEnergySector()
  * 
  * @endcond
  *
  * @cond CDOC
  * 
  * @endcond
  */
  extern "C" void DKDKx_UpdateSurfaceEnergySector(void);

 /**
  * @fn void DKDKx_ComputeStress(void)
  * @brief update surface energy sector
  *
  * @cond PYDOC
  * python usage : DKDKx_ComputeStress()
  * 
  * @endcond
  *
  * @cond CDOC
  * 
  * @endcond
  */
  extern "C" void DKDKx_ComputeStress(void);

 /**
  * @fn void DKDKx_ComputeBetai(void)
  * @brief compute equivalent damage parameter
  *
  * @cond PYDOC
  * python usage : DKDKx_ComputeBetai()
  * 
  * @endcond
  *
  * @cond CDOC
  * 
  * @endcond
  */
  extern "C" void DKDKx_ComputeBetai(void);

 /**
  * @fn void DKDKx_ComputeCZMEnergy(void)
  * @brief compute and decompose local contact energy with CZM law
  *
  * @cond PYDOC
  * python usage : DKDKx_ComputeCZMEnergy()
  * 
  * @endcond
  *
  * @cond CDOC
  * 
  * @endcond
  */
  extern "C" void DKDKx_ComputeCZMEnergy(void);

 /**
  * @fn void DKDKx_CleanMemory(void)
  * @brief Free all memory allocated within DKDKx module
  *
  * @cond PYDOC
  * python usage : DKDKx_CleanMemory()
  * @endcond
  */
  extern "C" void DKDKx_CleanMemory(void);

 /**
  * @fn void DKDKx_GetCZMEnergy(int icdan, double * energy)
  * @brief Get the CZM energy of a given contact
  *
  * @cond PYDOC
  * python usage : energy = DKDKx_GetCZMEnergy(icdan)
  *
  * @param[in] icdan(int) : index of the DKDKx contact
  *
  * @return energy(double[4]) : energy value
  * @endcond
  */
  extern "C" void DKDKx_GetCZMEnergy(int icdan, double * energy);


#endif /* wrap_DKDKx_h */
