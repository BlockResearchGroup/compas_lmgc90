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

#ifndef wrap_PLPLx_h
#define wrap_PLPLx_h

 /**
  * @fn void PLPLx_SelectProxTactors(int reset)
  * @brief contact detection between POLYG tactors
  *
  * First recup coordinate prediction, then proceed to a box selection 
  * to found rough contact list and finally compute the final contact list.
  *
  * @cond PYDOC
  * python usage : PLPLx_SelectProxTactors(reset=0)
  *
  * @param[in] reset (integer) : if not 0, detection is skipped but the boxes will be computed anew at next call
  * @endcond
  */
  extern "C" void PLPLx_SelectProxTactors(int reset=0);

 /**
  * @fn void PLPLx_WriteLastVlocRloc(void)
  * @brief write last local values of all PLPLx contacts
  *
  * The values written are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : PLPLx_WriteLastVlocRloc()
  * @endcond
  */
  extern "C" void PLPLx_WriteLastVlocRloc(void);
   
 /**
  * @fn void PLPLx_WriteOutVlocRloc(void)
  * @brief write local values of all PLPLx contacts
  *
  * The values written are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : PLPLx_WriteOutVlocRloc()
  * @endcond
  */
  extern "C" void PLPLx_WriteOutVlocRloc(void);

 /**
  * @fn void PLPLx_DisplayOutVlocRloc(void)
  * @brief display local values of all PLPLx contacts
  *
  * The values displayed are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : PLPLx_DisplayOutVlocRloc()
  * @endcond
  */
  extern "C" void PLPLx_DisplayOutVlocRloc(void);

 /**
  * @fn void PLPLx_DisplayProxTactors(void)
  * @brief display contacts
  *
  * @cond PYDOC
  * python usage : PLPLx_DisplayProxTactors()
  * @endcond
  */
  extern "C" void PLPLx_DisplayProxTactors(void);

 /**
  * @fn void PLPLx_ReadIniVlocRloc(int num=0)
  * @brief Read VlocRloc file
  *
  * - If num <= 0 : DATBOX/VlocRloc.INI file is read
  * - Else : OUTBOX/VlocRloc.OUT.num is read, num being
  *   + the parameter used in TimeEvolution_ReadIniVlocRloc last call
  *
  * @cond PYDOC
  * python usage : PLPLx_ReadIniVlocRloc(num=0)
  *
  * @param[in] num (integer) : which VlocRloc file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which VlocRloc file to read
  * @endcond
  *
  */
  extern "C" void PLPLx_ReadIniVlocRloc(int num=0);

 /**
  * @fn void PLPLx_SetPeriodicCondition(double period)
  * @brief initialize data for simulation using periodic condition
  *
  * @cond PYDOC
  * python usage : PLPLx_SetPeriodicCondition(period)
  * @param period (double) : value of the period
  * @endcond
  *
  * @cond CDOC
  * @param period (double) : value of the period
  * @endcond
  */
  extern "C" void PLPLx_SetPeriodicCondition(double period);

/**
  * @fn void PLPLx_SetFrictionModel(char * cflag)
  * @brief initialize data for simulation using evolutive local friction
  *
  * @cond PYDOC
  * python usage : PLPLx_SetFrictionModel(cflag)
  * @param[in] cflag (char) : model to use ('min', 'max' or 'ave')
  * @endcond
  *
  * @cond CDOC
  * @param[in] cflag (char) : model to use ('min', 'max' or 'ave')
  * @endcond
  */
  extern "C" void PLPLx_SetFrictionModel(char * cflag);

 /**
  * @fn void PLPLx_SetBigPolygTolerance(double tol)
  * @brief 
  *
  * @cond PYDOC
  * python usage : PLPLx_SetBigPolygTolerance(tol)
  * @param period (double) : value of the tolerance
  * @endcond
  *
  * @cond CDOC
  * @param tol (double) : tolerance value
  * @endcond
  */
  extern "C" void PLPLx_SetBigPolygTolerance(double tol);

 /**
  * @fn void PLPLx_ComputeStress(void)
  * @brief compute stress
  *
  * @cond PYDOC
  * python usage : PLPLx_ComputeStress()
  * 
  * @endcond
  *
  * @cond CDOC
  * 
  * @endcond
  */
  extern "C" void PLPLx_ComputeStress(void);

 /**
  * @fn void PLPLx_ComputeBetai(void)
  * @brief compute equivalent damage parameter
  *
  * @cond PYDOC
  * python usage : PLPLx_ComputeBetai()
  * 
  * @endcond
  *
  * @cond CDOC
  * 
  * @endcond
  */
  extern "C" void PLPLx_ComputeBetai(void);

 /**
  * @fn void PLPLx_ComputeCZMEnergy(void)
  * @brief compute and decompose local contact energy with CZM law
  *
  * @cond PYDOC
  * python usage : PLPLx_ComputeCZMEnergy()
  * 
  * @endcond
  *
  * @cond CDOC
  * 
  * @endcond
  */
  extern "C" void PLPLx_ComputeCZMEnergy(void);

 /**
  * @fn void PLPLx_CleanMemory(void)
  * @brief Free all memory allocated within PLPLx module
  *
  * @cond PYDOC
  * python usage : PLPLx_CleanMemory()
  * @endcond
  */
  extern "C" void PLPLx_CleanMemory(void);

/**
  * @fn void PLPLx_GetCZMEnergy(int icdan, double * energy)
  * @brief Get the CZM energy of a given contact
  *
  * @cond PYDOC
  * python usage energy = PLPLx_GetCZMEnergy(icdan)
  * @param[in] icdan(int) : index of the PLPLx contact
  * @return energy(double[4]) : energy value
  */
  extern "C" void PLPLx_GetCZMEnergy(int icdan, double * energy);

 /**
  * @fn void PLPLx_UseNcDetection(void)
  * @brief chooses contact detection methode between non-convex shapes
  *
  * @cond PYDOC
  * python usage : PLPLx_UseNcDetection()
  * @endcond
  */
  extern "C" void PLPLx_UseNcDetection(void);

 /**
  * @fn void PLPLx_ShrinkPolygFaces(double shrink)
  * @brief Shrink the face of the polygon for the detection
  *
  * @cond PYDOC
  * python usage : PLPLx_ShrinkPolygFaces(shrink)
  * @param[in] shrink (real) :
  * @endcond
  *
  * @cond CDOC
  * @param[in] shrink (double) :
  * @endcond
  */
  extern "C" void PLPLx_ShrinkPolygFaces(double shrink);

#endif /* wrap_PLPLx_h */
