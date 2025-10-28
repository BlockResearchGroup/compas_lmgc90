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

#ifndef wrap_postpro_h
#define wrap_postpro_h

 /**
  * @fn void postpro_PostproDuringComputation(void)
  * @brief Scan postprocessing function which should be call during the computation process
  *
  * @cond PYDOC
  * python usage : postpro_PostproDuringComputation()
  * @endcond
  */
 extern "C" void postpro_PostproDuringComputation(void);

 /**
  * @fn void postpro_ReadCommands(void)
  * @brief Scan postprocessing function which should be call during the computation process
  *
  * @cond PYDOC
  * python usage : postpro_ReadCommands()
  * @endcond
  */
 extern "C" void postpro_ReadCommands(void);
    
 /**
  * @fn void postpro_PostproBeforeComputation(int restart=0)
  * @brief Data initialization and scan postprocessing function
  * which should be called before the computation process
  *
  * @cond PYDOC
  * python usage : postpro_PostproBeforeComputation(restart=0)
  * param[in] restart (integer) : if the Postpro file must append to existing ones
  *                               and starting index of CONTACT_FORCE_DISTRIBUTION files
  * @endcond
  * @cond CDOC
  * param[in] restart (int) : if the Postpro file must append to existing ones
  *                           and starting index of CONTACT_FORCE_DISTRIBUTION files
  * @endcond
  */
 extern "C" void postpro_PostproBeforeComputation(int restart=0);

 /**
  * @fn void postpro_FlushDuringComputation(void)
  * @brief Flush all postpro files
  *
  * @cond PYDOC
  * python usage : postpro_FlushDuringComputation()
  * @endcond
  */
 extern "C" void postpro_FlushDuringComputation(void);

 /**
  * @fn void postpro_ClosePostproFiles(void)
  * @brief Close all postpro files
  *
  * @cond PYDOC
  * python usage : postpro_ClosePostproFiles()
  * @endcond
  */
 extern "C" void postpro_ClosePostproFiles(void);

 /**
  * @fn void postpro_SetCircularSelectionZone(double rvalue1, double rvalue2, double rvalue3)
  * @brief Initialize data for postreatment using a circular selection
  *
  * @cond PYDOC
  * python usage : postpro_SetCircularSelectionZone(rvalue1, rvalu2, rvalue3)
  * @param[in] rvalue1 (double) : X coordinate
  * @param[in] rvalue2 (double) : Y coordinate
  * @param[in] rvalue3 (double) : radius selection
  * @endcond
  *
  * @cond CDOC
  * @param[in] rvalue1 (double) : X coordinate
  * @param[in] rvalue2 (double) : Y coordinate
  * @param[in] rvalue3 (double) : radius selection
  * @endcond
  */
 extern "C" void postpro_SetCircularSelectionZone(double rvalue1, double rvalue2, double rvalue3);

/**
  * @fn void postpro_MoveCircularSelectionZone(double rvalue1, double rvalue2)
  * @brief Increment the position of the circular selection defined with CIRCULAR_SELECTION
  *
  * @cond PYDOC
  * python usage : postpro_MoveCircularSelectionZone(rvalue1, rvalu2)
  * @param[in] rvalue1 (double) : X translational velocity
  * @param[in] rvalue2 (double) : Y translational velocity
  * @endcond
  *
  * @cond CDOC
  * @param[in] rvalue1 (double) : X translational velocity
  * @param[in] rvalue2 (double) : Y translational velocity
  * @endcond
  */
 extern "C" void postpro_MoveCircularSelectionZone(double rvalue1, double rvalue2);
    
 /**
  * @fn void postpro_CleanMemory(void)
  * @brief Free all memory allocated within postpro module
  *
  * @cond PYDOC
  * python usage : postpro_CleanMemory()
  * @endcond
  */
  extern "C" void postpro_CleanMemory(void);


/**
  * @fn double postpro_2D_GetKineticEnergy(void)
  * @brief Compute Kinetic Energy for all bodies (rigids and defo)
  *
  * @cond PYDOC
  * python usage : KE = postpro_2D_GetKineticEnergy()
  * @endcond
  */
 extern "C" double postpro_2D_GetKineticEnergy(void);

#endif /* wrap_postpro_h */
