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

#ifndef wrap_global_thermal_solver_h
#define wrap_global_thermal_solver_h
  
/**
 * @fn void gts_Initialize()
 * @brief Initialize global solver module
 *
 * @cond PYDOC
 * python usage : gts_Initialize()
 * @endcond
 */
 extern "C" void gts_Initialize(void);

/**
 * @fn void gts_AssembleSystem()
 * @brief Assembling of the global system
 *
 * @cond PYDOC
 * python usage : gts_AssembleSystem()
 * @endcond
 */
 extern "C" void gts_AssembleSystem(void);


/**
 * @fn void gts_PrepSystem()
 * @brief Preparing the global system
 *
 * @cond PYDOC
 * python usage  gts_PrepSystem()
 * @endcond
 */
 extern "C" void gts_PrepSystem(void);

/**
 * @fn void gts_AssembleLHS()
 * @brief Assembling the lhs of the global system
 *
 * @cond PYDOC
 * python usage : gts_AssemblerLHS()
 * @endcond
 */
 extern "C" void gts_AssembleLHS(void);


/**
 * i@fn void gts_AssembleRHS()
 * @brief Assembling the rhs of the global system
 *
 * @cond PYDOC
 * python usage : gts_AssembleRHS()
 * @endcond
 */
 extern "C" void gts_AssembleRHS(void);

/**
 * i@fn void gts_Solve()
 * @brief Solving  of the global system
 *
 * @cond PYDOC
 * python usage : gts_Solve()
 * @endcond
 */
 extern "C" void gts_Solve(void);

/**
 * @fn void gts_Finalize()
 * @brief Clean memory of global solver module
 *
 * @cond PYDOC
 * python usage : gts_Finalize()
 * @endcond
 */
 extern "C" void gts_Finalize(void);

#endif
