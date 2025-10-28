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

#ifndef wrap_mp_solver_h
#define wrap_mp_solver_h

/**
 * @fn void mp_solver_ReadMpBehaviour(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : mp_solver_ReadMpBehaviour()
 * @endcond
 */
 extern "C" void mp_solver_ReadMpBehaviour(void);

/**
 * @fn void mp_solver_WriteMpBehaviour(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : mp_solver_WriteMpBehaviour()
 * @endcond
 */
 extern "C" void mp_solver_WriteMpBehaviour(void);

/**
 * @fn void mp_solver_ReadIniMpValues(int num=0)
 * @brief Read MP_VALUES file
 *
 * If num <= 0 : DATBOX/MP_VALUES.INI file is read
 * Else : OUTBOX/MP_VALUES.OUT.num is read, num being
 * the parameter used in TimeEvolution_ReadIniDof
 * last call
 *
 * @cond PYDOC
 * usage : mp_solver_ReadIniMpValues(num=0)
 * @param[in] num (integer) : which file to read
 * @endcond
 *
 * @cond CDOC
 * @param[in] num (int) : which file to read
 * @endcond
 */
 extern "C" void mp_solver_ReadIniMpValues(int num=0);

/**
 * @fn void mp_solver_WriteOutMpValues(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : mp_solver_WriteOutMpValues()
 * @endcond
 */
 extern "C" void mp_solver_WriteOutMpValues(void);

/**
 * @fn void mp_solver_WriteLastMpValues(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : mp_solver_WriteLastMpValues()
 * @endcond
 */
 extern "C" void mp_solver_WriteLastMpValues(void);

/**
 * @fn void mp_solver_SolveElectro1G(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : mp_solver_SolveElectro1G()
 * @endcond
 */
 extern "C" void mp_solver_SolveElectro1G(void);

/**
 * @fn void mp_solver_SolveNlElectro1G(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : mp_solver_SolveNlElectro1G()
 * @endcond
 */
 extern "C" void mp_solver_SolveNlElectro1G(void);

/**
 * @fn void mp_solver_SolveThermoProblem(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : mp_solver_SolveThermoProblem()
 * @endcond
 */
 extern "C" void mp_solver_SolveThermoProblem(void);

/**
 * @fn void mp_solver_UpdateThermoProblem(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : mp_solver_UpdateThermoProblem()
 * @endcond
 */
 extern "C" void mp_solver_UpdateThermoProblem(void);

/**
 * @fn void mp_solver_RecupTemperature(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : mp_solver_RecupTemperature()
 * @endcond
 */
 extern "C" void mp_solver_RecupTemperature(void);

/**
 * @fn void mp_solver_RecupPotential(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : mp_solver_RecupPotential()
 * @endcond
 */
 extern "C" void mp_solver_RecupPotential(void);

/**
 * @fn void mp_solver_UpdateConductivity(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : mp_solver_UpdateConductivity()
 * @endcond
 */
 extern "C" void mp_solver_UpdateConductivity(void);

/**
 * @fn void mp_solver_InitThermalConductivity(void)
 * @brief
 *
 * @cond PYDOC
 * python usage : mp_solver_InitThermalConductivity()
 * @endcond
 */
 extern "C" void mp_solver_InitThermalConductivity(void);

/**
 * @fn void mp_solver_GetBrancheValues(char* c5, int itact, double* rvect, int* ivalue2)
 * @brief
 *
 * @cond PYDOC
 * python usage : value = mp_solver_GetBrancheValues(c5,itact)
 * @endcond
 *
 */
extern "C" void mp_solver_GetBrancheValues(char* c5, int itact, double** r8_vector, int* r8_size);

/**
 * @fn void mp_solver_PutHeatGenerationFactor(double ivalue)
 * @brief
 *
 * @cond PYDOC
 * python usage : value = mp_solver_PutHeatGenerationFactor(ivalue)
 * @endcond
 *
 */
extern "C" void mp_solver_PutHeatGenerationFactor(double ivalue);

/**
 * @fn void mp_solver_PutHeatConductionContinueFactor(double ivalue)
 * @brief
 *
 * @cond PYDOC
 * python usage : value = mp_solver_PutHeatConductionContinueFactor(ivalue)
 * @endcond
 *
 */
extern "C" void mp_solver_PutHeatConductionContinueFactor(double ivalue);

#endif /* wrap_mp_solver_h */
