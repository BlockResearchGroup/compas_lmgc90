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
#ifndef wrap_timer_h
#define wrap_timer_h
  
/**
 * @fn void timer_InitializeTimers(void)
 * @brief Set all timers to 0
 *
 * @cond PYDOC
 * python usage : timer_InitializeTimers()
 * @endcond
 */
 extern "C" void timer_InitializeTimers(void);

/**
 * @fn void timer_WriteOutTimers(void)
 * @brief write the cumulated times of all the timers
 *
 * @cond PYDOC
 * python usage : timer_WriteOutTimers()
 * @endcond
 */
 extern "C" void timer_WriteOutTimers(void);

/**
 * @fn int timer_GetNewTimer(char * name)
 * @brief create a new timer
 *
 * @cond PYDOC
 * python usage : id = timer_GetNewTimer(name)
 * @param[in] name (string) : name of new timer
 * @return    id (integer)  : id of the timer created
 * @endcond
 *
 * @cond CDOC
 * @param[in] name (string) : name of new timer
 * @return (int) id of the timer created
 * @endcond
 */
 extern "C" int timer_GetNewTimer(char * name);

/**
 * @fn void timer_StartTimer(int ID)
 * @brief start a given timer
 *
 * @cond PYDOC
 * python usage : timer_StartTimer(timer_id)
 * @param[in] timer_id (integer) : id of the timer to start
 * @endcond
 *
 * @cond CDOC
 * @param[in] ID (int) : id of the timer to start
 * @endcond
 */
 extern "C" void timer_StartTimer(int ID);

/**
 * @fn void timer_StopTimer(int ID)
 * @brief stop a given timer, and add the elapsed time since start to the time
 *
 * @cond PYDOC
 * python usage : timer_StopTimer(timer_id)
 * @param[in] timer_id (integer) : id of the timer to stop
 * @endcond
 *
 * @cond CDOC
 * @param[in] ID (int) : id of the timer to stop
 * @endcond
 */
 extern "C" void timer_StopTimer(int ID);

/**
 * @fn void timer_ClearAll()
 * @brief clear all timers (internal, external and user)
 *
 * @cond PYDOC
 * python usage : timer_ClearAll()
 * @endcond
 */
 extern "C" void timer_ClearAll(void);
    
#endif /* wrap_timer_h */ 
