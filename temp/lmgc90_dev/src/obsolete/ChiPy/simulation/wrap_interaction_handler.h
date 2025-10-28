/*==========================================================================
 *
 * Copyright 2000-2022 CNRS-UM.
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

#ifndef _wrap_interaction_handler_h
#define _wrap_interaction_handler_h

 /**
  * @fn void interaction_handler_selectProxTactors(void)
  * @brief interaction_handler detection
  *
  * @cond PYDOC
  * python usage : interaction_handler_selectProxTactors()
  * @endcond
  *
  * \n first recup coordinate prediction, then proceed to a box selection to found rough
  * contact list and finally compute the final contact list\n 
  */
  extern "C" void interaction_handler_selectProxTactors(void);

 /**
  * @fn void interaction_handler_stockRloc(void)
  * @brief stock values of local contact forces for the next time step
  *
  * @cond PYDOC
  * python usage : interaction_handler_stockRloc()
  * @endcond
  */
  extern "C" void interaction_handler_stockRloc(void);

 /**
  * @fn void interaction_handler_recupRloc(void)
  * @brief recup values of local contact forces of the last time step
  *
  * @cond PYDOC
  * python usage : interaction_handler_recupRloc()
  * @endcond
  */
  extern "C" void interaction_handler_recupRloc(void);

 /**
  * @fn void interaction_handler_writeLastVlocRloc(void)
  * @brief write last local values of all interaction_handler contacts
  *
  * @cond PYDOC
  * python usage : interaction_handler_writeLastVlocRloc()
  * @endcond
  *
  * the values written are relative velocity, forces and local frame
  */
  extern "C" void interaction_handler_writeLastVlocRloc(void);

 /**
  * @fn void contactor_initOutlines(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the outlines of all contactor
  *
  * @cond PYDOC
  * usage : outlines = contactor_initOutlines()
  * @return outlines (double array) : a reference on outlines_contactor
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on outlines_contactor array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void contactor_initOutlines(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void contactor_updatePostdata(void)
  * @brief Update values of outlines_contactor and scalarfields_contactor pointers
  *
  * @cond PYDOC
  * usage : contactor_updatePostdata()
  * @endcond
  *
  */
  extern "C" void contactor_updatePostdata(void);

 /**
  * @fn void contactor_getNbPointOutlines(int ** pointer_out, int * length)
  *
  * @brief Get the list of cumulated outline points number
  *
  * @cond PYDOC
  * python usage : nb_pointOutlines = contactor_getNbPointOutlines()
  * @return nb_pointOutlines (integer array) : the cumulated number of outline points of the contactor
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (int **) : reference on nb_point_outlines array
  * @param[in,out] length (int *)       : first dimension of pointer_out
  * @endcond
  */
  extern "C" void contactor_getNbPointOutlines(int** pointer_out, int* length);
  
 /**
  * @fn void interaction_handler_cleanMemory(void)
  * @brief free memory allocated within interaction_handler module
  *
  * @cond PYDOC
  * python usage : interaction_handler_cleanMemory()
  * @endcond
  *
  */
  extern "C" void interaction_handler_cleanMemory(void);

#endif /* _wrap_interaction_handler_h */
