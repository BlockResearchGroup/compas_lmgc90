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

#ifndef wrap_interaction_handler_h
#define wrap_interaction_handler_h

 /**
  * @fn int interaction_handler_getNb(void)
  * @brief return the number of contact found
  *
  * @cond PYDOC
  * python usage : nb_int = interaction_handler_getNb()
  * @return nb_int (integer) : number of interaction found
  * @endcond
  *
  * @cond CDOC
  * @return nb_int(int) : number of interaction found
  * @endcond
  */
  extern "C" int interaction_handler_getNb(void);

 /**
  * @fn int interaction_handler_getNbRough(void)
  * @brief return the number of rough contact found
  *
  * @cond PYDOC
  * python usage : nb_int = interaction_handler_getNbRough()
  * @return nb_int (integer) : number of rough interaction found
  * @endcond
  *
  * @cond CDOC
  * @return nb_int(int) : number of rough interaction found
  * @endcond
  */
  extern "C" int interaction_handler_getNbRough(void);

 /**
  * @fn int interaction_handler_getNbRecup(void)
  * @brief return the number of recup contact found
  *
  * @cond PYDOC
  * python usage : nb_int = interaction_handler_getNbRecup()
  * @return nb_int (integer) : number of recup interaction found
  * @endcond
  *
  * @cond CDOC
  * @return nb_int(int) : number of recup interaction found
  * @endcond
  */
  extern "C" int interaction_handler_getNbRecup(void);

 /**
  * @fn void interaction_handler_activateDetection(char * cvalue)
  * @brief Activate a detection method
  *
  * @cond PYDOC
  * usage : interaction_handler_activateDetection(inter_name)
  * @param inter_name (string of size 5) : the interaction type to detect
  * @endcond
  *
  * @cond CDOC
  * @param[in]  cvalue (char[5]) : the interaction type to detect
  * @endcond
  */
  extern "C" void interaction_handler_activateDetection(char * cvalue);

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
  * @fn void interaction_handler_writeOutVlocRloc(void)
  * @brief write local values of all interaction_handler contacts
  *
  * @cond PYDOC
  * python usage : interaction_handler_writeOutVlocRloc()
  * @endcond
  *
  * \n the values written are relative velocity, forces and local frame\n 
  */
  extern "C" void interaction_handler_writeOutVlocRloc(void);

 /**
  * @fn void interaction_handler_displayOutVlocRloc(void)
  * @brief display local values of all interaction_handler contacts
  *
  * @cond PYDOC
  * python usage : interaction_handler_displayOutVlocRloc()
  * @endcond
  *
  * \n the values displayed are relative velocity, forces and local frame\n 
  */
  extern "C" void interaction_handler_displayOutVlocRloc(void);

 /**
  * @fn void interaction_handler_readIniVlocRloc(void)
  * @brief read file Vloc_Rloc.INI in DATBOX
  *
  * @cond PYDOC
  * python usage : interaction_handler_readIniVlocRloc()
  * @endcond
  */
  extern "C" void interaction_handler_readIniVlocRloc(void);

 /**
  * @fn void interaction_handler_getPtrAll(double ** pointer_out, int * dim1, int * dim2)
  * @brief return contact point coordinates, normal and forces
  *
  * @cond PYDOC
  * python usage : array = interaction_handler_getPtrAll()
  * @return array (double 2D-array) : mechanical data
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : xxx
  * @param[in]     dim1 (int *)            : pointer_out first dimension
  * @param[in]     dim2 (int *)            : pointer_out second dimension
  * @endcond
  */
  extern "C" void interaction_handler_getPtrAll(double ** pointer_out, int * dim1, int * dim2);

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


#endif /* wrap_interaction_handler_h */
