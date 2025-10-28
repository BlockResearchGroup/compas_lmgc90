/*==========================================================================
 *
 * Copyright 2000-2004 CNRS.
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
 * Frederic Dubois or Michel Jean.
 *
 * dubois@lmgc.univ-montp2.fr
 * mjean@mn.esm2.imt-mrs.fr
 *
 *=========================================================================*/

// /**
//  * @fn void mecaMAILx_AddSpringToNode(int IdBody, int IdNode, int IdDof, double k)
//  * @brief  add a spring to a dof of a node
//  *
//  * @cond PYDOC
//  * python usage : mecaMAILx_AddSpringToNode(idbody,idnode,iddof,rv)
//  * @param[in] idbody (integer) : id of the body
//  * @param[in] idnode (integer) : id of node
//  * @param[in] iddof  (integer) : id of dof
//  * @param[in] k        (float) : stiffness value
//  * @endcond
//  *
//  * @cond CDOC
//  * @param[in] idbody (int) : id of the body
//  * @param[in] idnode (int) : id of node
//  * @param[in] iddof  (int) : id of dof
//  * @param[in] k   (double) : stiffness value
//  * @endcond
//  */
//  extern "C" void  mecaMAILx_AddSpringToNode(int IdBody, int IdNode, int IdDof, double k);

 /**
  * @fn void mecaMAILx_InitTherField(int IdBody, double Tini)
  * @brief Initialize the thermal field to a constant value on a given body. The field is uniform.
  *
  * @cond PYDOC
  * python usage : mecaMAILx_InitTherField(IdBody, Tini)
  * @param[in] IdBody (int)  : id of the concern body
  * @param[in] Tini (double) : initial temperature on the body
  * @endcond
  *
  * @cond CDOC
  * @param[in] IdBody (int)  : id of the concern body
  * @param[in] Tini (double) : initial temperature on the body
  * @endcond
  */
  extern "C" void mecaMAILx_InitTherField(int IdBody, double Tini);

 /**
  * @fn void mecaMAILx_UpdateTherField(int IdBody, double* vector_in, int length)
  * @brief Update the thermal field on a given body. The field is uniform.
  *
  * @cond PYDOC
  * python usage : mecaMAILx_UpdateTherField(IdBody, Tini)
  * @param[in] IdBody (integer)    : id of the concern body
  * @param[in] Tini (double array) : temperature
  * @endcond
  *
  * \n You need to initialize with InitTherField\n 
  *
  * @cond CDOC
  * @param[in] IdBody (int)               : id of the concern body
  * @param[in] vector_in (double[length]) : temperature field
  * @param[in] length (int)               : length of vector_in
  * @endcond
  */
  extern "C" void mecaMAILx_UpdateTherField(int IdBody, double* vector_in, int length);

