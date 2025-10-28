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

#ifndef wrap_MBS2D_h
#define wrap_MBS2D_h

 /**
  * @fn void MBS2D_setNb(int nb)
  * @brief Set the number of MBS
  *
  * @cond PYDOC
  * python usage : MBS2D_setNb(nb)
  * @param[in] nb (integer) : set the number of MBS
  * @endcond
  *
  * @cond CDOC
  * @param[in] nb (int) : set the number of MBS
  * @endcond
  */
  extern "C" void MBS2D_setNb(int nb);

 /**
  * @fn int MBS2D_getNb(void)
  * @brief Get the number of MBS
  *
  * @cond PYDOC
  * python usage : nb = MBS2D_getNb()
  * @return nb (integer) : the number of MBS
  * @endcond
  *
  * @cond CDOC
  * @return (int) : the number of MBS
  * @endcond
  */
  extern "C" int MBS2D_getNb(void);

 /**
  * @fn void MBS2D_setNbNodes(int ibdyty, int nb)
  * @brief Set the number of nodes of a MBS
  *
  * @cond PYDOC
  * python usage : MBS2D_setNbNodes(ibdyty, nb)
  * @param[in] ibdyty(integer) : id of the MBS
  * @param[in] nb (integer) : the number of nodes of the MBS
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : id of the MBS
  * @param[in] nb (int)    : the number of nodes of the MBS
  * @endcond
  */
  extern "C" void MBS2D_setNbNodes(int ibdyty, int nb);

 /**
  * @fn void MBS2D_setNbTactors(int ibdyty, int nb)
  * @brief Set the number contactors of a MBS
  *
  * @cond PYDOC
  * python usage : MBS2D_setNbTactors(ibdyty, nb)
  * @param[in] ibdyty(integer) : id of the MBS
  * @param[in] nb (integer) : the number of contactor of the MBS
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int) : id of the MBS
  * @param[in] nb (int)    : the number of contactor of the MBS
  * @endcond
  */
  extern "C" void MBS2D_setNbTactors(int ibdyty, int nb);

 /**
  * @fn void MBS2D_getPtrCoor(int ibdyty, double** pointer_out, int* dim1, int* dim2)
  * @brief Get a pointer on the coor of a MBS
  *
  * @cond PYDOC
  * python usage : coor = MBS2D_getPtrCoor(ibdyty)
  *
  * @param[in]  ibdyty (integer)     : rank of considered MBS
  *
  * @return coor (double 2D-array)   : reference on the coordinates of the nodes
  * @endcond
  *
  * @cond CDOC
  * @param[in]     ibdyty (int)           : rank of considered MBS
  * @param[in,out] pointer_out (double**) : reference on the coordinates of the nodes
  * @param[out]    dim1 (int*)            : number of dof per node
  * @param[out]    dim2 (int*)            : number of nodes
  * @endcond
  */
  extern "C" void MBS2D_getPtrCoor(int ibdyty, double** pointer_out, int* dim1, int* dim2);

 /**
  * @fn void MBS2D_getPtrCoorTT(int ibdyty, double** pointer_out, int* dim1, int* dim2)
  * @brief Set the array of coordinates of nodes of a MBS
  *
  * @cond PYDOC
  * python usage : coorTT = MBS2D_GetPtrCoorTT(ibdyty)
  *
  * @param[in] ibdyty(integer) : id of the MBS
  *
  * @return coorTT (double array) : coordinates of nodes of a MBS
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty(int)     : id of the MBS
  * @param[in] coor (double *) : coordinates of nodes of a MBS
  * @param[in] dim1 (int)      : number of nodes in the mbs
  * @param[in] dim2 (int)      : space dim
  * @endcond
  */
  extern "C" void MBS2D_getPtrCoorTT(int ibdyty, double** pointer_out, int* dim1, int* dim2);

 /**
  * @fn void MBS2D_addContactor(int ibdyty, int inodty, int itacty, char * tactype, char * color, double * rvector_in, int rlength_in, int * ivector_in, int ilength_in)
  * @brief Add a new contactor to a MBS
  *
  *  Available contactor types are :
  *  - JONCx: inputs are:
  *    - rdata must hold [axe_x, axe_y]
  *  - POLYR: inputs are:
  *    - rdata must hold the coordinates of the vertices [x_1, y_1,  ... x_n, y_n]
  *    - idata must hold the number of vertices
  *
  * @cond PYDOC
  * python usage : MBS2D_addContactor(ibdyty, inodty, itacty, tacttype, color, rdata, idata=None)
  * @param[in] ibdyty (integer)      : rank of the MBS
  * @param[in] inodty (integer)      : rank of the node of the MBS the contactor is tied to
  * @param[in] itacty (integer)      : rank of the contactor of MBS
  * @param[in] tactype (string [5])  : the type of contactor
  * @param[in] color   (string [5])  : the color of the contactor
  * @param[in] rdata (double array)  : the new value of the vector
  * @param[in] idata (integer array) : the new value of the vector
  * @endcond
  *
  * @cond CDOC
  * @param[in] ibdyty (int)          : rank of the MBS
  * @param[in] inodty (int)          : rank of the node of the MBS the contactor is tied to
  * @param[in] itacty (int)          : rank of the contactor of the MBS
  * @param[in] tactype (char[5])     : the type of contactor to add
  * @param[in] color (char[5])       : the color of the contactor to add
  * @param[in] rvector_in (double *) : the real data describing the contactor
  * @param[in] rlength_in (int)      : the length of rvector_in
  * @param[in] ivector_in (int *)    : the integer data describing the contactor
  * @param[in] ilength_in (int)      : the length of ivector_in
  * @endcond
  *
  */
  extern "C" void MBS2D_addContactor(int ibdyty, int inodty, int itacty, char * tactype, char * color, double * rvector_in, int rlength_in, int * ivector_in=nullptr, int ilength_in=0);

 /**
  * @fn void MBS2D_initialize(void)
  * @brief Initialize MBS module once loading is done
  *
  * @cond PYDOC
  * python usage : MBS2D_initialize()
  * @endcond
  *
  */
  extern "C" void MBS2D_initialize(void);

 /**
  * @fn void MBS2D_finalize(void)
  * @brief Finalize MBS module 
  *
  * @cond PYDOC
  * python usage : MBS2D_finalize()
  * @endcond
  *
  */
  extern "C" void MBS2D_finalize(void);


 /**
  * @fn void MBS2D_IncrementStep(void)
  * @brief compute the current velocity and displacement
  *
  * @cond PYDOC
  * python usage : MBS2D_IncrementStep()
  * @endcond
 */
 extern "C" void MBS2D_IncrementStep(void);



 /**
  * @fn void MBS2D_ComputeFreeVelocity(void)
  * @brief compute free velocity 
  *
  * @cond PYDOC
  * python usage : MBS2D_ComputeFreeVelocity()
  * @endcond
 */
 extern "C" void MBS2D_ComputeFreeVelocity(void);


 /**
  * @fn void MBS2D_ComputeDof(void)
  * @brief update current position and velocity
  *
  * @cond PYDOC
  * python usage : MBS2D_ComputeDof()
  * @endcond
 */
 extern "C" void MBS2D_ComputeDof(void);

 /**
  * @fn void MBS2D_UpdateDof(void)
  * @brief save d.o.f. of the end of the time step to d.o.f. of the begining of the next one
  *
  * @cond PYDOC
  * python usage : MBS2D_UpdateDof()
  * @endcond
 */
 extern "C" void MBS2D_UpdateDof(void);


#endif /* wrap_MBS2D_h */
