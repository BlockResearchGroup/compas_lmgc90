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

#ifndef wrap_inter_handler_3D_h
#define wrap_inter_handler_3D_h

 /**
  * @fn int inter_handler_3D_tgetNb(int inter_id )
  * @brief return the number of interactions of the selected type stored in this data structure
  *
  * @cond PYDOC
  * python usage : nb_inter = inter_handler_3D_tgetNb(inter_id)
  * @param[in] inter_id (integer) : type of interaction (lmgc90 parameter)
  * @return    nb_inter (integer) : number of interaction found of selected type
  * @endcond
  *
  * @cond CDOC
  * @param[in] inter_id (int) : type of interaction (lmgc90 parameter)
  * @return    nb_inter (int) : number of interaction found of selected type
  * @endcond
  */
  extern "C" int inter_handler_3D_tgetNb(int inter_id);

 /**
  * @fn int inter_handler_3D_tgetTactLawNb(int inter_id, int icdan)
  * @brief return the contact law number of an interaction stored in this data structure
  *
  * @cond PYDOC
  * python usage : tact_law = inter_handler_3D_tgetTactLawNb(inter_id, icdan)
  * @param[in] inter_id (integer) : type of interaction (lmgc90 parameter)
  * @param[in] icdan    (integer) : index of the interaction of selected type
  * @return    tact_law (integer) : contact law number
  * @endcond
  *
  * @cond CDOC
  * @param[in] inter_id (int) : type of interaction (lmgc90 parameter)
  * @param[in] icdan    (int) : index of the interaction of selected type
  * @return    tact_law (int) : contact law number
  * @endcond
  */
  extern "C" int inter_handler_3D_tgetTactLawNb(int inter_id, int icdan);

/**
  * @fn void inter_handler_3D_tgetIdBodies(int inter_id, int icdan, int** i4_vector, int* i4_size)
  * @brief return the serial numbers of contacting objects of an interaction stored in this data structure
  *
  * @cond PYDOC
  * python usage : idBodies = inter_handler_3D_tgetIdBodies(inter_id, icdan)
  * @param[in] inter_id (integer) : type of interaction (lmgc90 parameter)
  * @param[in] icdan    (integer) : index of the interaction of selected type
  * @return    idBodies (integer) : array with cd and an bodies serial number 
  * @endcond
  *
  * @cond CDOC
  * @param[in]  inter_id (int)   : type of interaction (lmgc90 parameter)
  * @param[in]  icdan    (int)   : index of the interaction of selected type
  * @param[out] i4_vector(int**) : array with cd and an bodies serial number 
  * @param[out] i4_size  (int*)  : size of vector 
  * @endcond
  */
  extern "C" void inter_handler_3D_tgetIdBodies(int inter_id, int icdan, int** i4_vector, int* i4_size);

 /**
  * @fn void inter_handler_3D_tgetIData(int inter_id, int icdan, int ** i4_vector, int * i4_size)
  * @brief Get the integer data of an interaction stored in this data structure
  *
  * idata vector holds cd body type, an body type, cd body id, an body id,
  * cd contactor type, an contactory type, cd contactor id, an contactor id,
  * cd subcontactor id, an subcontactor id, tact law id, status, number of internals
  *
  * @cond PYDOC
  * usage : idata = inter_handler_3D_tgetIData(inter_id, icdan)
  *
  * @param[in] inter_id (integer)  : type of interaction (lmgc90 parameter)
  * @param[in] icdan    (integer)  : index of the interaction of selected type
  * @return idata (integer array)  : the values array
  * @endcond
  *
  * @cond CDOC
  * @param[in] inter_id   (int)    : type of interaction (lmgc90 parameter)
  * @param[in] icdan      (int)    : index of the interaction of selected type
  * @param[out] i4_vector (int **) : the array with idata
  * @param[out] i4_size (int *)    : size of i4_vector
  * @endcond
  *
  */
  extern "C" void inter_handler_3D_tgetIData(int inter_id, int icdan, int ** i4_vector, int * i4_size);


/**
  * @fn void inter_handler_3D_tgetRData(int inter_id, int icdan, double** r8_vector, int* r8_size)
  * @brief return the real data associated with an interactions
  *
  * Get an output array with, in this order, : coor, t/n/suc, rlt/n/s, vlt/n/s, gapTT
  *
  * @cond PYDOC
  * python usage : rdata = inter_handler_3D_tgetRData(inter_id, icdan)
  * @param[in] inter_id (integer)      : type of interaction (lmgc90 parameter)
  * @param[in] icdan    (integer)      : index of the interaction of selected type
  * @return    rdata    (double array) : array with real data of the interaction
  * @endcond
  *
  * @cond CDOC
  * @param[in]  inter_id (int)      : type of interaction (lmgc90 parameter)
  * @param[in]  icdan    (int)      : index of the interaction of selected type
  * @param[out] r8_vector(double**) : array with real data of the interaction
  * @param[out] r8_size  (int*)     : size of vector 
  * @endcond
  */
  extern "C" void inter_handler_3D_tgetRData(int inter_id, int icdan, double** r8_vector, int* r8_size);

 /**
  * @fn void inter_handler_3D_tsetInternal(int inter_id, int icdan, double * rvector_in=nullptr, int rlength_in=0, int index=0, double value=0.)
  * @brief Set the internal of an interaction (either the array or a single value) stored in this data structure
  *
  * Uses copy.
  * If internal array is provided, the whole array is set. Otherwise index and value must
  * be provided and a single value is set.\n
  *
  * @cond PYDOC
  * usage : inter_handler_3D_tsetInternal(inter_id, icdan, internal)
  *   or    inter_handler_3D_tsetInternal(inter_id, icdan, index, value)
  *
  * @param[in] inter_id (integer)      : type of interaction (lmgc90 parameter)
  * @param[in] icdan    (integer)      : index of the interaction of selected type
  * @param[in] internal (double array) : the new values array
  * @param[in] index    (integer)      : the index where to set single value
  * @param[in] value    (double )      : the new value to put at index
  * @endcond
  *
  * @cond CDOC
  * @param[in] inter_id   (int)      : type of interaction (lmgc90 parameter)
  * @param[in] icdan      (int)      : index of the interaction of selected type
  * @param[in] rvector_in (double *) : the new vector
  * @param[in] rlength_in (int)      : size of rvector_in
  * @param[in] index      (int)      : the index where to set value
  * @param[in] value      (double)   : the new value to set
  * @endcond
  *
  */
  extern "C" void inter_handler_3D_tsetInternal(int inter_id, int icdan, double * rvector_in=nullptr, int rlength_in=0, int index=0, double value=0.);

 /**
  * @fn void inter_handler_3D_tgetInternal(int inter_id, int icdan, double ** r8_vector, int * r8_size)
  * @brief Get the internal of an interaction stored in this data structure
  *
  * @cond PYDOC
  * usage : internal = inter_handler_3D_tgetInternal(inter_id, icdan)
  *
  * @param[in] inter_id (integer)      : type of interaction (lmgc90 parameter)
  * @param[in] icdan    (integer)      : index of the interaction of selected type
  * @param[in] internal (double array) : the new values array
  * @endcond
  *
  * @cond CDOC
  * @param[in] inter_id   (int)      : type of interaction (lmgc90 parameter)
  * @param[in] icdan      (int)      : index of the interaction of selected type
  * @param[in] rvector_out (double *) : the new vector
  * @param[in] rlength_out (int)      : size of rvector_out
  * @endcond
  *
  */
  extern "C" void inter_handler_3D_tgetInternal(int inter_id, int icdan, double ** r8_vector, int * r8_size);

 /**
  * @fn int inter_handler_3D_getNbRecup(int inter_id )
  * @brief return the number of recup interactions of the selected type
  *
  * @cond PYDOC
  * python usage : nb_recup = inter_handler_3D_getNbRecup(inter_id)
  * @param[in] inter_id (integer) : type of interaction (lmgc90 parameter)
  * @return    nb_recup (integer) : number of interaction recup of selected type
  * @endcond
  *
  * @cond CDOC
  * @param[in] inter_id (int) : type of interaction (lmgc90 parameter)
  * @return    nb_recup (int) : number of interaction recup of selected type
  * @endcond
  */
  extern "C" int inter_handler_3D_getNbRecup(int inter_id);

 /**
  * @fn int inter_handler_3D_getNb(int inter_id )
  * @brief return the number of interactions of the selected type stored in verlet data structure
  *
  * @cond PYDOC
  * python usage : nb_inter = inter_handler_3D_getNb(inter_id)
  * @param[in] inter_id (integer) : type of interaction (lmgc90 parameter)
  * @return    nb_inter (integer) : number of interaction found of selected type
  * @endcond
  *
  * @cond CDOC
  * @param[in] inter_id (int) : type of interaction (lmgc90 parameter)
  * @return    nb_inter (int) : number of interaction found of selected type
  * @endcond
  */
  extern "C" int inter_handler_3D_getNb(int inter_id);

 /**
  * @fn void inter_handler_3D_getAllTactLawNb(int inter_id, int ** vector_out, int * dim1)
  * @brief return the tact law number of all interactions stored in verlet data structure
  *
  * @cond PYDOC
  * python usage : vector = inter_handler_3D_getAllTactLawNb(inter_id)
  * @param[in] inter_id (integer)         : type of interaction (lmgc90 parameter)
  * @return    vector   (int 1D-array)    : mechanical data
  * @endcond
  *
  * @cond CDOC
  * @param[in]     inter_id (int)         : type of interaction (lmgc90 parameter)
  * @param[in,out] vector_out (int **)    : xxx
  * @param[in]     dim1 (int *)           : vector_out dimension
  * @endcond
  */
  extern "C" void inter_handler_3D_getAllTactLawNb(int inter_id, int ** i4_vector, int * i4_size);

 /**
  * @fn void inter_handler_3D_getAll(int inter_id, double ** matrix_out, int * dim1, int * dim2)
  * @brief return coorx,coory,coorz,tx,ty,tz,nx,ny,nz,sx,sy,sz,rlt,rln,rls,vlt,vln,vls,gaptt of all 'verlet' interactions
  *
  * @cond PYDOC
  * python usage : array = inter_handler_3D_getAll(inter_id)
  * @param[in] inter_id (integer)         : type of interaction (lmgc90 parameter)
  * @return    array    (double 2D-array) : mechanical data
  * @endcond
  *
  * @cond CDOC
  * @param[in]     inter_id (int)         : type of interaction (lmgc90 parameter)
  * @param[in,out] matrix_out (double **) : xxx
  * @param[in]     dim1 (int *)           : matrix_out first dimension
  * @param[in]     dim2 (int *)           : matrix_out second dimension
  * @endcond
  */
  extern "C" void inter_handler_3D_getAll(int inter_id, double ** matrix_out, int * dim1, int * dim2);
  
 /**
  * @fn void inter_handler_3D_getAllInternal(int inter_id, double ** matrix_out, int * dim1, int * dim2)
  * @brief return contact point internal variables of all 'verlet' interactions
  *
  * @cond PYDOC
  * python usage : array = inter_handler_3D_getAllInternal()
  * @param[in] inter_id (integer)         : type of interaction (lmgc90 parameter)
  * @return    array    (double 2D-array) : mechanical data
  * @endcond
  *
  * @cond CDOC
  * @param[in]     inter_id (int)         : type of interaction (lmgc90 parameter)
  * @param[in,out] matrix_out (double **) : xxx
  * @param[in]     dim1 (int *)           : matrix_out first dimension
  * @param[in]     dim2 (int *)           : matrix_out second dimension
  * @endcond
  */
  extern "C" void inter_handler_3D_getAllInternal(int inter_id, double ** matrix_out, int * dim1, int * dim2);

 /**
  * @fn void inter_handler_3D_getAllIdata(int inter_id, int ** matrix_out, int * dim1, int * dim2)
  * @brief return all integer data of all 'verlet' interaction
  *
  * Which are in order cd body type, an body type, cd body id, an body id,
  * cd contactor type, an contactory type, cd contactor id, an contactor id,
  * cd subcontactor id, an subcontactor id, tact law id, status, number of internals
  *
  * @cond PYDOC
  * python usage : array = inter_handler_3D_getAllIdata(inter_id)
  * @param[in] inter_id (integer)      : type of interaction (lmgc90 parameter)
  * @return    array    (int 2D-array) : identification data
  * @endcond
  *
  * @cond CDOC
  * @param[in]     inter_id (int)      : type of interaction (lmgc90 parameter)
  * @param[in,out] matrix_out (int **) : identification data
  * @param[in]     dim1 (int *)        : matrix_out first dimension
  * @param[in]     dim2 (int *)        : matrix_out second dimension
  * @endcond
  */
  extern "C" void inter_handler_3D_getAllIdata(int inter_id, int ** matrix_out, int * dim1, int * dim2);
  
 /**
  * @fn int inter_handler_3D_getVerletAdjsz(int inter_id, int icdtac)
  * @brief return integer number of verlet interaction of a candidate
  *
  * @cond PYDOC
  * python usage : iantac = inter_handler_3D_getVerletAdjsz(inter_id, icdtac)
  * @param[in] inter_id (integer) : type of interaction (lmgc90 parameter)
  * @param[in] icdtac   (integer) : candidate contactor id
  * @return    iantac   (integer) : number of verlet interactions on candidate
  * @endcond
  *
  * @cond CDOC
  * @param[in] inter_id (int) : type of interaction (lmgc90 parameter)
  * @param[in] icdtac   (int) : candidate contactor id
  * @return    iantac   (int) : number of verlet interactions on candidate
  * @endcond
  */
  extern "C" int inter_handler_3D_getVerletAdjsz(int inter_id, int icdtac);

 /**
  * @fn int inter_handler_3D_getVerletIantac(int inter_id, int icdtac, int iadj)
  * @brief return integer antagonist contact of a verlet interaction
  *
  * @cond PYDOC
  * python usage : iantac = inter_handler_3D_getVerletIantac(inter_id, icdtac, iadj)
  * @param[in] inter_id (integer) : type of interaction (lmgc90 parameter)
  * @param[in] icdtac   (integer) : candidate contactor id
  * @param[in] iadj     (integer) : id of adjacent of candidate
  * @return    iantac   (integer) : id of antagonist contactor corresponding to verlet interaction
  * @endcond
  *
  * @cond CDOC
  * @param[in] inter_id (int) : type of interaction (lmgc90 parameter)
  * @param[in] icdtac   (int) : candidate contactor id
  * @param[in] iadj     (int) : id of adjacent of candidate
  * @return    iantac   (int) : id of antagonist contactor corresponding to verlet interaction
  * @endcond
  */
  extern "C" int inter_handler_3D_getVerletIantac(int inter_id, int icdtac, int iadj);

 /**
  * @fn void inter_handler_3D_computeRnod(void)
  * @brief Put back the Reac value of bodies from (this) interactions
  */
  extern "C" void inter_handler_3D_computeRnod(void);

 /**
  * @fn void inter_handler_3D_redoNbAdj(int inter_id)
  * @brief Redo the adjacent counting array for a interaction type
  *
  * @cond PYDOC
  * python usage : inter_handler_3D_redoNbAdj(inter_id)
  * @param[in] inter_id (integer) : type of interaction (lmgc90 parameter)
  * @endcond
  *
  * @cond CDOC
  * @param[in] inter_id (int) : type of interaction (lmgc90 parameter)
  * @endcond
  */
  extern "C" void inter_handler_3D_redoNbAdj(int inter_id);

 /**
  * @fn int inter_handler_3D_stockRloc(int inter_id)
  * @brief stock from this to verlet
  *
  * @cond PYDOC
  * python usage : inter_handler_3D_stockRloc(inter_id)
  * @param[in] inter_id (integer) : type of interaction (lmgc90 parameter)
  * @endcond
  *
  * @cond CDOC
  * @param[in] inter_id (int) : type of interaction (lmgc90 parameter)
  * @endcond
  */
  extern "C" void inter_handler_3D_stockRloc(int inter_id);

 /**
  * @fn int inter_handler_3D_recupRloc(int inter_id)
  * @brief recup from verlet to this
  *
  * @cond PYDOC
  * python usage : inter_handler_3D_recupRloc(inter_id)
  * @param[in] inter_id (integer) : type of interaction (lmgc90 parameter)
  * @endcond
  *
  * @cond CDOC
  * @param[in] inter_id (int) : type of interaction (lmgc90 parameter)
  * @endcond
  */
  extern "C" void inter_handler_3D_recupRloc(int inter_id);

 /**
  * @fn int inter_handler_3D_recupRlocByPos(int inter_id, double rtol)
  * @brief recup from verlet to this using position as criteria
  *
  * Only available for CSASp inter_id
  *
  * @cond PYDOC
  * python usage : inter_handler_3D_recupRloc(inter_id, rtol)
  * @param[in] inter_id (integer) : type of interaction (lmgc90 parameter)
  * @param[in] rtol     (real)    : tolerance to decide if contact is recup
  * @endcond
  *
  * @cond CDOC
  * @param[in] inter_id (int)    : type of interaction (lmgc90 parameter)
  * @param[in] rtol     (double) : tolerance to decide if contact is recup
  * @endcond
  */
  extern "C" void inter_handler_3D_recupRlocByPos(int inter_id, double rtol);

#endif /* wrap_inter_handler_3D_h */
