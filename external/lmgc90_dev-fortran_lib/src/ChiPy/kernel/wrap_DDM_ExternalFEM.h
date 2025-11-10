/*==========================================================================
 *
 * Copyright 2000-2025 CNRS-UM.
 *
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

#ifndef wrap_DDM_ExternalFEM_h
#define wrap_DDM_ExternalFEM_h

/**
 * @fn void DDM_ExternalFEM_SetDDWorkingDirectory(void)
 * @brief Working directories for each subdomain
 *
 * @cond PYDOC
 * python usage : DDM_ExternalFEM_SetDDWorkingDirectory()
 * @endcond
 *
 */
 extern "C" void DDM_ExternalFEM_SetDDWorkingDirectory(void);
 
/**
 * @fn void DDM_ExternalFEM_ExSolver(char * cvalue1_c, char * cvalue2_c, double rvalue1, double rvalue2, int ivalue1, int ivalue2)
 * @brief Solve fully the local contact problem in DDM
 *
 * @cond PYDOC
 * python usage : DDM_ExternalFEM_ExSolver(storage, checktype, tol, relax, nb_iter_check, nb_block_iter)
 * @param[in] storage (char[30])      : matrix storage (cf nlgs_ExPrep)
 * @param[in] checktype (char[5])     : convergentce test keyword
 * @param[in] tolerance (double)      : tolerance value
 * @param[in] relaxation (double)     : relaxation number
 * @param[in] nb_iter_check (integer) : number of iteration between convergence test
 * @param[in] nb_block_iter (integer) : number of block iterations
 * @endcond
 *
 * @cond CDOC
 * @param[in] cvalue1 (char[30]) : matrix storage (cf nlgs_ExPrep)
 * @param[in] cvalue2 (char[5])  : convergentce test keyword
 * @param[in] rvalue1 (double)   : tolerance value
 * @param[in] rvalue2 (double)   : relaxation number
 * @param[in] ivalue1 (int)      : number of iteration between convergence test
 * @param[in] ivalue2 (int)      : number of block iterations
 * @endcond
 */
 extern "C" void DDM_ExternalFEM_ExSolver(char * cvalue1_c, char * cvalue2_c, double rvalue1, double rvalue2, int ivalue1, int ivalue2);
 
 /**
 * @fn void DDM_ExternalFEM_ExSolver_3D(char * Wstorage_c, char * checktype_c, double tol, double RELAX, int nb_iter_check, int nb_block_iter)
 * @brief Solve fully the local contact problem
 *
 * @cond PYDOC
 * python usage : DDM_ExternalFEM_ExSolver_3D(storage, checktype, tol, relax, nb_iter_check, nb_block_iter)
 * @param[in] storage (char[30])      : matrix storage (cf nlgs_ExPrep)
 * @param[in] checktype (char[5])     : convergentce test keyword
 * @param[in] tolerance (double)      : tolerance value
 * @param[in] relaxation (double)     : relaxation number
 * @param[in] nb_iter_check (integer) : number of iteration between convergence test
 * @param[in] nb_block_iter (integer) : number of block iterations
 * @endcond
 *
 * @cond CDOC
 * @param[in] Wstorage_c  (char[30]) : matrix storage (cf nlgs_ExPrep)
 * @param[in] checktype_c (char[5])  : convergentce test keyword
 * @param[in] tol         (double)   : tolerance value
 * @param[in] RELAX       (double)   : relaxation number
 * @param[in] nb_iter_check (int)    : number of iteration between convergence test
 * @param[in] nb_block_iter (int)    : number of block iterations
 * @endcond
 */
 extern "C" void DDM_ExternalFEM_ExSolver_3D(char * Wstorage_c, char * checktype_c, double tol, double RELAX,
            int nb_iter_check, int nb_block_iter);

#endif /* wrap_DDM_ExternalFEM_h */
