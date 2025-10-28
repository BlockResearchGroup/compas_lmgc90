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

#ifndef wrap_DDM_2D_h
#define wrap_DDM_2D_h

/**
 * @fn void DDM_2D_Partioning(void)
 * @brief Working directories for each subdomain
 *
 * @cond PYDOC
 * python usage : DDM_2D_SetWorkingDirectory()
 * @endcond
 *
 */
 extern "C" void DDM_2D_SetDDWorkingDirectory(void);

/**
 * @fn void DDM_2D_Initialize(int nb_sdmx, int nb_sdmy, int ddm_type)
 * @brief Initialize 2D DDM module
 *
 * @cond PYDOC
 * python usage : DDM_2D_Initialize(nb_sdmx, nb_sdmy, ddm_type)
 * @param nb_sdmx (integer)  : number of domains on x-axis
 * @param nb_sdmy (integer)  : number of domains on y-axis
 * @param ddm_type (integer) : type of DDM to use
 * @endcond
 *
 * @cond CDOC
 * @param[in] nb_sdmx (int)  : number of domains on x-axis
 * @param[in] nb_sdmy (int)  : number of domains on y-axis
 * @param[in] ddm_type (int) : type of DDM to use
 * @endcond
 *
 * ddm_type may be :
 * - 1 for Feti (DDM without overlap)
 * - 2 for Schwarz (DDM with overlap)
 */
 extern "C" void DDM_2D_Initialize(int nb_sdmx, int nb_sdmy, int ddm_type);

/**
 * @fn void DDM_2D_Partioning(void)
 * @brief Domain partitioning
 *
 * @cond PYDOC
 * python usage : DDM_2D_Partitioning()
 * @endcond
 *
 */
 extern "C" void DDM_2D_Partitioning(void);

/**
 * @fn void DDM_2D_AddToFext(void)
 * @brief Add external forces due to DDM
 *
 * @cond PYDOC
 * python usage : DDM_2D_AddToFext()
 * @endcond
 *
 * To add after RBDY2_ComputeFext
 *
 */
 extern "C" void DDM_2D_AddToFext(void);

/**
 * @fn void DDM_2D_ExSolver(char * cvalue1_c, char * cvalue2_c, double rvalue1, double rvalue2, int ivalue1, int ivalue2)
 * @brief Solve fully the local contact problem with DDM
 *
 * @cond PYDOC
 * python usage : DDM_2D_ExSolver(storage, checktype, tol, relax, nb_iter_check, nb_block_iter)
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
 extern "C" void DDM_2D_ExSolver(char * cvalue1_c, char * cvalue2_c, double rvalue1, double rvalue2, int ivalue1, int ivalue2);

/**
 * @fn void DDM_2D_ComputeDof(void)
 * @brief Compute degrees of freedom
 *
 * @cond PYDOC
 * python usage : DDM_2D_ComputeDof()
 * @endcond
 *
 */
 extern "C" void DDM_2D_ComputeDof(void);

/**
 * @fn void DDM_2D_Post(void)
 * @brief Does postpro operation related to DDM
 *
 * @cond PYDOC
 * python usage : DDM_2D_Post()
 * @endcond
 *
 */
 extern "C" void DDM_2D_Post(void);

/**
 * @fn void DDM_2D_SetParameters(int f_ddm, int f_out, int f_last, int f_postpro, int f_display)
 * @brief Set frequencies parameters of DDM
 *
 * @cond PYDOC
 * python usage : DDM_2D_SetParameters(f_ddm, f_out, f_last, f_postpro, f_display)
 * @param f_ddm     (integer) : frequency of ddm partionning
 * @param f_out     (integer) : frequency of output file writing
 * @param f_last    (integer) : frequency of last file writing
 * @param f_postpro (integer) : frequency of postpro file writing
 * @param f_display (integer) : frequency of display file writing
 * @endcond
 *
 * @cond CDOC
 * @param[in] f_ddm     (int) : frequency of ddm partionning
 * @param[in] f_out     (int) : frequency of output file writing
 * @param[in] f_last    (int) : frequency of last file writing
 * @param[in] f_postpro (int) : frequency of postpro file writing
 * @param[in] f_display (int) : frequency of display file writing
 * @endcond
 *
 */
 extern "C" void DDM_2D_SetParameters(int f_ddm, int f_out, int f_last, int f_postpro, int f_display);

/**
 * @fn void DDM_2D_WriteLast(void)
 * @brief Write some data at the end of computation
 *
 * @cond PYDOC
 * python usage : DDM_2D_WriteLast()
 * @endcond
 *
 */
 extern "C" void DDM_2D_WriteLast(void);

/**
 * @fn void DDM_2D_Finalize(void)
 * @brief End of computation/module's life
 *
 * @cond PYDOC
 * python usage : DDM_2D_Finalize()
 * @endcond
 *
 */
 extern "C" void DDM_2D_Finalize(void);

#endif /* wrap_DDM_2D_h */
