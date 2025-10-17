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

#ifndef wrap_nlgs_newarch_h
#define wrap_nlgs_newarch_h
  
/**
 * @fn void nlgs_newarch_ExPrep(char * cvalue1_c)
 * @brief Prepare matrix storage
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_ExPrep(storage)
 * @param[in] sotrage (char[30]) : matrix storage
 * @endcond
 *
 * \n prepare the matrix and the RHS of the contact problem
 * in regards of the selected matrix storage:\n 
 * - Exchange_Local_Global (the standard case)
 *  only the diagonal blocks are computed and stored.\n 
 * - Stored_Delassus_Loops (faster but memory expensive)
 *  the complete Delassus matrix is computed.\n 
 *
 * @cond CDOC
 * @param[in] cvalue1_c (char[30]) : matrix storage
 * @endcond
 */
 extern "C" void nlgs_newarch_ExPrep(char * cvalue1_c);

/**
 * @fn void nlgs_newarch_ExIter(int nb_iter)
 * @brief Execute NLGS iterations over the contact loop
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_ExIter(nb_iter)
 * param[in] nb_iter (integer) : number of iterations to do
 * @endcond
 *
 * @cond CDOC
 * param[in] nb_iter (int) : number of iterations to do
 * @endcond
 */
 extern "C" void nlgs_newarch_ExIter(int nb_iter);

/**
 * @fn void nlgs_newarch_ExPost(void)
 * @brief Run a jacobi iteration with the solution obtained with the NLGS algorithm
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_ExPost()
 * @endcond
 */
 extern "C" void nlgs_newarch_ExPost(void);

/**
 * @fn int nlgs_newarch_AfterIterCheck(void)
 * @brief Control NLGS convergence
 *
 * @cond PYDOC
 * python usage : convergence = nlgs_newarch_AfterIterCheck()
 * @return convergence (integer) :
 * @endcond
 *
 * @cond CDOC
 * @return convergence (int) :
 * @endcond
 */
 extern "C" int nlgs_newarch_AfterIterCheck(void);

/**
 * @fn void nlgs_newarch_DisplayAfterIterCheck(void)
 * @brief Display NLGS convergence results
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_DisplayAfterIterCheck()
 * @endcond
 */
 extern "C" void nlgs_newarch_DisplayAfterIterCheck(void);

/**
 * @fn void nlgs_newarch_NormCheck(void)
 * @brief Active one step norm evolution
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_NormCheck()
 * @endcond
 */
 extern "C" void nlgs_newarch_NormCheck(void);

/**
 * @fn void nlgs_newarch_UpdateTactBehav(void)
 * @brief Update internal parameters of contact lawz for each contact
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_UpdateTactBehav()
 * @endcond
 */
 //extern "C" void nlgs_newarch_UpdateTactBehav(void);

/**
 * @fn void nlgs_newarch_SetCheckType(char * cvalue1_c, double rvalue1, double rvalue2)
 * @brief Define numerical convergence of the NLGS algorithm
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_SetCheckType(check_type, tolerance, relaxation)
 * @param[in] check_type (char[5]) : type of convergence check
 * @param[in] tolerance (double)   : norm tolerance
 * @param[in] relaxation (double)  : relaxation factor
 * @endcond
 *
 * \n convergence check keywords:\n 
 * Quad  : quadratic norm (faulty contacts are redeemed by accurate
 *         contacts; laxist norm)\n 
 * Maxm  : maximum norm (faulty contacts must comply; severe norm)\n 
 * QM/16 : maximum of Quad and Maxm/16 norms (a compromise). For
 *         large dense collections Quad ranges usually around 1/16 Maxm\n 
 * where Quad,Maxm,QM/16 are keywords for the check test, and the 
 * following real number is the tolerance value.\n 
 *
 * @cond CDOC
 * @param[in] cvalue1_c (char[5]) : type of convergence check
 * @param[in] rvalue1   (double)  : norm tolerance
 * @param[in] rvalue2   (double)  : relaxation factor
 * @endcond
 */
 extern "C" void nlgs_newarch_SetCheckType(char * cvalue1_c, double rvalue1, double rvalue2);

/**
 * @fn void nlgs_newarch_ScrambleContactOrder(void)
 * @brief Random renumbering of the contact list
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_ScrambleContactOrder()
 * @endcond
 */
 extern "C" void nlgs_newarch_ScrambleContactOrder(void);

/**
 * @fn void nlgs_newarch_QuickScrambleContactOrder(void)
 * @brief Random renumbering of the contact list
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_QuickScrambleContactOrder()
 * @endcond
 */
 extern "C" void nlgs_newarch_QuickScrambleContactOrder(void);

/**
 * @fn void nlgs_newarch_SetWithQuickScramble(void)
 * @brief active quick scramble in macro function ExSolver
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_SetWithQuickScramble()
 * @endcond
 */
 extern "C" void nlgs_newarch_SetWithQuickScramble(void);

/**
 * @fn void nlgs_newarch_ReverseContactOrder(void)
 * @brief Reverse the numbering of the contact list
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_ReverseContactOrder()
 * @endcond
 */
 extern "C" void nlgs_newarch_ReverseContactOrder(void);

/**
 * @fn void nlgs_newarch_BimodalContactOrder(void)
 * @brief Renumbering of the contact list using the definition
 * of weak and strong network in granular assemblies
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_BimodalContactOrder()
 * @endcond
 */
// extern "C" void nlgs_newarch_BimodalContactOrder(void);

/**
 * @fn void nlgs_newarch_ScaleRloc(void)
 * @brief Scale all local contact forces of a factor equal to * 0.9 < f < 1.1
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_ScaleRloc()
 * @endcond
 */
 extern "C" void nlgs_newarch_ScaleRloc(void);

/**
 * @fn void nlgs_newarch_ComputeRnod(void)
 * @brief mapping from local contact forces to global ones
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_ComputeRnod()
 * @endcond
 */
 extern "C" void nlgs_newarch_ComputeRnod(void);

/**
 * @fn void nlgs_newarch_DisplayRlocNSum(void)
 * @brief Display the sum of normal contact forces
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_DisplayRlocNSum()
 * @endcond
 */
 extern "C" void nlgs_newarch_DisplayRlocNSum(void);

/**
 * @fn void nlgs_newarch_ExSolver(char * cvalue1_c, char * cvalue2_c, double rvalue1, double rvalue2, int ivalue1, int ivalue2)
 * @brief Solve fully the local contact problem
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_ExSolver(storage, checktype, tol, relax, nb_iter_check, nb_block_iter)
 * @param[in] storage (char[30])      : matrix storage (cf nlgs_newarch_ExPrep)
 * @param[in] checktype (char[5])     : convergentce test keyword
 * @param[in] tolerance (double)      : tolerance value
 * @param[in] relaxation (double)     : relaxation number
 * @param[in] nb_iter_check (integer) : number of iteration between convergence test
 * @param[in] nb_block_iter (integer) : number of block iterations
 * @endcond
 *
 * @cond CDOC
 * @param[in] cvalue1 (char[30]) : matrix storage (cf nlgs_newarch_ExPrep)
 * @param[in] cvalue2 (char[5])  : convergentce test keyword
 * @param[in] rvalue1 (double)   : tolerance value
 * @param[in] rvalue2 (double)   : relaxation number
 * @param[in] ivalue1 (int)      : number of iteration between convergence test
 * @param[in] ivalue2 (int)      : number of block iterations
 * @endcond
 */
 extern "C" void nlgs_newarch_ExSolver(char * cvalue1_c, char * cvalue2_c, double rvalue1, double rvalue2, int ivalue1, int ivalue2);

/**
 * @fn void nlgs_newarch_InitCohesiveBehav(void)
 * @brief update internal parameters of contact laws for each contact
 *
 * @cond PYDOC
 * python usage : nlgs_newarch_InitCohesiveBehav(void)
 * @endcond
 */
 //extern "C" void nlgs_newarch_InitCohesiveBehav(void);

/**
  * @fn void nlgs_newarch_GetAllThis(double** matrix_out, int* dim1, int* dim2)
  * @brief Get all interactions in "this" array
  *
  * Each interaction has (in this order): coor, tuc, nuc, (suc,) rlt, rln, (rls,) vlt, vln (, vls)
  * @cond PYDOC
  * usage : interactions = nlgs_newarch_GetAllThis()
  * @return interactions (double 2D-array) : the interactions
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] matrix_out (double**) : array of interaction data
  * @param[out]    dim1 (int*)           : number of interaction
  * @param[out]    dim2 (int*)           : number of real data per interaction
  * @endcond
  */
  extern "C" void nlgs_newarch_GetAllThis(double** matrix_out, int* dim1, int* dim2);

#endif /* wrap_nlgs_newarch */
