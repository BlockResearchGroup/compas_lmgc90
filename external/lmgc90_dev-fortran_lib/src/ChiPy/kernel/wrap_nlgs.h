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

#ifndef wrap_nlgs_h
#define wrap_nlgs_h
  
/**
 * @fn void nlgs_ExPrep(char * cvalue1_c)
 * @brief Prepare matrix storage
 *
 * @cond PYDOC
 * python usage : nlgs_ExPrep(storage)
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
 extern "C" void nlgs_ExPrep(char * cvalue1_c);

/**
 * @fn void nlgs_ExIter(int nb_iter)
 * @brief Execute NLGS iterations over the contact loop
 *
 * @cond PYDOC
 * python usage : nlgs_ExIter(nb_iter)
 * param[in] nb_iter (integer) : number of iterations to do
 * @endcond
 *
 * @cond CDOC
 * param[in] nb_iter (int) : number of iterations to do
 * @endcond
 */
 extern "C" void nlgs_ExIter(int nb_iter);

/**
 * @fn void nlgs_ExPost(void)
 * @brief Run a jacobi iteration with the solution obtained with the NLGS algorithm
 *
 * @cond PYDOC
 * python usage : nlgs_ExPost()
 * @endcond
 */
 extern "C" void nlgs_ExPost(void);

/**
 * @fn int nlgs_AfterIterCheck(void)
 * @brief Control NLGS convergence
 *
 * @cond PYDOC
 * python usage : convergence = nlgs_AfterIterCheck()
 * @return convergence (integer) :
 * @endcond
 *
 * @cond CDOC
 * @return convergence (int) :
 * @endcond
 */
 extern "C" int nlgs_AfterIterCheck(void);

/**
 * @fn void nlgs_DisplayAfterIterCheck(void)
 * @brief Display NLGS convergence results
 *
 * @cond PYDOC
 * python usage : nlgs_DisplayAfterIterCheck()
 * @endcond
 */
 extern "C" void nlgs_DisplayAfterIterCheck(void);

/**
 * @fn void nlgs_NormCheck(void)
 * @brief Active one step norm evolution
 *
 * @cond PYDOC
 * python usage : nlgs_NormCheck()
 * @endcond
 */
 extern "C" void nlgs_NormCheck(void);

/**
 * @fn void nlgs_UpdateTactBehav(void)
 * @brief Update internal parameters of contact lawz for each contact
 *
 * @cond PYDOC
 * python usage : nlgs_UpdateTactBehav()
 * @endcond
 */
 extern "C" void nlgs_UpdateTactBehav(void);

/**
 * @fn void nlgs_SetCheckType(char * cvalue1_c, double rvalue1, double rvalue2)
 * @brief Define numerical convergence of the NLGS algorithm
 *
 * @cond PYDOC
 * python usage : nlgs_SetCheckType(check_type, tolerance, relaxation)
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
 extern "C" void nlgs_SetCheckType(char * cvalue1_c, double rvalue1, double rvalue2);

/**
 * @fn void nlgs_ScrambleContactOrder(void)
 * @brief Random renumbering of the contact list
 *
 * @cond PYDOC
 * python usage : nlgs_ScrambleContactOrder()
 * @endcond
 */
 extern "C" void nlgs_ScrambleContactOrder(void);

/**
 * @fn void nlgs_QuickScrambleContactOrder(void)
 * @brief Random renumbering of the contact list
 *
 * @cond PYDOC
 * python usage : nlgs_QuickScrambleContactOrder()
 * @endcond
 */
 extern "C" void nlgs_QuickScrambleContactOrder(void);

/**
 * @fn void nlgs_SetWithQuickScramble(void)
 * @brief active quick scramble in macro function ExSolver
 *
 * @cond PYDOC
 * python usage : nlgs_SetWithQuickScramble()
 * @endcond
 */
 extern "C" void nlgs_SetWithQuickScramble(void);

/**
 * @fn void nlgs_ReverseContactOrder(void)
 * @brief Reverse the numbering of the contact list
 *
 * @cond PYDOC
 * python usage : nlgs_ReverseContactOrder()
 * @endcond
 */
 extern "C" void nlgs_ReverseContactOrder(void);

/**
 * @fn void nlgs_BimodalContactOrder(void)
 * @brief Renumbering of the contact list using the definition
 * of weak and strong network in granular assemblies
 *
 * @cond PYDOC
 * python usage : nlgs_BimodalContactOrder()
 * @endcond
 */
 extern "C" void nlgs_BimodalContactOrder(void);

/**
 * @fn void nlgs_ScaleRloc(void)
 * @brief Scale all local contact forces of a factor equal to * 0.9 < f < 1.1
 *
 * @cond PYDOC
 * python usage : nlgs_ScaleRloc()
 * @endcond
 */
 extern "C" void nlgs_ScaleRloc(void);

/**
 * @fn void nlgs_ComputeRnod(void)
 * @brief mapping from local contact forces to global ones
 *
 * @cond PYDOC
 * python usage : nlgs_ComputeRnod()
 * @endcond
 */
 extern "C" void nlgs_ComputeRnod(void);

/**
 * @fn void nlgs_DisplayRlocNSum(void)
 * @brief Display the sum of normal contact forces
 *
 * @cond PYDOC
 * python usage : nlgs_DisplayRlocNSum()
 * @endcond
 */
 extern "C" void nlgs_DisplayRlocNSum(void);

/**
 * @fn void nlgs_ExSolver(char * cvalue1_c, char * cvalue2_c, double rvalue1, double rvalue2, int ivalue1, int ivalue2)
 * @brief Solve fully the local contact problem
 *
 * @cond PYDOC
 * python usage : nlgs_ExSolver(storage, checktype, tol, relax, nb_iter_check, nb_block_iter)
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
 extern "C" void nlgs_ExSolver(char * cvalue1_c, char * cvalue2_c, double rvalue1, double rvalue2, int ivalue1, int ivalue2);

/**
 * @fn void nlgs_UpdateCohesiveBehav(void)
 * @brief update internal parameters of contact laws for each contact
 *
 * @cond PYDOC
 * python usage : nlgs_UpdateCohesiveBehav(void)
 * @endcond
 */
 extern "C" void nlgs_UpdateCohesiveBehav(void);

/**
 * @fn void nlgs_UpdateFrictionalBehav(void)
 * @brief update internal parameters of contact laws for each contact
 *
 * @cond PYDOC
 * python usage : nlgs_UpdateFrictionalBehav(void)
 * @endcond
 */
 extern "C" void nlgs_UpdateFrictionalBehav(void);

/**
  * @fn void nlgs_GetAllThis(double** matrix_out, int* dim1, int* dim2)
  * @brief Get all interactions in "this" array
  *
  * Each interaction has (in this order): coor, tuc, nuc, rlt, rln, vlt, vln
  * @cond PYDOC
  * usage : interactions = nlgs_GetAllThis()
  * @return interactions (double 2D-array) : the interactions
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] matrix_out (double**) : array of interaction data
  * @param[out]    dim1 (int*)           : number of interaction
  * @param[out]    dim2 (int*)           : number of real data per interaction
  * @endcond
  */
  extern "C" void nlgs_GetAllThis(double** matrix_out, int* dim1, int* dim2);

  /**
  * @fn void nlgs_UseJacobiSolver(bool jacobi)
  * @brief Use a Jacobi solver instead of Gauss Seidel solver
  *
  * @cond PYDOC
  * usage : nlgs_UseJacobiSolver(True) or nlgs_UseJacobiSolver(False)
  * @endcond
  *
  * @cond CDOC
  * @param[in] jacobi (boolean) : set to True to use a Jacobi solver
  * @endcond
  */
  extern "C" void nlgs_UseJacobiSolver(bool jacobi);

 /**
 * @fn void nlgs_UseRegularization(double rvalue1, double rvalue2)
 * @brief use some regularization heuristics on interaction laws
 *
 * @cond PYDOC
 * python usage : nlgs_UseRegularization(krn, krt)
 * @param[in] krn (double)  : normal penality (default 1e14)
 * @param[in] krt (double)  : tangential penality (default 1e14)
 * @endcond
 */
 extern "C" void nlgs_UseRegularization(double rvalue1=1e14, double rvalue2=1e14);

 /**
 * @fn void nlgs_SetTemporaryVariable(int ivalue1, int ivalue2, double rvalue1)
 * @brief set temporary variables used in nlgs ; ivalue2 == 3 gives access to post crack pressure
 *
 * @cond PYDOC
 * python usage : nlgs_SetTemporaryVariable(icdan,id,val)
 * @param[in] icdan (int)  : interaction rank
 * @param[in] id (int)     : value rank
 * @param[in] val (double) : value
 * @endcond
 */

 extern "C" void nlgs_SetTemporaryVariable(int ivalue1, int ivalue2, double rvalue1);

 /**
 * @fn double nlgs_GetTemporaryVariable(int ivalue1, int ivalue2)
 * @brief get temporary variables used in nlgs ; ivalue2 == 3 gives access to post crack pressure
 *
 * @cond PYDOC
 * python usage : val = nlgs_GetTemporaryVariable(icdan,id)
 * @param[in] icdan (int)  : interaction rank
 * @param[in] id (int)     : value rank
 * @param[in] val (double) : value
 * @endcond
 */
 extern "C" double nlgs_GetTemporaryVariable(int ivalue1, int ivalue2);

 /**
 * @fn void nlgs_IsInitialized(int is_init = 1)
 * @brief In case of restart say that nlgs is initialized or reset it
 *
 * @cond PYDOC
 * python usage : nlgs_IsInitialized(is_init=1)
 * @endcond
 */
 extern "C" void nlgs_IsInitialized(int is_init=1);

#endif /* wrap_nlgs */
