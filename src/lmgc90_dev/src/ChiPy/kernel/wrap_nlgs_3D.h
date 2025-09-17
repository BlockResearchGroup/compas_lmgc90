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

#ifndef wrap_nlgs_3D_h
#define wrap_nlgs_3D_h

//
// flat solver
//

/**
 * @fn void nlgs_3D_ExIter(int nb_iter)
 * @brief Executes nb_iter NLGS iterations
 *
 * @cond PYDOC
 * python usage : nlgs_3D_ExIter(nb_iter)
 * param[in] nb_iter (integer) : number of iterations to do
 * @endcond
 *
 * @cond CDOC
 * param[in] nb_iter (int) : number of iterations to do
 * @endcond
 */
 extern "C" void nlgs_3D_ExIter(int nb_iter);

/**
 * @fn void nlgs_3D_ExIterJacobi(int nb_iter)
 * @brief Executes nb_iter NLJacobi iterations
 *
 * @cond PYDOC
 * python usage : nlgs_3D_ExIterJacobi(nb_iter)
 * param[in] nb_iter (integer) : number of iterations to do
 * @endcond
 *
 * @cond CDOC
 * param[in] nb_iter (int) : number of iterations to do
 * @endcond
 */
 extern "C" void nlgs_3D_ExIterJacobi(int nb_iter);


/**
 * @fn int nlgs_3D_AfterIterCheck(void)
 * @brief Control NLGS convergence
 *
 * @cond PYDOC
 * python usage : convergence = nlgs_3D_AfterIterCheck()
 * @return convergence (integer) :
 * @endcond
 *
 * @cond CDOC
 * @return (int) convergence state
 * @endcond
 */
 extern "C" int nlgs_3D_AfterIterCheck(void);

/**
 * @fn int nlgs_3D_AfterIterCheckJacobi(void)
 * @brief Control NLGS convergence
 *
 * @cond PYDOC
 * python usage : convergence = nlgs_3D_AfterIterCheckJacobi()
 * @return convergence (integer) :
 * @endcond
 *
 * @cond CDOC
 * @return (int) convergence state
 * @endcond
 */
 extern "C" int nlgs_3D_AfterIterCheckJacobi(void);

/**
 * @fn void nlgs_3D_ScrambleContactOrder(void)
 * @brief Random renumbering of the contact list
 *
 * @cond PYDOC
 * python usage : nlgs_3D_ScrambleContactOrder()
 * @endcond
 */
 extern "C" void nlgs_3D_ScrambleContactOrder(void);

/**
 * @fn void nlgs_3D_QuickScrambleContactOrder(void)
 * @brief Random renumbering of the contact list
 *
 * @cond PYDOC
 * python usage : nlgs_3D_QuickScrambleContactOrder()
 * @endcond
 */
 extern "C" void nlgs_3D_QuickScrambleContactOrder(void);

/**
 * @fn void nlgs_3D_ReverseContactOrder(void)
 * @brief reverse the numbering of the contact list
 *
 * @cond PYDOC
 * python usage : nlgs_3D_ReverseContactOrder()
 * @endcond
 */
 extern "C" void nlgs_3D_ReverseContactOrder(void);

/**
 * @fn void nlgs_3D_DisplayAfterIterCheck(void)
 * @brief display NLGS convergence results
 *
 * @cond PYDOC
 * python usage : nlgs_3D_DisplayAfterIterCheck()
 * @endcond
 */
 extern "C" void nlgs_3D_DisplayAfterIterCheck(void);

/**
 * @fn void nlgs_3D_ScaleRloc(void)
 * @brief scale all local contact forces of a factor equal to 0.9 < f < 1.1
 *
 * @cond PYDOC
 * python usage : nlgs_3D_ScaleRloc()
 * @endcond
 */
 extern "C" void nlgs_3D_ScaleRloc(void);

/**
 * @fn void nlgs_3D_ComputeRnod(void)
 * @brief mapping from local contact forces to global ones
 *
 * @cond PYDOC
 * python usage : nlgs_3D_ComputeRnod()
 * @endcond
 */
 extern "C" void nlgs_3D_ComputeRnod(void);

/**
 * @fn void nlgs_3D_ExPost(void)
 * @brief run a jacobi iteration with the solution obtain with the NLGS algorithm
 *
 * @cond PYDOC
 * python usage : nlgs_3D_ExPost()
 * @endcond
 */
 extern "C" void nlgs_3D_ExPost(void);

/**
 * @fn void nlgs_3D_ExPostJacobi(void)
 * @brief run a jacobi iteration with the solution obtain with the NLGS algorithm
 *
 * @cond PYDOC
 * python usage : nlgs_3D_ExPostJacobi()
 * @endcond
 */
 extern "C" void nlgs_3D_ExPostJacobi(void);


/**
 * @fn void nlgs_3D_SetCheckType(char * checktype_c, double tol, double relax)
 * @brief define numerical convergence of the NLGS algorithm
 *
 * @cond PYDOC
 * python usage : nlgs_SetCheckType(check_type, tolerance, relaxation)
 * @param[in] chekctype_c (char[5]) : type of convergence check
 * @param[in] tol         (double)  : norm tolerance
 * @param[in] relax       (double)  : relaxation factor
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
 * @param[in] chekctype_c (char[5]) : type of convergence check
 * @param[in] tol         (double)  : norm tolerance
 * @param[in] relax       (double)  : relaxation factor
 * @endcond
 */
 extern "C" void nlgs_3D_SetCheckType(char * checktype_c, double tol, double RELAX);

/**
 * @fn void nlgs_3D_ExPrep(char * storage_c)
 * @brief Prepare matrix storage
 *
 * @cond PYDOC
 * python usage : nlgs_ExPrep(storage)
 * @param[in] storage_c(char[30]) : matrix storage
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
 * @param[in] storage_c (char[30]) : matrix storage
 * @endcond
 */
 extern "C" void nlgs_3D_ExPrep(char * storage_c);

/**
 * @fn void nlgs_3D_WriteNormCheck(void)
 * @brief write norm to file
 *
 * @cond PYDOC
 * python usage : nlgs_3D_WriteNormCheck()
 * @endcond
 */
 extern "C" void nlgs_3D_WriteNormCheck(void);


/**
 * @fn void nlgs_3D_DiagonalResolution(void)
 * @brief 
 *
 * @cond PYDOC
 * python usage : nlgs_3D_DiagonalResolution()
 * @endcond
 */
 extern "C" void nlgs_3D_DiagonalResolution(void);

//
//  macro solver
//

/**
 * @fn void nlgs_3D_SetWithQuickScramble(void)
 * @brief Activate quick scramble in macro function ExSolver
 *
 * @cond PYDOC
 * python usage : nlgs_3D_SetWithQuickScramble()
 * @endcond
 */
extern "C" void nlgs_3D_SetWithQuickScramble(void);

/**
 * @fn void nlgs_3D_SetWithReverseContactOrder(void)
 * @brief Activate reverse order in macro function ExSolver
 *
 * @cond PYDOC
 * python usage : nlgs_3D_SetWithReverseContactOrder()
 * @endcond
 */
extern "C" void nlgs_3D_SetWithReverseContactOrder(void);

 /**
  * @fn void nlgs_3D_UseJacobiSolver(bool jacobi)
  * @brief Use a Jacobi solver instead of Gauss Seidel solver
  *
  * @cond PYDOC
  * usage : nlgs_3D_UseJacobiSolver(True) or nlgs_UseJacobiSolver(False)
  * @endcond
  *
  * @cond CDOC
  * @param[in] jacobi (boolean) : set to True to use a Jacobi solver
  * @endcond
  */
  extern "C" void nlgs_3D_UseJacobiSolver(bool jacobi);

/**
 * @fn void nlgs_3D_ExSolver(char * Wstorage_c, char * checktype_c, double tol, double RELAX, int nb_iter_check, int nb_block_iter)
 * @brief Solve fully the local contact problem
 *
 * @cond PYDOC
 * python usage : nlgs_3D_ExSolver(storage, checktype, tol, relax, nb_iter_check, nb_block_iter)
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
 extern "C" void nlgs_3D_ExSolver(char * Wstorage_c, char * checktype_c, double tol, double RELAX,
				int nb_iter_check, int nb_block_iter);

//
// common options and function
//

/**
 * @fn void nlgs_3D_UpdateTactBehav(void)
 * @brief update internal parameters of contact laws for each contact
 *
 * @cond PYDOC
 * python usage : nlgs_3D_UpdateTactBehav()
 * @endcond
 */
 extern "C" void nlgs_3D_UpdateTactBehav(void);

/**
 * @fn void nlgs_3D_IsInitialized(int is_init = 1)
 * @brief In case of restart say that nlgs is initialized
 *
 * @cond PYDOC
 * python usage : nlgs_3D_IsInitialized(is_init=1)
 * @endcond
 */
 extern "C" void nlgs_3D_IsInitialized(int is_init=1);

/**
 * @fn void nlgs_3D_DisplayTacInfo(int itac)
 * @brief Display information concerning one contact
 *
 * @cond PYDOC
 * python usage : nlgs_3D_DsplayTacInfo(itac)
 * param[in] itac (integer) : contact rank
 * @endcond
 *
 * @cond CDOC
 * param[in] itac (int) : contact rank
 * @endcond
 */
 extern "C" void nlgs_3D_DisplayTacInfo(int itac);
 
//
///**
// * @fn void nlgs_3D_InitCohesiveBehav(void)
// * @brief update internal parameters of contact laws for each contact
// *
// * @cond PYDOC
// * python usage : nlgs_3D_InitCohesiveBehav(void)
// * @endcond
// */
// extern "C" void nlgs_3D_InitCohesiveBehav(void);

/**
 * @fn void nlgs_3D_UseRegularization(double rvalue1, double rvalue2)
 * @brief use some regularization heuristics on interaction laws
 *
 * @cond PYDOC
 * python usage : nlgs_3D_UseRegularization(krn, krt)
 * @param[in] krn (double)  : normal penality (default 1e14)
 * @param[in] krt (double)  : tangential penality (default 1e14)
 *
 * @endcond
 */
 extern "C" void nlgs_3D_UseRegularization(double rvalue1=1e14, double rvalue2=1e14);

/**
 * @fn void nlgs_3D_CutOpenCZM(double rvalue1)
 * @brief If some czm contact have a gap greater than the given they are considered as broken ; works only with EXPO_CZM or IQS_EXPO_CZM
 *
 * @cond PYDOC
 * python usage : nlgs_3D_CutOpenCZM(tol)
 * @param[in] tol (double)  : threshold on positive distance (default 1e-6)
 *
 * @endcond
 */
 extern "C" void nlgs_3D_CutOpenCZM(double rvalue1=1e-06);

/**
 * @fn void nlgs_3D_ManageInterpenetratedCZM(void)
 * @brief Apply a g0 strategy if gap is negative and if gap is positive (without using nlgs_3D_CutOpenCZM) ; works only with EXPO_CZM or IQS_EXPO_CZM
 *
 * @cond PYDOC
 * python usage : nlgs_3D_ManageInterpenetratedCZM()
 *
 * @endcond
 */
 extern "C" void nlgs_3D_ManageInterpenetratedCZM(void);

#endif /* wrap_nlgs_3D_h */
