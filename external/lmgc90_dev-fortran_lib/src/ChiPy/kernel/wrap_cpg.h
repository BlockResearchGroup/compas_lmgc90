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

#ifndef wrap_cpg_h
#define wrap_cpg_h

/**
 * @fn void cpg_ExIter(void)
 * @brief Execute one CPG iteration over the contact loop
 *
 * @cond PYDOC
 * python usage cpg_ExIter()
 * @endcond
 */
 extern "C" void cpg_ExIter(void);

/**
 * @fn void cpg_AfterIterCheck(void)
 * @brief Control CPG convergence
 *
 * @cond PYDOC
 * python usage cpg_AfterIterCheck()
 * @endcond
 */
 extern "C" void cpg_AfterIterCheck(void);

/**
 * @fn void cpg_ExPost(void)
 * @brief Transfer local solution
 *
 * @cond PYDOC
 * python usage cpg_ExPost()
 * @endcond
 */
 extern "C" void cpg_ExPost(void);

/**
 * @fn void cpg_ExPrep(void)
 * @brief prepare the matrix and the RHS of the contact problem
 *
 * @cond PYDOC
 * python usage cpg_ExPrep()
 * @endcond
 */
 extern "C" void cpg_ExPrep(void);

/**
 * @fn void cpg_ScaleRloc(void)
 * @brief scale all local contact forces of a factor equal to 0.9 < f < 1.1
 *
 * @cond PYDOC
 * python usage cpg_ScaleRloc()
 * @endcond
 */
 extern "C" void cpg_ScaleRloc(void);

/**
 * @fn void cpg_SetDiagonalPrecond(void)
 * @brief active diagonal preconditioner
 *
 * @cond PYDOC
 * python usage cpg_SetDiagonalPrecond()
 * @endcond
 */
 extern "C" void cpg_SetDiagonalPrecond(void);

/**
 * @fn void cpg_SetFrictionless(void)
 * @brief active frictionless solver
 *
 * @cond PYDOC
 * python usage cpg_SetFrictionless()
 * @endcond
 */
 extern "C" void cpg_SetFrictionless(void);

/**
 * @fn void cpg_SetNoConjugaison(void)
 * @brief desactive conjugaison
 *
 * @cond PYDOC
 * python usage cpg_SetNoConjugaison()
 * @endcond
 */
 extern "C" void cpg_SetNoConjugaison(void);

/**
 * @fn void cpg_SetCheckType(char * checktype_c, double tol)
 * @brief define numerical convergence of the NLGS algorithm
 *
 * @cond PYDOC
 * python usage cpg_SetCheckType(checktype, tol)
 * @param[in] chekctype (char[5]) : type of convergence check
 * @param[in] tol (double)        : norm tolerance
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
 * @param[in] tol (double)          : norm tolerance
 * @endcond
 */
 extern "C" void cpg_SetCheckType(char * checktype_c, double tol);

/**
 * @fn void cpg_NormCheck(void)
 * @brief Active one step norm evolution
 *
 * @cond PYDOC
 * python usage : cpg_norm_check()
 * @endcond
 */
 extern "C" void cpg_NormCheck(void);

/**
 * @fn void cpg_ExSolver(char * checktype_c, double tol, int nb_iter_check, int nb_block_iter)
 * @brief Solve fully the local contact problem
 *
 * @cond PYDOC
 * python usage : cpg_ExSolver(checktype, tol, nb_iter_check, nb_block_iter)
 * @param[in] checktype (char[5]) c   : convergentce test keyword
 * @param[in] tol (double)            : tolerance value
 * @param[in] nb_iter_check (integer) : number of iteration between convergence test
 * @param[in] nb_block_iter (integer) : number of block iterations
 * @endcond
 *
  * @cond CDOC
 * @param[in] checktype_c (char[5]) : convergentce test keyword
 * @param[in] tol (double)          : tolerance value
 * @param[in] nb_iter_check (int)   : number of iteration between convergence test
 * @param[in] nb_block_iter (int)   : number of block iterations
 * @endcond
 */
 extern "C" void cpg_ExSolver(char * checktype_c, double tol, int nb_iter_check, int nb_block_iter);

#endif /* wrap_cpg_h */
