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

#ifndef wrap_cpg_3D_h
#define wrap_cpg_3D_h

/**
 * @fn void cpg_3D_ExIter(void)
 * @brief Execute one CPG iteration over the contact loop
 *
 * @cond PYDOC
 * python usage cpg_3D_ExIter()
 * @endcond
 */
 extern "C" void cpg_3D_ExIter(void);

/**
 * @fn void cpg_3D_AfterIterCheck(void)
 * @brief Control CPG convergence
 *
 * @cond PYDOC
 * python usage cpg_3D_AfterIterCheck()
 * @endcond
 */
 extern "C" void cpg_3D_AfterIterCheck(void);

/**
 * @fn void cpg_3D_ExPost(void)
 * @brief Transfer local solution
 *
 * @cond PYDOC
 * python usage cpg_3D_ExPost()
 * @endcond
 */
 extern "C" void cpg_3D_ExPost(void);

/**
 * @fn void cpg_3D_ExPrep(void)
 * @brief prepare the matrix and the RHS of the contact problem
 *
 * @cond PYDOC
 * python usage cpg_3D_ExPrep()
 * @endcond
 */
 extern "C" void cpg_3D_ExPrep(void);

/**
 * @fn void cpg_3D_ScaleRloc(void)
 * @brief scale all local contact forces of a factor equal to 0.9 < f < 1.1
 *
 * @cond PYDOC
 * python usage cpg_3D_ScaleRloc()
 * @endcond
 */
 extern "C" void cpg_3D_ScaleRloc(void);

/**
 * @fn void cpg_3D_SetDiagonalPrecond(void)
 * @brief active diagonal preconditioner
 *
 * @cond PYDOC
 * python usage cpg_3D_SetDiagonalPrecond()
 * @endcond
 */
 extern "C" void cpg_3D_SetDiagonalPrecond(void);

/**
 * @fn void cpg_3D_SetFrictionless(void)
 * @brief active frictionless solver
 *
 * @cond PYDOC
 * python usage cpg_3D_SetFrictionless()
 * @endcond
 */
 extern "C" void cpg_3D_SetFrictionless(void);

/**
 * @fn void cpg_3D_BimodalContactOrder(void)
 * @brief active bimodal list
 *
 * @cond PYDOC
 * python usage : cpg_3D_BimodalContactOrder()
 * @endcond
 */
 extern "C" void cpg_3D_BimodalContactOrder(void);

/**
 * @fn void cpg_3D_SetCheckType(char * checktype_c, double tol, int idproj)
 * @brief define numerical convergence of the NLGS algorithm
 *
 * @cond PYDOC
 * python usage cpg_3D_SetCheckType(checktype, tol, idproj)
 * @param[in] chekctype (char[5])   : type of convergence check
 * @param[in] tol         (double)  : norm tolerance
 * @param[in] idproj      (integer) : 
 * @endcond
 *
 * \n convergence check keywords:\n 
 * Quad  : quadratic norm (faulty contacts are redeemed by accurate
 *         contacts; laxist norm)\n 
 * Maxm  : maximum norm (faulty contacts must comply; severe norm)\n 
 * where Quad,Maxm,QM/16 are keywords for the check test, and the 
 * following real number is the tolerance value.\n 
 * The identifiant projection parameter corrsponds to :\n 
 *  PYRAMIDAL APPROXIMATION (1) \n 
 *   Efficient but no more isotropic friction\n 
 *  NORMAL PROJECTION (2)\n 
 *   The basic projection but not really efficient\n 
 *  HYBRID CORRECTION (3)\n 
 *   Efficient for sphere but not really sense for other bodies.\n 
 *
 * @cond CDOC
 * @param[in] chekctype_c (char[5]) : type of convergence check
 * @param[in] tol (double)          : norm tolerance
 * @param[in] idproj (int)          : 
 * @endcond
 */
 extern "C" void cpg_3D_SetCheckType(char * checktype_c, double tol, int idproj);

/**
 * @fn void cpg_3D_NormCheck(void)
 * @brief Active one step norm evolution
 *
 * @cond PYDOC
 * python usage : cpg_3D_norm_check()
 * @endcond
 */
 extern "C" void cpg_3D_NormCheck(void);

/**
 * @fn void cpg_3D_ExSolver(char * checktype_c, double tol, int idproj, int nb_iter_check, int nb_block_iter)
 * @brief Solve fully the local contact problem
 *
 * @cond PYDOC
 * python usage : cpg_3D_ExSolver(checktype, tol, idpoj, nb_iter_check, nb_block_iter)
 * @param[in] checktype (char[5])     : convergentce test keyword
 * @param[in] tol         (double)    : tolerance value
 * @param[in] idproj      (integer)   : 
 * @param[in] nb_iter_check (integer) : number of iteration between convergence test
 * @param[in] nb_block_iter (integer) : number of block iterations
 * @endcond
 *
 * @cond CDOC
 * @param[in] checktype_c (char[5]) : convergentce test keyword
 * @param[in] tol (double)          : tolerance value
 * @param[in] idproj (int)          : 
 * @param[in] nb_iter_check (int)   : number of iteration between convergence test
 * @param[in] nb_block_iter (int)   : number of block iterations
 * @endcond
 */
 extern "C" void cpg_3D_ExSolver(char * checktype_c, double tol, int idproj, int nb_iter_check, int nb_block_iter);

#endif /* wrap_cpg_3D_h */
