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

#ifndef wrap_SiconosNumerics_h
#define wrap_SiconosNumerics_h

/**
 * @fn void SiconosNumerics_SetParameters(char * checktype_c, double tol, int iter, double relax, int verbose, int output)
 * @brief define parameters for SiconosNumerics algorithm
 *
 * @cond PYDOC
 * python usage : SiconosNumerics_SetParameters(name, tolerance, itererror, itermax, relaxation)
 * @param[in] chekctype_c (char[5]) : solver name
 * @param[in] tol         (double)  : norm tolerance
 * @param[in] itererror   (int)     : iterations without error computation
 * @param[in] itermax     (int)     : max iteration
 * @param[in] relax       (double)  : relaxation factor
 * @param[in] verbose     (int)     : verbosity 0 off 1 on
 * @param[in] output      (int)     : solver ouput 0 off, 1 C file, 2 dat, 3 Flib
 * @endcond
 *
 *
 * @cond CDOC
 * @param[in] chekctype_c (char[5]) : type of convergence check
 * @param[in] tol         (double)  : norm tolerance
 * @param[in] intererror     (int)  : iteration without error computation
 * @param[in] intermax       (int)  : max iteration
 * @param[in] relax       (double)  : relaxation factor
 * @param[in] verbose     (int)     : verbosity 0 off 1 on
 * @param[in] output      (int)     : solver ouput 0 off, 1 C file, 2 dat, 3 Flib
 * @param[in] freq_output      (int): solver ouput frequency
 * @endcond
 */
extern "C" void SiconosNumerics_SetParameters(char * checktype_c, double tol, int iter_error, int iter_max, double RELAX, int verbose, int output, int freq_output);



/**
 * @fn void SiconosNumerics_ExSolver()
 * @brief Solve fully the local contact problem
 *
 * @cond PYDOC
 * python usage : SiconosNumerics_ExSolver()
 * @endcond
 *
 * @cond CDOC
 * @endcond
 */
 extern "C" void SiconosNumerics_ExSolver();


/**
 * @fn void SiconosNumerics_IsInitialized(void)
 * @brief In case of restart say that nlgs is initialized
 *
 * @cond PYDOC
 * python usage : SiconosNumerics_IsInitialized()
 * @endcond
 */
 extern "C" void SiconosNumerics_IsInitialized(void);


#endif /* wrap_SiconosNumerics_h */
