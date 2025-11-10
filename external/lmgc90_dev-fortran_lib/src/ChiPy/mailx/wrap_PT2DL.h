/*========================================================================== *
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

#ifndef wrap_PT2DL_h
#define wrap_PT2DL_h

 /**
  * @fn void PT2DL_LoadTactors(void)
  * @brief Initialize existing_entities variable for PT2DL contactors
  *
  * @cond PYDOC
  * python usage : PT2DL_LoadTactors()
  * @endcond
  */
   extern "C" void PT2DL_LoadTactors(void);

 /**
  * @fn void PT2DL_PushPreconNodes(void)
  * @brief
  *
  * @cond PYDOC
  * python usage : PT2DL_PushPreconNodes()
  * @endcond
  */
  extern "C" void PT2DL_PushPreconNodes(void);

 /**
  * @fn int PT2DL_GetNbPT2DL(void)
  * @brief Get the number of PT2DL
  *
  * @cond PYDOC
  * usage : nb_PT2DL = PT2DL_GetNbPT2DL()
  * @param nb_PT2DL (integer) : number of PT2DL in container
  * @endcond
  *
  * @cond CDOC
  * @return nb_PT2DL (int) : number of PT2DL in container
  * @endcond
  */
  extern "C" int PT2DL_GetNbPT2DL(void);

/**
  * @fn int PT2DL_GetNbPT2TL(int)
  * @brief Get the number of PT2TL of a body
  *
  * @cond PYDOC
  * usage : nb_PT2DL = PT2DL_GetNbPT2TL(ibdyty)
  * @param nb_PT2TL (integer) : number of PT2TL in container
  * @endcond
  *
  * @cond CDOC
  * @return nb_PT2TL (int) : number of PT2TL in container
  * @endcond
  */
  extern "C" int PT2DL_GetNbPT2TL(int);

 /* /\** */
 /*  * @fn void PT2DL_SetHConv(int itacty, double value) */
 /*  * @brief Set H for a PT2TL */
 /*  * */
 /*  * @cond PYDOC */
 /*  * python usage : H = PT2DL_SetHConv(itacty) */
 /*  * @param[in] itacty (integer) : rank of PT2DL */
 /*  * @param[in] hconv  (real)    : H */
 /*  * @endcond */
 /*  * */
 /*  * @cond CDOC */
 /*  * @param[in] itacty (int)   : rank of PT2DL */
 /*  * @return hconv (double) : H */
 /*  * @endcond */
 /*  *\/ */
 /*  extern "C" void PT2DL_SetHConv(int itacty, double value); */

 /* /\** */
 /*  * @fn void PT2DL_SetTConv(int itacty, double value) */
 /*  * @brief Set T conv for a PT2TL */
 /*  * */
 /*  * @cond PYDOC */
 /*  * python usage : T = PT2DL_SetTConv(itacty) */
 /*  * @param[in] itacty (integer) : rank of PT2DL */
 /*  * @return Tconv  (real) : T */
 /*  * @endcond */
 /*  * */
 /*  * @cond CDOC */
 /*  * @param[in] itacty (int)   : rank of PT2DL */
 /*  * @param[in] hconv (double) : T */
 /*  * @endcond */
 /*  *\/ */
 /*  extern "C" void PT2TL_SetTconv(int itacty, double value); */

 /**
  * @fn void PT2DL_ComputeConvectiveFlux(void)
  * @brief
  *
  * @cond PYDOC
  * python usage : PT2DL_ComputeConvectiveFlux()
  * @endcond
  */
  extern "C" void PT2DL_ComputeConvectiveFlux(void);

 /**
  * @fn void PT2DL_AssembThermKT(void)
  * @brief
  *
  * @cond PYDOC
  * python usage : PT2DL_AssembThermKT()
  * @endcond
  */
  extern "C" void PT2DL_AssembThermKT(void);

 /**
  * @fn void PT2DL_AssembThermRHS(void)
  * @brief
  *
  * @cond PYDOC
  * python usage : PT2DL_AssembThermRHS()
  * @endcond
  */
  extern "C" void PT2DL_AssembThermRHS(void);

 /**
  * @fn int PT2DL_GetBody(int itacty)
  * @brief return corresponding body
  *
  * @cond PYDOC
  * python usage : ibdy = PT2DL_GetBody(itacty)
  * @endcond
  */
  extern "C" int PT2DL_GetBody(int);

/**
  * @fn void PT2DL_CleanMemory(void)
  * @brief Free all memory allocated within PT2DL module
  *
  * @cond PYDOC
  * python usage : PT2DL_CleanMemory()
  * @endcond
  */
  extern "C" void PT2DL_CleanMemory(void);

#endif /* wrap_PT2DL_h */
