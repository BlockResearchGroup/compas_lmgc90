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
 ==========================================================================*/

#ifndef wrap_models_h
#define wrap_models_h

/**
 * @fn void models_ReadModels(void)
 * @brief read models from DATBOX/MODELS.DAT
 *
 * @cond PYDOC
 * python usage : models_ReadModels()
 * @endcond
 */
extern "C" void models_ReadModels(void);

/**
 * @fn void models_WriteModels(void)
 * @brief write models to OUTBOX/MODELS.OUT
 *
 * @cond PYDOC
 * python usage : models_WriteModels()
 * @endcond
 */
extern "C" void models_WriteModels(void);

/**
 * @fn void models_InitModels(void)
 * @brief initialize models
 *
 * @cond PYDOC
 * python usage : models_InitModels()
 * @endcond
 */
extern "C" void models_InitModels(void);

/**
 * @fn void models_InitProperties(void)
 * @brief initialize properties
 *
 * In face re-initialize properties (since it is done in InitModels).
 * Necessary if a Store has been done and it is wanted again to LoadModel
 *
 * @cond PYDOC
 * python usage : models_InitProperties()
 * @endcond
 */
extern "C" void models_InitProperties(void);

/**
 * @fn void models_StoreProperties(void)
 * @brief create properties (couple of model and models)
 *
 * @cond PYDOC
 * python usage : models_StoreProperties()
 * @endcond
 */
extern "C" void models_StoreProperties(void);

/**
 * @fn void models_CleanMemory(void)
 * @brief Free all memory allocated within models module
 *
 * @cond PYDOC
 * python usage : models_CleanMemory()
 * @endcond
 */
extern "C" void models_CleanMemory(void);

#endif /* wrap_models_h */
