/*===========================================================================
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

#ifndef wrap_PARAM_h
#define wrap_PARAM_h

 /**
  * @fn int parameters_getPhysicTypeId(char* string_in)
  * @brief Get the id a body type from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getPhysicTypeId(bodyName)
  * @param[in] bodyName (string): body type name
  * @return    i_param (int)    : body type parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : body type name
  * @return    i_param   (int)    : body type parameter
  * @endcond
  */
  extern "C" int parameters_getPhysicTypeId(char* cvalue1);

 /**
  * @fn void parameters_getPhysicTypeNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of body types
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getPhysicTypeName()
  * @return bodyName (string array) : body type names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : body type name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getPhysicTypeNames(char** string_vector, int* vector_size, int* string_size);

 /**
  * @fn int parameters_getBodyModelId(char* string_in)
  * @brief Get the id a body model from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getBodyModelId(bodyName)
  * @param[in] bodyName (string): body model name
  * @return    i_param (int)    : body model parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : body model name
  * @return    i_param   (int)    : body model parameter
  * @endcond
  */
  extern "C" int parameters_getBodyModelId(char* cvalue1);

 /**
  * @fn void parameters_getBodyModelNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of body types
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getBodyModelName()
  * @return bodyName (string array) : body model names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : body model name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getBodyModelNames(char** string_vector, int* vector_size, int* string_size);

 /**
  * @fn int parameters_getContactorId(char* string_in)
  * @brief Get the id a contactor from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getContactorId(bodyName)
  * @param[in] bodyName (string): contactor name
  * @return    i_param (int)    : contactor parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : contactor name
  * @return    i_param   (int)    : contactor parameter
  * @endcond
  */
  extern "C" int parameters_getContactorId(char* cvalue1);

 /**
  * @fn void parameters_getContactorNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of contactors
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getContactorName()
  * @return bodyName (string array) : contactor names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : contactor name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getContactorNames(char** string_vector, int* vector_size, int* string_size);

 /**
  * @fn int parameters_getInteractionId(char* string_in)
  * @brief Get the id a interaction from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getInteractionId(bodyName)
  * @param[in] bodyName (string): interaction name
  * @return    i_param (int)    : interaction parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : interaction name
  * @return    i_param   (int)    : interaction parameter
  * @endcond
  */
  extern "C" int parameters_getInteractionId(char* cvalue1);

 /**
  * @fn void parameters_getInteractionNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of interactions
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getInteractionName()
  * @return bodyName (string array) : interaction names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : interaction name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getInteractionNames(char** string_vector, int* vector_size, int* string_size);

 /**
  * @fn int parameters_getMatrixStorageId(char* string_in)
  * @brief Get the id a matrix storage from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getMatrixStorageId(bodyName)
  * @param[in] bodyName (string): matrix storage name
  * @return    i_param (int)    : matrix storage parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : matrix storage name
  * @return    i_param   (int)    : matrix storage parameter
  * @endcond
  */
  extern "C" int parameters_getMatrixStorageId(char* cvalue1);

 /**
  * @fn void parameters_getMatrixStorageNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of matrix storages
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getMatrixStorageName()
  * @return bodyName (string array) : matrix storage names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : matrix storage name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getMatrixStorageNames(char** string_vector, int* vector_size, int* string_size);

 /**
  * @fn int parameters_getMatrixShapeId(char* string_in)
  * @brief Get the id a matrix shape from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getMatrixShapeId(bodyName)
  * @param[in] bodyName (string): matrix shape name
  * @return    i_param (int)    : matrix shape parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : matrix shape name
  * @return    i_param   (int)    : matrix shape parameter
  * @endcond
  */
  extern "C" int parameters_getMatrixShapeId(char* cvalue1);

 /**
  * @fn void parameters_getMatrixShapeNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of matrix shapes
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getMatrixShapeName()
  * @return bodyName (string array) : matrix shape names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : matrix shape name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getMatrixShapeNames(char** string_vector, int* vector_size, int* string_size);

 /**
  * @fn int parameters_getGeneralizedCoordinatesId(char* string_in)
  * @brief Get the id a generalized coordinates from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getGeneralizedCoordinatesId(bodyName)
  * @param[in] bodyName (string): generalized coordinates name
  * @return    i_param (int)    : generalized coordinates parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : generalized coordinates name
  * @return    i_param   (int)    : generalized coordinates parameter
  * @endcond
  */
  extern "C" int parameters_getGeneralizedCoordinatesId(char* cvalue1);

 /**
  * @fn void parameters_getGeneralizedCoordinatesNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of generalized coordinatess
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getGeneralizedCoordinatesName()
  * @return bodyName (string array) : generalized coordinates names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : generalized coordinates name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getGeneralizedCoordinatesNames(char** string_vector, int* vector_size, int* string_size);

 /**
  * @fn int parameters_getSurfaceEnergyStatusId(char* string_in)
  * @brief Get the id a surface energy status from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getSurfaceEnergyStatusId(bodyName)
  * @param[in] bodyName (string): surface energy status name
  * @return    i_param (int)    : surface energy status parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : surface energy status name
  * @return    i_param   (int)    : surface energy status parameter
  * @endcond
  */
  extern "C" int parameters_getSurfaceEnergyStatusId(char* cvalue1);

 /**
  * @fn void parameters_getSurfaceEnergyStatusNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of surface energy statuss
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getSurfaceEnergyStatusName()
  * @return bodyName (string array) : surface energy status names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : surface energy status name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getSurfaceEnergyStatusNames(char** string_vector, int* vector_size, int* string_size);

 /**
  * @fn int parameters_getInterLawId(char* string_in)
  * @brief Get the id a inter law from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getInterLawId(bodyName)
  * @param[in] bodyName (string): inter law name
  * @return    i_param (int)    : inter law parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : inter law name
  * @return    i_param   (int)    : inter law parameter
  * @endcond
  */
  extern "C" int parameters_getInterLawId(char* cvalue1);

 /**
  * @fn void parameters_getInterLawNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of inter laws
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getInterLawName()
  * @return bodyName (string array) : inter law names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : inter law name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getInterLawNames(char** string_vector, int* vector_size, int* string_size);

 /**
  * @fn int parameters_getIntegratorId(char* string_in)
  * @brief Get the id a integrator from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getIntegratorId(bodyName)
  * @param[in] bodyName (string): integrator name
  * @return    i_param (int)    : integrator parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : integrator name
  * @return    i_param   (int)    : integrator parameter
  * @endcond
  */
  extern "C" int parameters_getIntegratorId(char* cvalue1);

 /**
  * @fn void parameters_getIntegratorNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of integrators
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getIntegratorName()
  * @return bodyName (string array) : integrator names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : integrator name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getIntegratorNames(char** string_vector, int* vector_size, int* string_size);

 /**
  * @fn int parameters_getNodeId(char* string_in)
  * @brief Get the id a node from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getNodeId(bodyName)
  * @param[in] bodyName (string): node name
  * @return    i_param (int)    : node parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : node name
  * @return    i_param   (int)    : node parameter
  * @endcond
  */
  extern "C" int parameters_getNodeId(char* cvalue1);

 /**
  * @fn void parameters_getNodeNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of nodes
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getNodeName()
  * @return bodyName (string array) : node names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : node name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getNodeNames(char** string_vector, int* vector_size, int* string_size);

 /**
  * @fn int parameters_getDimeModeId(char* string_in)
  * @brief Get the id a dime mode from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getDimeModeId(bodyName)
  * @param[in] bodyName (string): dime mode name
  * @return    i_param (int)    : dime mode parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : dime mode name
  * @return    i_param   (int)    : dime mode parameter
  * @endcond
  */
  extern "C" int parameters_getDimeModeId(char* cvalue1);

 /**
  * @fn void parameters_getDimeModeNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of dime modes
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getDimeModeName()
  * @return bodyName (string array) : dime mode names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : dime mode name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getDimeModeNames(char** string_vector, int* vector_size, int* string_size);

 /**
  * @fn int parameters_getBodyVectorId(char* string_in)
  * @brief Get the id a body vector from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getBodyVectorId(bodyName)
  * @param[in] bodyName (string): body vector name
  * @return    i_param (int)    : body vector parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : body vector name
  * @return    i_param   (int)    : body vector parameter
  * @endcond
  */
  extern "C" int parameters_getBodyVectorId(char* cvalue1);

 /**
  * @fn void parameters_getBodyVectorNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of body vectors
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getBodyVectorName()
  * @return bodyName (string array) : body vector names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : body vector name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getBodyVectorNames(char** string_vector, int* vector_size, int* string_size);

 /**
  * @fn int parameters_getContactStatusId(char* string_in)
  * @brief Get the id a contact status from its name
  *
  * @cond PYDOC
  * usage i_param = parameters_getContactStatusId(bodyName)
  * @param[in] bodyName (string): contact status name
  * @return    i_param (int)    : contact status parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] string_in (char *) : contact status name
  * @return    i_param   (int)    : contact status parameter
  * @endcond
  */
  extern "C" int parameters_getContactStatusId(char* cvalue1);

 /**
  * @fn void parameters_getContactStatusNames(char** string_vector, int* vector_size, int* string_size)
  * @brief Get the list of contact statuss
  *
  * @cond PYDOC
  * usage bodyNames = parameters_getContactStatusName()
  * @return bodyName (string array) : contact status names
  * @endcond
  *
  * @cond CDOC
  * @param[out] string_vector (char **) : contact status name
  * @param[out] vector_size (int * ) : size of string_vector
  * @param[out] string_size (int * ) : size of each string
  * @endcond
  */
  extern "C" void parameters_getContactStatusNames(char** string_vector, int* vector_size, int* string_size);


 /**
  * @fn void parameters_checkAll(void)
  * @brief Check the consistency of all parameters id and name
  *
  * @cond PYDOC
  * usage parameters_checkAll()
  * @endcond
  */
  extern "C" void parameters_checkAll(void);

#endif /* wrap_PARAM */

