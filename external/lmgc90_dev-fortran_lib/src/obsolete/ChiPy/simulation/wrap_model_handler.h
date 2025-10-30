#ifndef _wrap_model_handler_h
#define _wrap_model_handler_h

 /**
  * @fn int model_handler_getNb(void)
  * @brief Get the number of model_handles
  *
  * @cond PYDOC
  * usage : nb = model_handler_getNb()
  * @return nb (integer) : the number of model_handles in the database
  * @endcond
  *
  * @cond CDOC
  * @return (int) : the number of model_handles in the database
  * @endcond
  */
  extern "C" int model_handler_getNb(void);

 /**
  * @fn void model_handler_getModelType(int id, char** string_out, int* string_size)
  * @brief Get the model type of a model_handle
  *
  * @cond PYDOC
  * usage : model = model_handler_getModelType(id)
  * @param[in] id (integer) : the id of model_handle in the database
  * @return model (string)  : the model type of the model_handle
  * @endcond
  *
  * @cond CDOC
  * @param[in] id (int) : the id of the model_handle in the database
  * @param[out] string_out (char**) : pointer on string of model type
  * @param[out] string_size (int*)  : size of string pointed by string_out
  * @endcond
  */
  extern "C" void model_handler_getModelType(int id, char** string_out, int* string_size);

 /**
  * @fn void model_handler_initialize(void)
  * @brief Initialize the model_handles
  *
  * For each model, allocate and fill mapping arrays ccdof, ccfield,
  * ppsets
  *
  * @cond PYDOC
  * usage : model_handler_initialize()
  * @endcond
  */
  extern "C" void model_handler_initialize(void);

 /**
  * @fn void model_handler_changeSystemStorage(int id, char* storage)
  * @brief Change the type of storage of the system of equation associated with a model handle
  *
  * @cond PYDOC
  * usage : model_handler_changeSystemStorage(id, storage)
  * @param[in] id (integer) : the id of model_handle in the database
  * @param[in] storage (string)  : the type of storage to use
  * @endcond
  *
  * storage possible values are "std_diag", "std_band", "sym_full", "sym_band" or "sym_sky_"
  *
  * @cond CDOC
  * @param[in] id (int) : the id of the model_handle in the database
  * @param[in] storage (char*) : the type of storage to use
  * @endcond
  */
  extern "C" void model_handler_changeSystemStorage(int id, char* storage);

 /**
  * @fn void model_handler_computeElementaryMasses4All(void)
  * @brief Compute the elementary masses of every mechanical models
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryMasses4All()
  * @endcond
  */
  extern "C" void model_handler_computeElementaryMasses4All(void);

 /**
  * @fn void model_handler_computeElementaryMasses4One(int id)
  * @brief Compute the elementary masses of one mechanical model
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryMasses4One(int id)
  * @param[in] id (integer) : index of the model_handle to compute elementary masses
  * @endcond
  *
  * @cond CDOC
  * @param[in] id (int) : index of the model_handle to compute elementary masses
  * @endcond
  */
  extern "C" void model_handler_computeElementaryMasses4One(int id);

 /**
  * @fn void model_handler_computeElementaryMasses4Some(int * ivector_in, int ilength_in)
  * @brief Compute the elementary masses of a  list of model_handles
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryMasses4Some(i_list)
  * @param[in] i_list (list of integer) : list of indices of model_handles to compute elementary masses
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of model_handle
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void model_handler_computeElementaryMasses4Some(int * ivector_in, int ilength_in);

 /**
  * @fn void model_handler_computeElementaryCapacities4All(void)
  * @brief Compute the elementary capacities of every thermal models
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryCapacities4All()
  * @endcond
  */
  extern "C" void model_handler_computeElementaryCapacities4All(void);

 /**
  * @fn void model_handler_computeElementaryCapacities4One(int id)
  * @brief Compute the elementary capacities of one thermal model
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryCapacities4One(int id)
  * @param[in] id (integer) : index of the model_handle to compute elementary capacities
  * @endcond
  *
  * @cond CDOC
  * @param[in] id (int) : index of the model_handle to compute elementary capacities
  * @endcond
  */
  extern "C" void model_handler_computeElementaryCapacities4One(int id);

 /**
  * @fn void model_handler_computeElementaryCapacities4Some(int * ivector_in, int ilength_in)
  * @brief Compute the elementary capacities of a  list of model_handles
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryCapacities4Some(i_list)
  * @param[in] i_list (list of integer) : list of indices of model_handles to compute elementary capacities
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of model_handle
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void model_handler_computeElementaryCapacities4Some(int * ivector_in, int ilength_in);

 /**
  * @fn void model_handler_computeElementaryConductivities4All(void)
  * @brief Compute the elementary conductivities of every thermal models
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryConductivities4All()
  * @endcond
  */
  extern "C" void model_handler_computeElementaryConductivities4All(void);

 /**
  * @fn void model_handler_computeElementaryConductivities4One(int id)
  * @brief Compute the elementary conductivities of one thermal model
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryConductivities4One(int id)
  * @param[in] id (integer) : index of the model_handle to compute elementary conductivities
  * @endcond
  *
  * @cond CDOC
  * @param[in] id (int) : index of the model_handle to compute elementary conductivities
  * @endcond
  */
  extern "C" void model_handler_computeElementaryConductivities4One(int id);

 /**
  * @fn void model_handler_computeElementaryConductivities4Some(int * ivector_in, int ilength_in)
  * @brief Compute the elementary conductivities of a  list of model_handles
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryConductivities4Some(i_list)
  * @param[in] i_list (list of integer) : list of indices of model_handles to compute elementary conductivities
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of model_handle
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void model_handler_computeElementaryConductivities4Some(int * ivector_in, int ilength_in);

 /**
  * @fn void model_handler_computeElementaryBulks4All(void)
  * @brief Compute the elementary bulks of every mechanical models
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryBulks4All()
  * @endcond
  */
  extern "C" void model_handler_computeElementaryBulks4All(void);

 /**
  * @fn void model_handler_computeElementaryBulks4One(int id)
  * @brief Compute the elementary bulks of one mechanical model
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryBulks4One(int id)
  * @param[in] id (integer) : index of the model_handle to compute elementary bulks
  * @endcond
  *
  * @cond CDOC
  * @param[in] id (int) : index of the model_handle to compute elementary bulks
  * @endcond
  */
  extern "C" void model_handler_computeElementaryBulks4One(int id);

 /**
  * @fn void model_handler_computeElementaryBulks4Some(int * ivector_in, int ilength_in)
  * @brief Compute the elementary bulks of a  list of model_handles
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryBulks4Some(i_list)
  * @param[in] i_list (list of integer) : list of indices of model_handles to compute elementary bulks
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of model_handle
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void model_handler_computeElementaryBulks4Some(int * ivector_in, int ilength_in);

 /**
  * @fn void model_handler_computeElementaryFext4All(void)
  * @brief Compute the elementary external forces of every mechanical models
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryFext4All()
  * @endcond
  */
  extern "C" void model_handler_computeElementaryFext4All(void);

 /**
  * @fn void model_handler_computeElementaryFext4One(int id)
  * @brief Compute the elementary external forces of one mechanical model
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryFext4One(int id)
  * @param[in] id (integer) : index of the model_handle to compute elementary external forces
  * @endcond
  *
  * @cond CDOC
  * @param[in] id (int) : index of the model_handle to compute elementary external forces
  * @endcond
  */
  extern "C" void model_handler_computeElementaryFext4One(int id);

 /**
  * @fn void model_handler_computeElementaryFext4Some(int * ivector_in, int ilength_in)
  * @brief Compute the elementary external forces of a  list of model_handles
  *
  * @cond PYDOC
  * usage : model_handler_computeElementaryFext4Some(i_list)
  * @param[in] i_list (list of integer) : list of indices of model_handles to compute elementary external forces
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of model_handle
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void model_handler_computeElementaryFext4Some(int * ivector_in, int ilength_in);

 /**
  * @fn void model_handler_updateState4All(void)
  * @brief Update the state of every model_handles
  *
  * @cond PYDOC
  * usage : model_handler_updateState4All()
  * @endcond
  */
  extern "C" void model_handler_updateState4All(void);

 /**
  * @fn void model_handler_updateState4One(int id)
  * @brief Update the state of a model_handle
  *
  * @cond PYDOC
  * usage : model_handler_updateState4One(int id)
  * @param[in] id (integer) : index of the model_handle to update
  * @endcond
  *
  * @cond CDOC
  * @param[in] id (int) : index of the model_handle to update
  * @endcond
  */
  extern "C" void model_handler_updateState4One(int id);

 /**
  * @fn void model_handler_updateState4Some(int * ivector_in, int ilength_in)
  * @brief Update the state of a list of model_handles
  *
  * @cond PYDOC
  * usage : model_handler_updateState4Some(i_list)
  * @param[in] i_list (list of integer) : list of indices of model_handles to update
  * @endcond
  *
  * @cond CDOC
  * @param[in] vector_in (int*) : list of indices of model_handle
  * @param[in] length (int)     : size of vector_in
  * @endcond
  */
  extern "C" void model_handler_updateState4Some(int * ivector_in, int ilength_in);

 /**
  * @fn void model_handler_assemble4All()
  * @brief Assemble the systems of equations
  *
  * @cond PYDOC
  * usage : model_handler_assemble4All()
  * @endcond
  */
  extern "C" void model_handler_assemble4All(void);

 /**
  * @fn void model_handler_computeFreeState4All()
  * @brief Compute free state of all systems
  *
  * @cond PYDOC
  * usage : model_handler_computeFreeStateAll()
  * @endcond
  */
  extern "C" void model_handler_computeFreeState4All(void);

 /**
  * @fn void model_handler_computeDofs4All()
  * @brief Compute degrees of freedom
  *
  * @cond PYDOC
  * usage : model_handler_computeDofs4All()
  * @endcond
  */
  extern "C" void model_handler_computeDofs4All(void);

 /**
  * @fn double model_handler_computeResidueNorm(void)
  * @brief Compute the norm of the residue
  *
  * @cond PYDOC
  * python usage : norm = model_handler_computeResidueNorm()
  * @return norm (double) : norm of the residue
  * @endcond
  *
  * @cond CDOC
  * @return norm (double) : norm of the residu
  * @endcond
  */
  extern "C" double model_handler_computeResidueNorm(void);

 /**
  * @fn void model_handler_cleanMemory()
  * @brief Free memory allocated within model_handler and modelization module
  *
  * @cond PYDOC
  * usage : model_handler_cleanMemory()
  * @endcond
  */
  extern "C" void model_handler_cleanMemory(void);

 /**
  * @fn int model_handler_getNbMecaModels(void)
  * @brief Get the number of mechanical models in simulation
  *
  * @cond PYDOC
  * usage : nb_mecaModels = model_handler_getNbMecaModels()
  * @return nb_mecaModels (integer) : number of mechanical models in simulation
  * @endcond
  *
  * @cond CDOC
  * @return (int) : number of mechanical models in simulation
  * @endcond
  */
// extern "C" int model_handler_getNbMecaModels(void);

 /**
  * @fn int model_handler_getNbTherModels(void)
  * @brief Get the number of thermal models in simulation
  *
  * @cond PYDOC
  * usage : nb_therModels = model_handler_getNbTherModels()
  * @return nb_therModels (integer) : number of thermal models in simulation
  * @endcond
  *
  * @cond CDOC
  * @return (int) : number of thermal models in simulation
  * @endcond
  */
// extern "C" int model_handler_getNbTherModels(void);

 /**
  * @fn void model_handler_computeDetectionCoor4All(void)
  * @brief Compute an intermediate state used by the contact detection  of every mechanical models
  *
  * This function cannot be used before the model_handler_sizeStateArrays function.
  *
  * @cond PYDOC
  * usage : model_handler_computeDetectionCoor4All()
  * @endcond
  */
// extern "C" void model_handler_computeDetectionCoor4All(void);

 /**
  * @fn void model_handler_getInertia(int ivalue1, double * rvector_out, int rlength_out)
  * @brief Get a copy of a inertia vector of a given mechanical model
  *
  * @cond PYDOC
  * usage : inertia = model_handler_getInertia(model_id, length)
  * @param[in] model_id (integer)     : id of considered model
  * @param[in] length   (integer)     : the length of returned vector
  * @return    inertia (double array) : the inertia of desired model
  * @endcond
  *
  * @cond CDOC
  * @param[in]  model_id (int)        : id of considered model
  * @param[out] vector_out (double *) : the inertia
  * @param[in]  length (integer)      : size of vector_out
  * @endcond
  */
//  extern "C" void model_handler_getInertia(int ivalue1, double * rvector_out, int rlength_out);

 /**
  * @fn void model_handler_getConnectivities(int id, int** i4_vector, int* i4_size)
  * @brief Get the connectivities of a model hander
  *
  * @cond PYDOC
  * usage : connec = model_handler_getConnectivities(ibdyty)
  * @param[in] ibdyty (integer)       : id of considered model
  * @return    connec (integer array) : the connectivies of considered model
  * @endcond
  *
  * The array contains :
  * first value -> number of elements
  * then for each elements -> size of connectivity, connectivity
  *
  * @cond CDOC
  * @param[in]     id   (int)        : id of considered model
  * @param[in,out] i4_vector (int**) : reference on the connectivities
  * @param[in]     i4_size (int*)    : the dimension of i4_vector array
  * @endcond
  */
  extern "C" void model_handler_getConnectivities(int id, int** i4_vector, int* i4_size);

 /**
  * @fn void model_handler_getCoordinates(int id, char* when, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of a the reference coordinates of a given mechanical model
  *
  * @cond PYDOC
  * usage : coor = model_handler_getCoordinates(ibdyty, when)
  * @param[in] ibdyty (integer)      : id of considered model
  * @param[in] when   (string)       : string defining what coordinates to get
  * @return    coor   (double array) : the coordinates of the model
  * @endcond
  *
  * "when" string can be "ref" for any kind of model
  * In case of mechanical model, "when" can also take the values : "now", "begin" or "detec"
  *
  * @cond CDOC
  * @param[in]     id   (int)             : id of considered model
  * @param[in]     when (char[5])         : string defining what coordinates to get
  * @param[in,out] matrix_out (double **) : reference on the coordinates
  * @param[in]     dim1 (int*)            : the first dimension of matrix_out array (nbDIME)
  * @param[in]     dim2 (int*)            : the second dimension of matrix_out array (nb_nodes)
  * @endcond
  */
  extern "C" void model_handler_getCoordinates(int id, char* when, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void model_handler_getNodalField(int id, char* what, int when, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of a nodal field of a given model handle
  *
  * @cond PYDOC
  * usage : field = model_handler_getNodalField(ibdyty, what, when)
  * @param[in] ibdyty (integer)         : id of considered model
  * @param[in] what   (string)          : string defining what nodal field to get
  * @param[in] when   (integer)         : index of the desired time depth for the field
  * @return    field  (double 2D array) : the nodal field of the model
  * @endcond
  *
  * "what" string can be "displacement" or "velocity" for mechanical models
  * and "temperature" for thermal models
  *
  * "when" integer with value 1 means current computing value, whereas 2 means value at beginning of step
  *
  * @cond CDOC
  * @param[in]     id   (int)             : id of considered model
  * @param[in]     what (char[12])        : string defining what nodal field to get
  * @param[in]     when (int)             : index of the desired time depths for the field
  * @param[in,out] matrix_out (double **) : reference on the nodal field
  * @param[in]     dim1 (int*)            : the first dimension of matrix_out array (nbDIME)
  * @param[in]     dim2 (int*)            : the second dimension of matrix_out array (nb_nodes)
  * @endcond
  */
  extern "C" void model_handler_getNodalField(int id, char* what, int when, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void model_handler_getPtrNodalField(int id, char* what, int when, double** pointer_out, int* dim1, int* dim2)
  * @brief Get a reference on a nodal field of a given model handle
  *
  * @cond PYDOC
  * usage : field = model_handler_getNodalField(ibdyty, what, when)
  * @param[in] ibdyty (integer)         : id of considered model
  * @param[in] what   (string)          : string defining what nodal field to get
  * @param[in] when   (integer)         : index of the desired time depth for the field
  * @return    field  (double 2D array) : reference on the nodal field of the model
  * @endcond
  *
  * "what" string can be "displacement" or "velocity" for mechanical models
  * and "temperature" for thermal models
  *
  * "when" integer with value 1 means current computing value, whereas 2 means value at beginning of step
  *
  * @cond CDOC
  * @param[in]     id   (int)              : id of considered model
  * @param[in]     what (char[12])         : string defining what nodal field to get
  * @param[in]     when (int)              : index of the desired time depths for the field
  * @param[in,out] pointer_out (double **) : reference on the nodal field
  * @param[in]     dim1 (int*)             : the first dimension of matrix_out array (nbDIME)
  * @param[in]     dim2 (int*)             : the second dimension of matrix_out array (nb_nodes)
  * @endcond
  */
  extern "C" void model_handler_getPtrNodalField(int id, char* what, int when, double** pointer_out, int* dim1, int* dim2);

 /**
  * @fn void model_handler_setNodalField(int id, char* what, int when, double * rvector_in, int rlength_in)
  * @brief Set nodal field of a given model
  *
  * @cond PYDOC
  * usage : model_handler_setNodalField(ibdyty, what, when, field_value)
  * @param[in] ibdyty (integer)              : id of considered model
  * @param[in] what   (string)               : string defining what nodal field to set
  * @param[in] when   (integer)              : index of the desired time depth for the field
  * @param[in] nodal_field (double 2D array) : the new nodal field value of the model
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue1 (int)        : id of considered model
  * @param[in] what    (char*)      : string defining what nodal field to set
  * @param[in] when    (int)        : index of the desired time depth for the field
  * @param[in] vector_in (double *) : the new nodal field of the model at desired time depth
  * @param[in] length  (int)        : the length of vector_in
  * @endcond
  */
  extern "C" void model_handler_setNodalField(int ivalue1, char* what, int when, double * rvector_in, int rlength_in);

 /**
  * @fn void model_handler_getNodalFieldNames(int id, char** string_vector, int* string_size)
  * @brief Get the list of nodal fields in the model_handle
  *
  * @cond PYDOC
  * usage : field_names = model_handler_getNodalFieldNames(ibdyty)
  * @param[in] ibdyty (integer)           : id of considered model
  * @return    field_names (string array) : the nodal field names of the model handle
  * @endcond
  *
  * @cond CDOC
  * @param[in]     id   (int)             : id of considered model
  * @param[in,out] string_vector(char **) : reference on the list of nodal field names
  * @param[in]     string_size (int*)     : the size of string_vector
  * @endcond
  */
  extern "C" void model_handler_getNodalFieldNames(int id, char** string_vector, int* string_size);

 /**
  * @fn void model_handler_getRhsContribution(int id, char* what, int when, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of a rhs contribution on nodes of a given model handle
  *
  * @cond PYDOC
  * usage : rhs_contrib = model_handler_getRhsContribution(ibdyty, what, when)
  * @param[in] ibdyty      (integer)         : id of considered model
  * @param[in] what        (string)          : string defining what rhs contribution to get
  * @param[in] when        (integer)         : index of the desired time depth for the rhs contribution
  * @return    rhs_contrib (double 2D array) : the rhs contribution of the model
  * @endcond
  *
  * "what" string can be "Fext" or "Fint" for mechanical and thermal models
  * "when" integer with value 1 means current computing value, whereas 2 means value at beginning of step
  * rhs contributions are computed on elements, thus the returned vector is assembled to get the contribution
  * nodes instead
  *
  * @cond CDOC
  * @param[in]     id   (int)             : id of considered model
  * @param[in]     what (char[12])        : string defining what rhs contribution to get
  * @param[in]     when (int)             : index of the desired time depths for the contribution
  * @param[in,out] matrix_out (double **) : reference on the contribution
  * @param[in]     dim1 (int*)            : the first dimension of matrix_out array (nbDIME)
  * @param[in]     dim2 (int*)            : the second dimension of matrix_out array (nb_nodes)
  * @endcond
  */
  extern "C" void model_handler_getRhsContribution(int id, char* what, int when, double** matrix_out, int* dim1, int* dim2);

 /**
  * @fn void model_handler_getRhsContributionNames(int id, char** c5_vector, int* c5_size)
  * @brief Get the list of rhs contribution of the model_handle
  *
  * @cond PYDOC
  * usage : field_names = model_handler_getRhsContributionNames(ibdyty)
  * @param[in] ibdyty (integer)           : id of considered model
  * @return    field_names (string array) : the rhs contribution names of the model handle
  * @endcond
  *
  * @cond CDOC
  * @param[in]     id   (int)             : id of considered model
  * @param[in,out] string_vector(char **) : reference on the list of nodal field names
  * @param[in]     string_size (int*)     : the size of string_vector
  * @endcond
  */
  extern "C" void model_handler_getRhsContributionNames(int id, char** c5_vector, int* c5_size);

 /**
  * @fn void model_handler_getElementaryField(int id, char* what, int when, double** matrix_out, int* dim1, int* dim2)
  * @brief Get a copy of a nodal field of a given mechanical model
  *
  * @cond PYDOC
  * usage : field = model_handler_getElementaryField(ibdyty, what, when)
  * @param[in] ibdyty (integer)         : id of considered model
  * @param[in] what   (string)          : string defining what elementary field to get
  * @param[in] when   (integer)         : index of the desired time depth for the field
  * @return    field  (double 2D array) : the nodal field of the model
  * @endcond
  *
  * @cond CDOC
  * @param[in]     id   (int)             : id of considered model
  * @param[in]     what (char[30])        : string defining what elementary field to get
  * @param[in]     when (int)             : index of the desired time depths for the field
  * @param[in,out] matrix_out (double **) : reference on the nodal field
  * @param[in]     dim1 (int*)            : the first dimension of matrix_out array (nbDIME)
  * @param[in]     dim2 (int*)            : the second dimension of matrix_out array (nb_nodes)
  * @endcond
  */
  extern "C" void model_handler_getElementaryField(int id, char* what, int when, double** matrix_out, int* dim1, int* dim2);

#endif /* _wrap_model_handler_h */
