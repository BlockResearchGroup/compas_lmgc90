
#ifndef wrap_integrator_h
#define wrap_integartor_h

 /**
  * @fn void integrator_addIntegrator(char * name, char * model, char * formulation, double * rvector_in, int rlength_in)
  * @brief Create a new integrator
  *
  * @cond PYDOC
  * usage : integrator_addIntegrator(name, model, formulation, params)
  * @param name (string) : type of the integrator 
  * @param name (string) : model of the integrator
  * @param name (string) : formulation of the integrator
  * @param params (real array) : real parameters associated to the integrator
  * @endcond
  *
  * @cond CDOC
  * @param[in] name (char *)        : type of the integrator
  * @param[in] model (char *)       : model of the integrator
  * @param[in] formulation (char *) : formulation of the integrator
  * @param[in] vector_in (double *) : real parameters of the integrator
  * @param[in] length (int)         : size of vector_in
  * @endcond
  *
  * The possible model type and formulation are:
  * "meca" -> "rigid" or "defor"
  * "ther" -> "defor"
  *
  * The possible integrator type and what parameters are expected are:
  * "moreau" -> timeStep, theta
  *
  */
  extern "C" void integrator_addIntegrator(char * name, char * model, char * formulation, double * rvector_in, int rlength_in);

 /**
  * @fn void integrator_setParameter(char * formulation, char * param_name, double value)
  * @brief Set the value of a parameter of an integrator
  *
  * @cond PYDOC
  * usage : integrator_setParameter(form, param_name, value)
  * @param formulation (string) : formulation of the desired integrator
  * @param param_name  (string) : name of the parameter to set
  * @param value       (real)   : new real value of the parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] formulation (char *) : formulation of the desired integrator
  * @param[in] param_name  (char *) : name of the parameter to set
  * @param[in] value       (double) : new real value of the parameter
  * @endcond
  */
  extern "C" void integrator_setParameter(char * formulation, char * param_name, double value);

 /**
  * @fn double integrator_getParameter(char * formulation, char * param_name)
  * @brief Set the value of a parameter of an integrator
  *
  * @cond PYDOC
  * usage : value = integrator_setParameter(form, param_name)
  * @param  formulation (string) : formulation of the desired integrator
  * @param  param_name  (string) : name of the parameter to get
  * @return value       (real)   : value of the parameter
  * @endcond
  *
  * @cond CDOC
  * @param[in] formulation (char *) : formulation of the desired integrator
  * @param[in] param_name  (char *) : name of the parameter to get
  * @return    value       (double) : value of the parameter
  * @endcond
  */
  extern "C" double integrator_getParameter(char * formulation, char * param_name);

 /**
  * @fn void integrator_cleanMemory()
  * @brief Free memory allocated within integrator module
  *
  * @cond PYDOC
  * usage : integrator_cleanMemory()
  * @endcond
  */
  extern "C" void integrator_cleanMemory(void);

#endif /* wrap_integrator */
