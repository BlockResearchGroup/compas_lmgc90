#ifndef _wrap_meli_melo_h
#define _wrap_meli_melo_h

 /**
  * @fn void melimelo_createEntityContainer(void)
  * @brief Create a new empty container
  *
  * Only one entity container can be used at a time.
  * This ensure that you are starting with a fresh one,
  * it stops the programm otherwise.
  *
  * @cond PYDOC
  * usage : melimelo_createEntityContainer()
  * @endcond
  */
 extern "C" void melimelo_createEntityContainer(void);

 /**
  * @fn void melimelo_eraseEntityContainer(void)
  * @brief Remove all entities from the entity container
  *
  * @cond PYDOC
  * usage : melimelo_eraseEntityContainer(void)
  * @endcond
  *
  */
 extern "C" void melimelo_eraseEntityContainer(void);

 /**
  * @fn void melimelo_addEntity(int e_rank)
  * @brief Add a new entity to the entity container
  *
  * @cond PYDOC
  * usage : melimelo_addEntity(e_rank)
  * @param[in] e_rank (integer) : rank of the entity 
  * @endcond
  *
  * @cond CDOC
  * @param[in] e_rank (int) : rank of the entity 
  * @endcond
  */
 extern "C" void melimelo_addEntity(int e_rank);

 /**
  * @fn void melimelo_addAvatar(int a_rank, char * type, char * modelType)
  * @brief Add an avatar to an entity
  *
  * @cond PYDOC
  * usage : melimelo_addAvatar(a_rank, type, modelType)
  * @param[in] a_rank (integer) : rank of the avatar
  * @param[in] type (string)    : type of the avatar to add
  * @param[in] modelType (string) : type of the model of the avatar to add
  * @endcond
  *
  * @cond CDOC
  * @param[in] a_rank (int)   : rank of the avatar
  * @param[in] type (char[5]) : type of the avatar to add
  * @param[in] modelType (char[5]) : type of the model of the avatar to add
  * @endcond
  *
  * Possible values for type : "RBDY2", "RBDY3" or "MAILx"
  * Possible values for modelType : "MECAx" or "THERx"
  */
 extern "C" void melimelo_addAvatar(int a_rank, char * type, char * modelType);

 /**
  * @fn void melimelo_addBulk(int b_rank, char * type, char * model, char * behav, double * rvector_in, int rlength_in, int * ivector_in, int ilength_in)
  * @brief Add a bulk to an avatar
  *
  * @cond PYDOC
  * usage : melimelo_addBulk(b_rank, type, model, behav, rvect, ivect)
  * @param[in] b_rank (integer) : rank of the tact
  * @param[in] type  (string)   : avatar type
  * @param[in] model (string)   : model nickname
  * @param[in] behav (string)   : material parameter nickname
  * @param[in] rvector_in (double vector)  : anonymous real vector
  * @param[in] ivector_in (integer vector) : anonymous integer vector
  * @endcond

  * @cond CDOC
  * @param[in] b_rank (int)    : rank of the bulk
  * @param[in] type  (char[5]) : avatar type
  * @param[in] model (char[5]) : model nickname
  * @param[in] behav (char[5]) : material parameter nickname
  * @param[in] rvector_in (double *) : anonymous real vector
  * @param[in] rlength (int)         : size of anonymous real vector
  * @param[in] ivector_in (int *)    : anonymous integer vector
  * @param[in] ilength (int)         : size of anonymous integer vector
  * @endcond
  */
 extern "C" void melimelo_addBulk(int b_rank, char * type, char * model, char * behav, double * rvector_in, int rlength_in, int * ivector_in, int ilength_in);

 /**
  * @fn void melimelo_addNode(int n_rank, char * type, double * rvector_in, int rlength_in)
  * @brief Add a node to an avatar
  *
  * @cond PYDOC
  * usage : melimelo_addNode(n_rank, type, rvect)
  * @param[in] n_rank (integer) : rank of the node
  * @param[in] type  (string)   : type of node: NO1xx ... NO6xx
  * @param[in] rvector_in (double vector) : coordinates of the node
  * @endcond

  * @cond CDOC
  * @param[in] n_rank (int)          : rank of the node
  * @param[in] type  (char[5])       : type of node: NO1xx ... NO6xx
  * @param[in] rvector_in (double[]) : coordinates of the node
  * @param[in] rlength (int)         : size of the rvector_in vector (depends on type)
  * @endcond
  */
 extern "C" void melimelo_addNode(int n_rank, char * type, double * rvector_in, int rlength_in);

 /**
  * @fn void melimelo_addTact(int t_rank, char * type, char * color, double * rvector_in, int rlength_in, int * ivector_in, int ilength_in)
  * @brief Add a tact to an avatar
  *
  * @cond PYDOC
  * usage : melimelo_addTact(t_rank, type, color, rvect, ivect)
  * @param[in] t_rank (integer) : rank of the contactor
  * @param[in] type  (string)   : type of contactor
  * @param[in] color (string)   :  color of the object
  * @param[in] rvect (vector double) : anonymous real vector
  * @param[in] ivect (vector int)    : anonymous integer vector
  * @endcond
  *
  * @cond CDOC
  * @param[in] t_rank (int)    : rank of the contactor
  * @param[in] type  (char[5]) : type of contactor
  * @param[in] color (char[5]) :  color of the object
  * @param[in] rvector_in (double*) : anonymous real vector
  * @param[in] rlength (int)        : size of anonymous real vector
  * @param[in] ivector_in (int*)    : anonymous integer vector
  * @param[in] ilength (int)        : size of anonymous integer vector
  * @endcond
  *
  * Possible contactor type are "DISKx", "DISKb", "DISPx", "xKSID", "xPSID"
  * "JONCx", "POLYG", "PT2Dx", "CYLND", DNLYC', PLAnx. "POLYR", "PT3Dx",
  * "SPHER", "SPHEb", "ALpxx", "ASpx3", "ASpx4", "CLxxx", "CSxx3", "CSxx4",
  * "DISKL" or "PT2DL"
  */
 extern "C" void melimelo_addTact(int t_rank, char * type, char * color, double * rvector_in, int rlength_in, int * ivector_in, int ilength_in);

 /**
  * @fn void melimelo_addDrvDof(int d_rank, char ** cvector_in, int clength, int * ivector_in, int ilength_in, double * rvector_in, int rlength_in, char ** svector_in, int slength)
  * @brief Add a driven dof to an avatar
  *
  * @cond PYDOC
  * usage : melimelo_addDrvDof(d_rank, cvect, rvect, ivect, svect)
  * @param[in] d_rank (integer) : rank of the driven dof
  * @param[in] cvect (vector string) : anonymous string vector (strings of size 5)
  * @param[in] ivect (vector int)    : anonymous integer vector
  * @param[in] rvect (vector double) : anonymous real vector
  * @param[in] svect (vector string) : anonymous string vector (strings of size 128)
  * @endcond
  *
  * @cond CDOC
  * @param[in] d_rank (int)         : rank of the driven dof
  * @param[in] cvector_in (char**)  : anonymous string (of size 5) vector
  * @param[in] clength (int)        : size of first anonymous string vector
  * @param[in] ivector_in (int*)    : anonymous integer vector
  * @param[in] ilength (int)        : size of anonymous integer vector
  * @param[in] rvector_in (double*) : anonymous real vector
  * @param[in] rlength (int)        : size of anonymous real vector
  * @param[in] svector_in (char**)  : anonymous string (of size 128) vector
  * @param[in] slength (int)        : size of second anonymous string vector
  * @endcond
  *
  */
 extern "C" void melimelo_addDrvDof(int d_rank, char** cvector_in, int clength, int * ivector_in, int ilength_in, double * rvector_in, int rlength_in, char** svector_in, int slength);

 /**
  * @fn void melimelo_addInitValues(int i_rank, int * ivector_in, int ilength_in, double * rvector_in, int rlength_in)
  * @brief Add initial values to the degrees of freedom of an avatar
  *
  * @cond PYDOC
  * usage : melimelo_addInitValues(i_rank, ivect, rvect)
  * @param[in] i_rank (integer)      : rank of the dof initial value
  * @param[in] ivect (vector int)    : anonymous integer vector
  * @param[in] rvect (vector double) : anonymous real vector
  * @endcond
  *
  * @cond CDOC
  * @param[in] i_rank (int)         : rank of the dof initial value
  * @param[in] ivector_in (int*)    : anonymous integer vector
  * @param[in] ilength (int)        : size of anonymous integer vector
  * @param[in] rvector_in (double*) : anonymous real vector
  * @param[in] rlength (int)        : size of anonymous real vector
  * @endcond
  *
  */
 extern "C" void melimelo_addInitValues(int i_rank, int * ivector_in, int ilength_in, double * rvector_in, int rlength_in);

 /**
  * @fn void melimelo_closeEntityContainer(void)
  * @brief Close the entity container
  *
  * The container holding a linked list, transfers
  * it to an array
  *
  * @cond PYDDOC
  * usage : melimelo_closeEntityContainer()
  * @endcond
  */
 extern "C" void melimelo_closeEntityContainer(void);

 /**
  * @fn void melimelo_openEntityContainer(void)
  * @brief Open the entity container
  *
  * The container holding an array transfers it to
  * a linked list
  *
  * @cond PYDOC
  * usage : melimelo_openEntityContainer()
  * @endcond
  */
 extern "C" void melimelo_openEntityContainer(void);

 /**
  * @fn void melimelo_visitEntityContainer(void)
  * @brief Visit the entity container
  *
  * Go through all elements of the container and
  * perform the action stored in visitor module
  * (see visitor_setVisitor function)
  *
  * @cond PYDOC
  * usage : melimelo_visitEntityContainer()
  * @endcond
  */
// extern "C" void melimelo_visitEntityContainer(void);

 /**
  * @fn void melimelo_displayEntityContainer(void)
  * @brief Display the entity container on standard output
  *
  * @cond PYDOC
  * usage : melimelo_displayEntityContainer()
  * @endcond
  */
 extern "C" void melimelo_displayEntityContainer(void);

 /**
  * @fn void melimelo_pushEntityContainerInMdlHdl(void)
  * @brief Push every avatars of every entities of entities container in model_handler
  *
  * @cond PYDOC
  * usage : melimelo_pushEntityContainerInMdlHdl()
  * @endcond
  */
 extern "C" void melimelo_pushEntityContainerInMdlHdl(void);

 /**
  * @fn void melimelo_pushInitialValuesInMdlHdl(void)
  * @brief Push initial values of every avatars of every entities of entities container in model_handler
  *
  * @cond PYDOC
  * usage : melimelo_pushInitialValuesInMdlHdl()
  * @endcond
  */
 extern "C" void melimelo_pushInitialValuesInMdlHdl(void);

#endif /* _wrap_meli_melo_h */
