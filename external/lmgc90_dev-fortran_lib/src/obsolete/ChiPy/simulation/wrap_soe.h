
#ifndef wrap_soe_h
#define wrap_soe_h

 /**
  * @fn void soe_setStorage(char * cvalue)
  * @brief set storage of matrices 
  *
  * @cond PYDOC
  * python usage : set_setStorage(storage)
  * @param[in] storage_type (string of size 8) : type of storage to use
  * @endcond
  *
  * \n possible values for storage_type are "dense", "sparse" and "exploded"
  *
  * @cond CDOC
  * @param[in] cvalue (char[8]) : type of storage to use
  * @endcond
  */
  extern "C" int soe_setStorage(char * cvalue);

 /**
  * @fn void soe_cleanMemory()
  * @brief Free memory allocated within soe module
  *
  * @cond PYDOC
  * usage : soe_cleanMemory()
  * @endcond
  */
  extern "C" void soe_cleanMemory(void);

#endif /* wrap_soe */
