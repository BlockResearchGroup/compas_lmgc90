
#ifndef wrap_ExternalModels_h
#define wrap_ExternalModels_h

/**
 * @fn void ExternalModels_InitModels(void)
 * @brief Initialize the external models (if any) 
 *
 * @cond PYDOC
 * python usage : ExternalModels_InitModels()
 * @endcond
 */
 extern "C" void ExternalModels_InitModels(void);

/**
 * @fn void ExternalModels_StoreProperties(void)
 * @brief Store external models (if any) 
 *
 * @cond PYDOC
 * python usage : ExternalModels_StoreProperties()
 * @endcond
 */
 extern "C" void ExternalModels_StoreProperties(void);
 

/**
 * @fn void ExternalModels_CleanMemory(void)
 * @brief Free all memory allocated within ExternalModels module
 *
 * @cond PYDOC
 * python usage : ExternalModels_CleanMemory()
 * @endcond
 */
 extern "C" void ExternalModels_CleanMemory(void);

#endif /* wrap_ExternalModels_h */
