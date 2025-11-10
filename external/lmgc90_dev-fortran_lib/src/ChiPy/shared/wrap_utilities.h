
#ifndef wrap_utilities_h
#define wrap_utilities_h

/**
 * @fn void utilities_logMes(char * ul_string, int string_size)
 * @brief ask to write a message
 *
 * If the message is too long (more than 256 characters)
 * it will be truncated
 *
 * @cond PYDOC
 * python usage : utilities_logMes(message)
 * @param[in] message (string) : log message to add
 * @param[in] length (integer) : length of the message string
 * @endcond
 *
 * @cond CDOC
 * @param[in] ul_string (char*) : log message to add
 * @param[in] string_size (int) : length of the message string
 * @endcond
 */
 extern "C" void utilities_logMes(char * ul_string, int string_size);

/**
 * @fn void utilities_DisableLogMes(void)
 * @brief disable printing of messages
 *
 * @cond PYDOC
 * python usage : utilities_DisableLogMes()
 * @endcond
 */
 extern "C" void utilities_DisableLogMes(void);

/**
 * @fn void utilities_EnableLogMes(void)
 * @brief enable priting of messages
 *
 * @cond PYDOC
 * python usage : utilities_EnableLogMes()
 * @endcond
 */
 extern "C" void utilities_EnableLogMes(void);

/**
  * @fn void utilities_setIoUnitLimits(int min_unit, int max_unit)
  * @brief set the interval of unit numbers lmgc90 can use to open file
  *
  *
  * @cond PyDOC
  * python usage : utilities_setIoUnitLimit(min_unit, max_unit)
  * @param[in] min_unit (integer) : minimum usable unit number
  * @param[in] max_unit (integer) : maximum usable unit number
  * @endcond
  *
  * @cond CDOC
  * @param[in] min_unit (int) : minimum usable unit number
  * @param[in] max_unit (int) : maximum usable unit number
  * @endcond
  */
  extern "C" void utilities_setIoUnitLimits(int min_unit, int max_unit);

/**
 * @fn void utilities_setStopMode(bool to_stop)
 * @brief Decide to stop or store a message in case of fatal error
 *
 * @cond PYDOC
 * python usage : utilities_setStopMode()
 * @endcond
 */
 extern "C" void utilities_setStopMode(bool to_stop);

/**
 * @fn void utilities_resetFatal(void)
 * @brief Clean fatal error state
 *
 * This function is not intended to be used in python but
 * by swig to throw an excpetion
 *
 */
 extern "C" int utilities_resetFatal();

/**
 * @fn int utilities_checkFatal(char * error_log)
 * @brief Get back fatal error message if there is one
 *
 * This function is not intended to be used in python but
 * by swig to throw an excpetion
 *
 */
 extern "C" int utilities_checkFatal(char** error_log);
 
 /**
 * @fn void utilities_OpenFileStandardOutput(char * ul_string, int string_size)
 * @brief Select the file for standard and errors outputs
 *
 * If the filename is too long (more than 256 characters)
 * it will be truncated
 *
 * @cond PYDOC
 * python usage : utilities_OpenFileStandardOutput(filename)
 * @param[in] filename (string) : the name of file
 * @param[in] length (integer)  : length the name of the file
 * @endcond
 *
 * @cond CDOC
 * @param[in] ul_string (char*) : the name of file
 * @param[in] string_size (int) : length the name of the file
 * @endcond
 */
 extern "C" void utilities_OpenFileStandardOutput(char * ul_string, int string_size);
 
 /**
 * @fn void utilities_CloseFileStandardOutput()
 * @brief Close the file for standard and errors outputs
 *
 * @cond PYDOC
 * python usage : utilities_CloseFileStandardOutput()
 * @endcond
 */
 extern "C" void utilities_CloseFileStandardOutput();

 /**
 * @fn void utilities_InitRandomSeed(int * ivector_in=nullptr, int ilength_in=0)
 * @brief Re-initialize the seed of the build-in random function
 *
 * @cond PYDOC
 * python usage : utilities_InitRandomSeed([seed])
 * @param[in] seed (integer array) : an optional desired input seed
 * @endcond
 *
 * @cond CDOC
 * @param[in] ivector_in (int*) : an optional desired input seed
 * @param[in] ilength_in (int)  : the size of the input seed vector
 * @endcond
 */
 extern "C" void utilities_InitRandomSeed(int * ivector_in=nullptr, int ilength_in=0);

 /**
 * @fn void utilities_Finalize()
 * @brief End of simulation operations
 * 
 * Only close all possibly opened units by the program.
 *
 * @cond PYDOC
 * python usage : utilities_Finalize()
 * @endcond
 */
 extern "C" void utilities_Finalize();

#endif /* wrap_utilities_h */
