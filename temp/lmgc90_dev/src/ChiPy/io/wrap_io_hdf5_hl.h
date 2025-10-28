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
 *=========================================================================*/

#ifndef wrap_io_hdf5
#define wrap_io_hdf5

/**
  * @fn void io_hdf5_initOutFile(char * cvalue1)
  * @brief Init HDF5 file in which to write results
  *
  * @cond PYDOC
  * python usage : io_hdf5_initOutFile(filename)
  * @param[in] filename (string) : file in which to write
  * @endcond
  *
  * @cond CDOC
  * @param[in] cvalue1 (char *) : file in which to write
  * @endcond
  */
  extern "C" void io_hdf5_initOutFile(char * cvalue1) ;

/**
  * @fn void io_hdf5_write(void)
  * @brief write output data in HDF5 file (GPV, DOF and VlocRloc).
  *
  * @cond PYDOC
  * python usage : io_hdf5_write()
  * @endcond
  */
  extern "C" void io_hdf5_write(void) ;

/**
  * @fn void io_hdf5_write_last(char * cvalue1)
  * @brief write output data in HDF5 file (GPV, DOF and VlocRloc) in file
  *
  * @cond PYDOC
  * python usage : io_hdf5_write_last(filename)
  * @param[in] filename (string) : file in which to write
  * @endcond
  *
  * @cond CDOC
  * @param[in] cvalue1 (char *) : file in which to write
  * @endcond
  */
  extern "C" void io_hdf5_write_last(char * cvalue1) ;

/**
  * @fn void io_hdf5_read(char * cvalue1, int step)
  * @brief read output data from HDF5 file (DOF and VlocRloc).
  *
  * @cond PYDOC
  * python usage : io_hdf5_read(filename, step)
  * @param[in] filename (string) : file to read
  * @param[in] step    (integer) : step number to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] file (char *) : file to read
  * @param[in] step (int)    : step number to read
  * @endcond
  */
  extern "C" void io_hdf5_read(char * cvalue1, int step) ;

/**
  * @fn void io_hdf5_cleanMemory(void)
  * @brief cleanMemory of io_hdf5 module
  *
  * @cond PYDOC
  * python usage : io_hdf5_cleanMemory()
  * @endcond
  */
  extern "C" void io_hdf5_cleanMemory(void) ;

/**
  * @fn void io_hdf5_fixVersion(int version)
  * @brief Will try to fix the file when reading it
  *
  * Because the parameters changed within version 0,
  * this flag is needed to fix the file whend reading it.
  *
  * @cond PYDOC
  * python usage : io_hdf5_fixVersion(version)
  * @endcond
  */
  extern "C" void io_hdf5_fixVersion(int version) ;

#endif /* wrap_io_hdf5 */
