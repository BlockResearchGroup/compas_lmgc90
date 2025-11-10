#################################################################################
#  $Id: matlib_config.cmake 124 2013-01-11 16:41:33Z lstainier $
#################################################################################
#  This file is part of ZorgLib, a computational simulation framework           #
#  for thermomechanics of solids and structures (systems in general).           #
#                                                                               #
#  Copyright (c) 2001-2013, L. Stainier.                                        #
#  See file LICENSE.txt for license information.                                #
#  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.      #
#################################################################################

function(configure)

  include(CheckIncludeFileCXX)
  
  #FORTRAN_UNDERSCORE
  set(FORTRAN_UNDERSCORE 1)

  #HAVE_UNORDERED_MAP
  check_include_file_cxx("unordered_map" HAVE_UNORDERED_MAP)

  #HAVE_TR1_UNORDERED_MAP
  check_include_file_cxx("tr1/unordered_map" HAVE_TR1_UNORDERED_MAP)

  #HAVE_HASH_MAP
  check_include_file_cxx("hash_map" HAVE_HASH_MAP)

  #HAVE_EXT_HASH_MAP
  check_include_file_cxx("ext/hash_map" HAVE_EXT_HASH_MAP)

  #HAVE_SSTREAM
  check_include_file_cxx("sstream" HAVE_SSTREAM)

  #USE_LAPACK
  if(ENABLE_BLAS_LAPACK AND NOT LAPACK_FOUND)
    if(__APPLE__)
      set(BLA_VENDOR "Apple")
    endif(__APPLE__)
    find_package(LAPACK)
    if(LAPACK_FOUND)
      set(MATLIB_USE_LAPACK 1)
      link_libraries(${LAPACK_LIBRARIES})
      if(APPLE)
        set(MATLIB_USE_VECLIB 1)
      endif(APPLE)
    endif(LAPACK_FOUND)
  endif(ENABLE_BLAS_LAPACK AND NOT LAPACK_FOUND)

  #USE_NAMESPACE
  if(ENABLE_NAMESPACE)
    set(MATLIB_USE_NAMESPACE 1)
  endif(ENABLE_NAMESPACE)

  if(NOT ${PYTHON_VERSION_MAJOR} LESS 3)
    set(MATLIB_USE_PYTHON3 1)
  endif(NOT ${PYTHON_VERSION_MAJOR} LESS 3)

  configure_file(${CMAKE_MODULE_PATH}/config.h.cmake.in
                 ${PROJECT_BINARY_DIR}/include/matlib_config.h)

endfunction()
