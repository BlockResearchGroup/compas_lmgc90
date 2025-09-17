
###############################################
####       Setting options for ChiPy       ####
set(CMAKE_C_FLAGS       ${LMGC90_C_FLAGS}      )
set(CMAKE_CXX_FLAGS     ${LMGC90_CXX_FLAGS}    )
set(CMAKE_Fortran_FLAGS ${LMGC90_Fortran_FLAGS})
###############################################

# to get the list of .h files
set( headers_list )

# to get the list of .f90 files
# in the wrapper in case a shared library must be build
set( sources_list )

if( ${WITH_HDF5} )
  add_subdirectory( io )
endif( ${WITH_HDF5} )

add_subdirectory(shared)

add_subdirectory(mailx)

add_subdirectory(contact_2D)
add_subdirectory(contact_3D)
add_subdirectory(rigid_2D)
add_subdirectory(rigid_3D)

add_subdirectory(mbs)

add_subdirectory(kernel)

add_subdirectory(post)

add_subdirectory(user)

if( ${BUILD_PRE} )
  add_subdirectory(pre_tools)
endif( ${BUILD_PRE} )

add_subdirectory(utils)

# dependencies
SET(LMGC90_PYTHON_LINK_LIBRARIES  lmgc_python_shared
                                  lmgc_python_mailx
                                  lmgc_python_rigid_2d
                                  lmgc_python_mbs2d
                                  lmgc_python_post_2d
                                  lmgc_python_kernel_2d
                                  lmgc_python_contact_2d
                                  lmgc_python_rigid_3d
                                  lmgc_python_mbs3d
                                  lmgc_python_post_3d
                                  lmgc_python_kernel_3d
                                  lmgc_python_contact_3d
                                  lmgc_python_user
                                  lmgc_python_kernel_global
                                  )

if( ${BUILD_PRE} )
SET(LMGC90_PYTHON_LINK_LIBRARIES lmgc_python_pre_tools
                                 ${LMGC90_PYTHON_LINK_LIBRARIES}
                                 )
endif( ${BUILD_PRE} )

if( ${WITH_HDF5} )
SET(LMGC90_PYTHON_LINK_LIBRARIES lmgc_python_io
                                 ${LMGC90_PYTHON_LINK_LIBRARIES}
                                 )
endif( ${WITH_HDF5} )

if(${WITH_SICONOS_NUMERICS})
    SET(LMGC90_PYTHON_LINK_LIBRARIES ${LMGC90_PYTHON_LINK_LIBRARIES}
                                     lmgc_python_siconos
                                     )
endif(${WITH_SICONOS_NUMERICS})

if(${WITH_MPI})
    SET(LMGC90_PYTHON_LINK_LIBRARIES ${LMGC90_PYTHON_LINK_LIBRARIES}
                                     lmgc_python_ddm_2d
                                     lmgc_python_ddm_3d
                                     )
endif(${WITH_MPI})

if(${WITH_MATLIB})
    message(STATUS "matlib library : ${MATLIB_LIBRARY}")
    SET(LMGC90_PYTHON_LINK_LIBRARIES ${LMGC90_PYTHON_LINK_LIBRARIES} ${MATLIB_LIBRARY})
endif(${WITH_MATLIB})

# if shared library is to be build
if(${BUILD_C_LIB})
    include_directories( ${LAPACK95_INCLUDE_DIR} )
    add_library(lmgc90 SHARED ${sources_list})
    target_link_libraries(lmgc90 ${LMGC90_PYTHON_LINK_LIBRARIES})

    install(TARGETS lmgc90
            LIBRARY DESTINATION lib
           )
endif(${BUILD_C_LIB})

if(${BUILD_ChiPy})
    file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/pylmgc90/chipy)

    # preparing swig use
    include(${SWIG_USE_FILE})
    include_directories(${Python_ALL_INCLUDE}
                        ${CMAKE_SOURCE_DIR}/src/ChiPy)
    
    if(NOT ${PYTHON_VERSION_MAJOR} LESS 3)
        set(CMAKE_SWIG_FLAGS "-py3")
    else(NOT ${PYTHON_VERSION_MAJOR} LESS 3)
        set(CMAKE_SWIG_FLAGS "")
    endif(NOT ${PYTHON_VERSION_MAJOR} LESS 3)
    
    # optional compile flag definition
    if(${WITH_SICONOS_NUMERICS})
      set(CMAKE_SWIG_FLAGS "-DWITH_SICONOS" ${CMAKE_SWIG_FLAGS})
      add_definitions(-DWITH_SICONOS) 
    endif(${WITH_SICONOS_NUMERICS})
    
    if(${WITH_MPI})
      set(CMAKE_SWIG_FLAGS "-DWITH_MPI" ${CMAKE_SWIG_FLAGS})
      add_definitions(-DWITH_MPI)
    endif(${WITH_MPI})
    
    if( ${WITH_HDF5} )
      set( CMAKE_SWIG_FLAGS "-DWITH_HDF5" ${CMAKE_SWIG_FLAGS} )
      add_definitions( -DWITH_HDF5 )
    endif( ${WITH_HDF5} )

    # if the docstring documentation is to be build
    if(${WITH_DOCSTRING})
      configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile_chipy.cfg.in
                     ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile_chipy.cfg
                    )
      set(DOXY2SWIG_FILE ${CMAKE_SOURCE_DIR}/src/tools/swig/doxy2swig.py)
      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/docs/chipy_swig_docstrings.i
                         DEPENDS ${headers_list}
                         COMMAND ${CMAKE_COMMAND} -E copy
                         ${CMAKE_CURRENT_SOURCE_DIR}/docs/chipy_swig_docstrings.i
                         ${CMAKE_CURRENT_BINARY_DIR}/docs/chipy_swig_docstrings.i
                         COMMAND ${CMAKE_COMMAND} -E copy
                         ${DOXY2SWIG_FILE}
                         ${CMAKE_CURRENT_BINARY_DIR}/doxy2swig.py
                         COMMAND ${CMAKE_COMMAND} -E copy
                         ${CMAKE_CURRENT_SOURCE_DIR}/make_chipy_docstring.py
                         ${CMAKE_CURRENT_BINARY_DIR}/make_chipy_docstring.py
                         COMMAND ${DOXYGEN_EXECUTABLE}
                         ARGS Doxyfile_chipy.cfg
                         COMMAND ${Python_EXECUTABLE}
                         ARGS make_chipy_docstring.py
                        )
      add_custom_target(chipy_swig_docstrings DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/docs/chipy_swig_docstrings.i)
    else(${WITH_DOCSTRING})
      add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/docs/chipy_swig_docstrings.i
                         DEPENDS ${headers_list}
                         COMMAND ${CMAKE_COMMAND} -E copy
                         ${CMAKE_CURRENT_SOURCE_DIR}/docs/chipy_swig_docstrings.i
                         ${CMAKE_CURRENT_BINARY_DIR}/docs/chipy_swig_docstrings.i
                        )
      add_custom_target(chipy_swig_docstrings DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/docs/chipy_swig_docstrings.i)
    endif(${WITH_DOCSTRING})
    
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/chipy.i.in
                   ${CMAKE_CURRENT_BINARY_DIR}/chipy.i
                  )

    # defining swig target
    set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR}/pylmgc90/chipy)
    SET_SOURCE_FILES_PROPERTIES(${CMAKE_CURRENT_BINARY_DIR}/chipy.i PROPERTIES CPLUSPLUS ON )
    set(SWIG_MODULE_lmgc90_EXTRA_DEPS ${NUMPY_DOT_I} ${headers_list} chipy_swig_docstrings)

    if( ${CMAKE_VERSION} VERSION_LESS 3.8 )
      SWIG_ADD_MODULE(lmgc90 python ${CMAKE_CURRENT_BINARY_DIR}/chipy.i)
    else( ${CMAKE_VERSION} VERSION_LESS 3.8 )
      SWIG_ADD_LIBRARY(lmgc90 LANGUAGE python SOURCES ${CMAKE_CURRENT_BINARY_DIR}/chipy.i)
    endif( ${CMAKE_VERSION} VERSION_LESS 3.8 )

    SWIG_LINK_LIBRARIES(lmgc90 ${Python_LIBRARIES}
                               ${LMGC90_PYTHON_LINK_LIBRARIES}
                               ${LAPACK_LIBRARIES}
                               utilsFortran
                              )
    set_property(TARGET ${SWIG_MODULE_lmgc90_REAL_NAME} PROPERTY LINKER_LANGUAGE Fortran)
    # ugly but necessary...
    if( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
      set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} -nofor-main")
    endif( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
 

    # test of new packaging
    add_custom_target(pylmgc90_init ALL
                      COMMAND ${CMAKE_COMMAND}
                              -Ddir=${CMAKE_SOURCE_DIR}
                              -Dfile_in=${CMAKE_CURRENT_SOURCE_DIR}/macro/__init__.py.in
                              -Dfile_out=${CMAKE_BINARY_DIR}/pylmgc90/__init__.py
                              -DGIT_EXECUTABLE=${GIT_EXECUTABLE}
                              -Drelease="${YEAR}${RCWITHDOT}"
                              -P ${CMAKE_SOURCE_DIR}/cmake/scripts/set_revision_number.cmake
                      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/macro/__init__.py.in
                     )

    add_custom_command(TARGET ${SWIG_MODULE_lmgc90_REAL_NAME} POST_BUILD
                       COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/lmgc90.py 
                                                          ${CMAKE_BINARY_DIR}/pylmgc90/chipy/lmgc90.py
                       DEPENDS pylmgc90_init
                      )
    set( l __init__.py macro.py vtk_select.py vtk_display.py preconW.py ddm_utils.py config.py computation.py)
    foreach( f ${l} )
      add_custom_target(${f} ALL
                        COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/src/ChiPy/macro/${f}
                                                         ${CMAKE_BINARY_DIR}/pylmgc90/chipy/${f}
                        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/macro/${f}
                       )

    endforeach( f ${l} )

endif(${BUILD_ChiPy})
