
SET(LMGC90_PYTHON_KERNEL2_SRCS wrap_nlgs.f90
                               wrap_cpg.f90
                               wrap_mp_solver.f90
                              )
SET(LMGC90_PYTHON_KERNEL3_SRCS wrap_nlgs_3D.f90
                               wrap_cpg_3D.f90
                               wrap_mp_solver_3D.f90
                              )

set(LMGC90_PYTHON_GLOBAL_SRCS wrap_global_thermal_solver.f90
                              )

SET(LMGC90_PYTHON_SICONOS_SRCS wrap_SiconosNumerics.f90
                              )

SET(LMGC90_PYTHON_DDM_2D_SRCS wrap_DDM_2D.f90)

SET(LMGC90_PYTHON_DDM_3D_SRCS wrap_DDM_3D.f90
                              wrap_DDM_ExternalFEM.f90
                              )

INCLUDE_DIRECTORIES( ${LAPACK95_INCLUDE_DIR} )

ADD_LIBRARY(lmgc_python_kernel_2d ${LMGC90_PYTHON_KERNEL2_SRCS})
TARGET_LINK_LIBRARIES(lmgc_python_kernel_2d lmgc_core_kernel_2d
                                            ${LAPACK_LIBRARIES}
                                           )

ADD_LIBRARY(lmgc_python_kernel_3d ${LMGC90_PYTHON_KERNEL3_SRCS})	
TARGET_LINK_LIBRARIES(lmgc_python_kernel_3d lmgc_core_kernel_3d
                                            ${LAPACK_LIBRARIES}
                                           )

add_library(lmgc_python_kernel_global ${LMGC90_PYTHON_GLOBAL_SRCS})	
target_link_libraries(lmgc_python_kernel_global lmgc_core_kernel_global
                                            ${LAPACK_LIBRARIES}
                                           )

SET(_headers ${CMAKE_CURRENT_SOURCE_DIR}/wrap_nlgs.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_cpg.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_mp_solver.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_nlgs_3D.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_cpg_3D.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_mp_solver_3D.h
             ${CMAKE_CURRENT_SOURCE_DIR}/wrap_global_thermal_solver.h
            )

SET(_HEADERS wrap_nlgs.h
             wrap_cpg.h
             wrap_mp_solver.h
             wrap_nlgs_3D.h
             wrap_cpg_3D.h
             wrap_mp_solver_3D.h
             wrap_global_thermal_solver.h
            )


if(${WITH_SICONOS_NUMERICS})
    ADD_LIBRARY(lmgc_python_siconos ${LMGC90_PYTHON_SICONOS_SRCS})	
    TARGET_LINK_LIBRARIES(lmgc_python_siconos lmgc_core_siconos
                                              ${LAPACK_LIBRARIES}
                                              )
    SET(_headers ${_headers}
                 ${CMAKE_CURRENT_SOURCE_DIR}/wrap_SiconosNumerics.h
                )
    SET(_HEADERS ${_HEADERS} wrap_SiconosNumerics.h)
endif(${WITH_SICONOS_NUMERICS})

set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

if(${WITH_MPI})
    ADD_LIBRARY(lmgc_python_ddm_2d ${LMGC90_PYTHON_DDM_2D_SRCS})	
    TARGET_LINK_LIBRARIES(lmgc_python_ddm_2d lmgc_core_ddm_2d
                                             ${LAPACK_LIBRARIES}
                                             )
    ADD_LIBRARY(lmgc_python_ddm_3d ${LMGC90_PYTHON_DDM_3D_SRCS})	
    TARGET_LINK_LIBRARIES(lmgc_python_ddm_3d lmgc_core_ddm_3d
                                             ${LAPACK_LIBRARIES}
                                             )
    SET(_headers ${_headers}
                 ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DDM_2D.h
                 ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DDM_3D.h
                 ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DDM_ExternalFEM.h
                )
    SET(_HEADERS ${_HEADERS} wrap_DDM_2D.h wrap_DDM_3D.h wrap_DDM_ExternalFEM.h)
endif(${WITH_MPI})

if(${BUILD_C_LIB})
  FILE(GLOB _sources ${CMAKE_CURRENT_SOURCE_DIR} wrap_*.f90)
  set( _sources ${CMAKE_CURRENT_SOURCE_DIR}/wrap_nlgs.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/wrap_cpg.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/wrap_mp_solver.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/wrap_nlgs_3D.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/wrap_cpg_3D.f90
                ${CMAKE_CURRENT_SOURCE_DIR}/wrap_mp_solver_3D.f90
               )

  if(${WITH_SICONOS_NUMERICS})
      set( _sources ${_sources} ${CMAKE_CURRENT_SOURCE_DIR}/wrap_SiconosNumerics.f90)
  endif(${WITH_SICONOS_NUMERICS})

  if(${WITH_MPI})
      set( _sources ${_sources} ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DDM_2D.f90
                                ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DDM_3D.f90
                                ${CMAKE_CURRENT_SOURCE_DIR}/wrap_DDM_ExternalFEM.f90
                               )
  endif(${WITH_MPI})

  set( sources_list ${sources_list} ${_sources} PARENT_SCOPE)
  install(FILES ${_HEADERS}
          DESTINATION include/LMGC90/kernel
         )
endif(${BUILD_C_LIB})
