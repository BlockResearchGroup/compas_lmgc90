
set(LMGC90_CORE_KERNEL2_SRCS mod_nlgs.f90
                             mod_cpg.f90
                             mod_mp_solver.f90
                             mod_ThermalSolver.f90
                            )
set(LMGC90_CORE_KERNEL3_SRCS mod_nlgs_3D.f90
                             mod_cpg_3D.f90
                             mod_mp_solver_3D.f90
                            )

set(LMGC90_CORE_GLOBAL_SRCS mod_global_thermal_solver.f90
                            )

set(LMGC90_CORE_SICONOS_SRCS mod_SiconosNumerics.f90
                            )

set(LMGC90_CORE_DDM_2D_SRCS mod_DDM_2D.f90
                            mod_nlgs.f90 mod_nlgs_3D.f90
                            mod_DDM_ExternalFEM.f90
                           )

set(LMGC90_CORE_DDM_3D_SRCS mod_DDM_3D.f90 
                            mod_nlgs.f90 mod_nlgs_3D.f90
                            mod_DDM_ExternalFEM.f90
                           )

include_directories(${LAPACK95_INCLUDE_DIR})

add_library(lmgc_core_kernel_2d ${LMGC90_CORE_KERNEL2_SRCS})
target_link_libraries(lmgc_core_kernel_2d lmgc_core_shared 
                                          lmgc_core_rigid_2d 
                                          lmgc_core_contact_2d 
                                          ${LAPACK_LIBRARIES}
                                         )

add_library(lmgc_core_kernel_3d ${LMGC90_CORE_KERNEL3_SRCS})
target_link_libraries(lmgc_core_kernel_3d lmgc_core_shared
                                          lmgc_core_rigid_3d
                                          lmgc_core_contact_3d
                                         )

add_library(lmgc_core_kernel_global ${LMGC90_CORE_GLOBAL_SRCS})
target_link_libraries(lmgc_core_kernel_global lmgc_core_shared
                                              lmgc_core_mailx
                                              lmgc_core_contact_2d
                                             )

if(${WITH_SICONOS_NUMERICS})
    add_library(lmgc_core_siconos ${LMGC90_CORE_SICONOS_SRCS})
    target_link_libraries(lmgc_core_siconos lmgc_core_shared 
                                            lmgc_core_contact_3d 
                                            
                                            )
    if(${BUILD_SICONOS_NUMERICS})
      target_link_libraries(lmgc_core_siconos ${SiconosNumerics_LIBRARIES})
      add_dependencies(lmgc_core_siconos Siconos)
    else(${BUILD_SICONOS_NUMERICS})
      target_link_libraries(lmgc_core_siconos Siconos::numerics)
    endif(${BUILD_SICONOS_NUMERICS})
endif(${WITH_SICONOS_NUMERICS})

if(${WITH_MPI})
    add_library(lmgc_core_ddm_2d ${LMGC90_CORE_DDM_2D_SRCS})
    target_link_libraries(lmgc_core_ddm_2d lmgc_core_shared 
                                           lmgc_core_rigid_2d 
                                           lmgc_core_contact_2d 
                                           ${LAPACK_LIBRARIES}
                                           )
    add_library(lmgc_core_ddm_3d ${LMGC90_CORE_DDM_3D_SRCS})
    target_link_libraries(lmgc_core_ddm_3d lmgc_core_shared 
                                           lmgc_core_rigid_3d 
                                           lmgc_core_contact_3d 
                                           ${LAPACK_LIBRARIES}
                                           )
endif(${WITH_MPI})

