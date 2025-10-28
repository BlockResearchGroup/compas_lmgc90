
# sources
set( LMGC90_CORE_IO_SRCS mod_h5_format.f90
                         mod_lmgc90_hdf5.f90
                         mod_h5_mailx.f90
                         mod_h5_rigid.f90
                         mod_h5_interaction.f90
                         mod_lmgc90_hdf5_hl.f90
                        )

include_directories( ${HDF5_INCLUDE_DIRS} )

add_library( lmgc_core_io ${LMGC90_CORE_IO_SRCS} )

target_link_libraries( lmgc_core_io lmgc_core_shared
                                    lmgc_core_contact_2d
                                    lmgc_core_contact_3d
                                    lmgc_core_rigid_2d
                                    lmgc_core_rigid_3d
                                    lmgc_core_mailx
                                    ${HDF5_LIBRARIES}
                                   )
