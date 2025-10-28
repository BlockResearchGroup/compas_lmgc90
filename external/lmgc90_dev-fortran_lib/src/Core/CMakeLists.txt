
###############################################
####       Setting options for Core        ####
set(CMAKE_C_FLAGS       ${LMGC90_C_FLAGS}      )
set(CMAKE_CXX_FLAGS     ${LMGC90_CXX_FLAGS}    )
set(CMAKE_Fortran_FLAGS ${LMGC90_Fortran_FLAGS})
###############################################

#set variable to know where templates are...
set( TEMPLATE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/templates )

add_subdirectory(contribs)
add_subdirectory(shared)
if( ${WITH_HDF5} )
  add_subdirectory( io )
endif( ${WITH_HDF5} )
add_subdirectory(rigid_2D)
add_subdirectory(rigid_3D)
add_subdirectory(mailx)
add_subdirectory(mbs)
add_subdirectory(contactor_2D)
add_subdirectory(contactor_3D)
add_subdirectory(contact_2D)
add_subdirectory(contact_3D)
add_subdirectory(kernel)
add_subdirectory(post)

if(${BUILD_PRE})
  add_subdirectory(pre_tools)
endif(${BUILD_PRE})

