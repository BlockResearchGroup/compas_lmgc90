
set(dir_name mecaMAILx_2D)

# quick
if(NOT "${MATLIB_VERSION}" STREQUAL "none")
  add_subdirectory(1Q4-traction)
  add_subdirectory(Rocking)
  add_subdirectory(autocontact)
  add_subdirectory(dilatation)
  add_subdirectory(dilatation_umat)
  add_subdirectory(poutre)
endif(NOT "${MATLIB_VERSION}" STREQUAL "none")
add_subdirectory(braudel)

# just_run
if(NOT "${MATLIB_VERSION}" STREQUAL "none")
  add_subdirectory(2Q4-traction)
  add_subdirectory(2Q4_PressurePostCracking)
  add_subdirectory(Inclusion_PreDamage)  
  add_subdirectory(extrusion)
  add_subdirectory(mur)
endif(NOT "${MATLIB_VERSION}" STREQUAL "none")
add_subdirectory(taylor)

