
set(dir_name mecaMAILx_RIGID_3D)

if(${WITH_MATLIB})
  # quick
  add_subdirectory(poutre_cubeR)
  add_subdirectory(poutre_cubesR)
  add_subdirectory(polyd_1block_coro)
  add_subdirectory(cube_visible_drvdof)
  # just_run
  add_subdirectory(2_briques)
  add_subdirectory(cube_cylindreR)
endif(${WITH_MATLIB})

