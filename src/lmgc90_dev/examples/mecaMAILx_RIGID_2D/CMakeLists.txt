
set(dir_name mecaMAILx_RIGID_2D)

if(${WITH_MATLIB})
  # quick
  add_subdirectory(4cubes)
  add_subdirectory(marble)
  add_subdirectory(mur_gd)
  # just_run
  add_subdirectory(gard2D)
endif(${WITH_MATLIB})

