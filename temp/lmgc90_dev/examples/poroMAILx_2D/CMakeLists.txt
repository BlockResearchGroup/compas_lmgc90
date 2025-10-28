
set(dir_name poroMAILx_2D)

# quick
if(${WITH_MATLIB})
  add_subdirectory(ConfinedCompression)
endif(${WITH_MATLIB})
add_subdirectory(Stokes_Tube)

# just_run
if(${WITH_MATLIB})
  add_subdirectory(NonConfinedCompression)
endif(${WITH_MATLIB})
