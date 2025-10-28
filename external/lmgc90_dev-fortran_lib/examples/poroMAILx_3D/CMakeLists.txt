
set(dir_name poroMAILx_3D)

#just_run
if(${WITH_MATLIB})
  add_subdirectory(ConfinedCompressionGD)
  add_subdirectory(NonConfinedCompressionGD)
endif(${WITH_MATLIB})
