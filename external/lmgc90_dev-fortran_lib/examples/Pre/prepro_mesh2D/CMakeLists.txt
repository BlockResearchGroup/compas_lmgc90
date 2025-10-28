
set(dir_name ${dir_name}_mesh2D)

# quick examples
if(${WITH_MATLIB})
  add_subdirectory(test_2blocks)
  add_subdirectory(test_maillage_mixte)
endif(${WITH_MATLIB})
add_subdirectory(test_beamT3)

# just_run
if(${WITH_MATLIB})
  add_subdirectory(test_2blocks_exploded)
  add_subdirectory(test_4T3_exploded)
  add_subdirectory(test_taylorQ8)
endif(${WITH_MATLIB})
