
set(dir_name ${dir_name}_wall2D)

# quick
add_subdirectory(2_briques_cablees)
add_subdirectory(test_rigid_bricks)

# just_run
if(${WITH_MATLIB})
  add_subdirectory(cisaillement_mur)
  add_subdirectory(test_deformable_bricks)
  add_subdirectory(test_exploded_deformable_bricks)
endif(${WITH_MATLIB})

