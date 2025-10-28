
set(dir_name "mecaMAILx_3D/demmefi")

add_subdirectory(compression)
add_subdirectory(compression_temperature)
add_subdirectory(triaxial_compression)
# way too slow...
#add_subdirectory(diagonal_compression_hybrid)
add_subdirectory(energy_regularization)
add_subdirectory(pure_shear)
add_subdirectory(shear_hybrid)
add_subdirectory(traction)
add_subdirectory(traction_hybrid)
