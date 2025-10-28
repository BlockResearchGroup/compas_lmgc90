
include( ${CMAKE_SOURCE_DIR}/ci_scripts/make_test.cmake )

#TODO add tests depending on WITH_MATLIB value ?

add_subdirectory(RIGID_2D)
add_subdirectory(RIGID_3D)

add_subdirectory(mecaMAILx_2D)
add_subdirectory(mecaMAILx_3D)

add_subdirectory(mecaMAILx_RIGID_2D)
add_subdirectory(mecaMAILx_RIGID_3D)

add_subdirectory(poroMAILx_2D)
add_subdirectory(poroMAILx_3D)

add_subdirectory(therMAILx_1D)
add_subdirectory(therMAILx_2D)

add_subdirectory(Pre)
