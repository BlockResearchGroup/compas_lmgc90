
add_library(lmgc90_F_lib SHARED dummy.f90)
target_link_libraries(lmgc90_F_lib lmgc_core_post_3d
                                   lmgc_core_post_2d
                     )
set_property(TARGET lmgc90_F_lib PROPERTY OUTPUT_NAME lmgc90)
set_property(TARGET lmgc90_F_lib PROPERTY LINKER_LANGUAGE Fortran)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/lmgc90.cfg.in
               ${CMAKE_BINARY_DIR}/lmgc90.cfg
              )
