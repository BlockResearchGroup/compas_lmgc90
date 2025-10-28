
SET(LMGC90_CORE_MELIMELO_SRCS mod_visitor.f90
                              mod_blmty.f90
                              mod_nodty.f90
                              mod_tacty.f90
                              mod_drvdofty.f90
                              mod_initvalty.f90
                              mod_avatar.f90
                              mod_avatar_container.f90
                              mod_entity.f90
                              mod_entity_container.f90
                              melimelo.f90
                             )

INCLUDE_DIRECTORIES(${TEMPLATE_DIRECTORY})
ADD_LIBRARY(lmgc_core_melimelo ${LMGC90_CORE_MELIMELO_SRCS})
TARGET_LINK_LIBRARIES(lmgc_core_melimelo lmgc_core_shared
                                         lmgc_core_simulation
                                        )


add_executable( test_bulk_visit tst_bulk_visit.f90 )
target_link_libraries(test_bulk_visit lmgc_core_melimelo
                                      lmgc_core_simulation
                                     )
add_test( test_bulk_visit ${EXECUTABLE_OUTPUT_PATH}/test_bulk_visit)

add_executable( test_avatar tst_avatar.f90 )
target_link_libraries(test_avatar lmgc_core_melimelo
                                  lmgc_core_simulation
                                 )
add_test( test_avatar ${EXECUTABLE_OUTPUT_PATH}/test_avatar)

