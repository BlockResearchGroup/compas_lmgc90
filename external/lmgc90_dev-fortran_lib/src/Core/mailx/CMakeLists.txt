# Pour rajouter un fichier, il faut ajouter le fichier dans la liste
# L'ordre d'ajout est important

SET(LMGC90_CORE_MAILx_SRCS mod_a_BDARY_ALpxx.f90
                           mod_a_BDARY_CLxxx.f90
                           mod_a_BDARY_xSpxx.f90
                           mod_a_BDARY_DISKL.f90
                           mod_a_BDARY_POLYD.f90
                           mod_a_BDARY_PT2DL.f90
                           mod_a_mecaEF.f90
                           mod_a_poroEF.f90
                           mod_a_mecaEF_bar.f90
                           mod_a_mecaEF_iso.f90
                           mod_a_mecaEF_shell.f90
                           mod_a_mecaEF_shb.f90
                           mod_a_mecaEF_discrete.f90
                           mod_a_mecaEF_joint.f90			   
                           mod_a_therEF.f90
                           mod_a_therEF_iso.f90
                           mod_a_therEF_bar.f90
                           mod_a_poroEF_iso.f90
                           mod_MAILx_type.f90
                           mod_mecaMAILx.f90
                           mod_therMAILx.f90
                           mod_poroMAILx.f90
                           mod_a_genericEF_iso.f90
                           mod_a_multiEF_iso.f90 
                           mod_a_multiEF.f90
                           mod_multiMAILx.f90
                           mod_MAILx.f90
                          )

# Lieu ou se trouve les .mod pour les USE
INCLUDE_DIRECTORIES( ${LAPACK95_INCLUDE_DIR} )

# Land le cas ou on veut utiliser -L/Repertoire -llibrary
# Ce n est pas homogene avec ailleurs
add_library(lmgc_core_mailx STATIC ${LMGC90_CORE_MAILx_SRCS})
SET(LMGC_CORE_MAILx_TARGET_LIBS lmgc_bindings_FEM
                                lmgc_bindings_user
                                lmgc_core_shared
                                lmgc_core_other_contribs
                                ${LAPACK_LIBRARIES})

TARGET_LINK_LIBRARIES(lmgc_core_mailx ${LMGC_CORE_MAILx_TARGET_LIBS})


# TESTS #

# ugly but necessary
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lgcc_s")
endif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI")

if( NOT ${NO_TEST} )
  add_executable( test_genericEF_iso test_genericEF_iso.f90)
  target_link_libraries(test_genericEF_iso lmgc_core_mailx)
  set_property(TARGET test_genericEF_iso PROPERTY LINKER_LANGUAGE Fortran)
endif( NOT ${NO_TEST} )
