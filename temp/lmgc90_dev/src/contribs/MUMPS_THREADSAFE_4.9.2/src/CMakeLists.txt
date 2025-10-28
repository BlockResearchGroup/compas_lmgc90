INCLUDE_DIRECTORIES(${MUMPS_SOURCE_DIR}/include)
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
IF(ENABLE_SEQUENTIAL)
  INCLUDE_DIRECTORIES(${MUMPS_SOURCE_DIR}/libseq)
ENDIF(ENABLE_SEQUENTIAL)
IF(ENABLE_PORD)
  INCLUDE_DIRECTORIES(${MUMPS_SOURCE_DIR}/PORD/include)
ENDIF(ENABLE_PORD)

# mumps_common library ########################################################
SET(MUMPS_COMMON_SOURCES
mumps_part9.F
mumps_common.c
mumps_ooc_common.F
mumps_orderings.c
mumps_size.c
mumps_io.c
mumps_io_basic.c
mumps_io_thread.c
mumps_io_err.c
mumps_static_mapping.F
tools_common_mod.F
)
IF(ENABLE_SHARED)
  ADD_LIBRARY(mumps_common SHARED ${MUMPS_COMMON_SOURCES})
  # Force GNU89 inline semantics so inline functions get emitted (fixes undefined refs)
  target_compile_options(mumps_common PRIVATE -fgnu89-inline)
  #SET_TARGET_PROPERTIES(mumps_common PROPERTIES LINK_FLAGS '-Wl,-z,defs')
  IF(ENABLE_SEQUENTIAL)
    TARGET_LINK_LIBRARIES(mumps_common mpiseq)
  ENDIF(ENABLE_SEQUENTIAL)
  IF(ENABLE_PORD)
    TARGET_LINK_LIBRARIES(mumps_common pord)
  ENDIF(ENABLE_PORD)
  TARGET_LINK_LIBRARIES(mumps_common ${CMAKE_THREAD_LIBS_INIT})
ELSE(ENABLE_SHARED)
  ADD_LIBRARY(mumps_common STATIC ${MUMPS_COMMON_SOURCES})
  # Force GNU89 inline semantics so inline functions get emitted (fixes undefined refs)
  target_compile_options(mumps_common PRIVATE -fgnu89-inline)
ENDIF(ENABLE_SHARED)
INSTALL(TARGETS mumps_common DESTINATION lib)

#INCLUDE(AddFileDependencies)
#Add_File_Dependencies(mumps_ooc_common.F mumps_orderings.o)
#SET_SOURCE_FILES_PROPERTIES(mumps_ooc_common.F PROPERTIES OBJECT_DEPENDS mumps_orderings.c)
###############################################################################

# mumps library ###############################################################
FOREACH(ARITH ${MUMPS_BUILD_VERSIONS})
  SET(MUMPS_${ARITH}_SOURCES
    ${ARITH}mumps_part1.F
    ${ARITH}mumps_part2.F
    ${ARITH}mumps_part3.F
    ${ARITH}mumps_part4.F
    ${ARITH}mumps_part5.F
    ${ARITH}mumps_part6.F
    ${ARITH}mumps_part7.F
    ${ARITH}mumps_part8.F
    ${ARITH}mumps_comm_buffer.F
    ${ARITH}mumps_load.F
    ${ARITH}mumps_ooc_buffer.F
    ${ARITH}mumps_ooc.F
    ${ARITH}mumps_struc_def.F
  )
  CONFIGURE_FILE(mumps_c.c ${CMAKE_CURRENT_BINARY_DIR}/${ARITH}mumps_c.c)
  IF(ENABLE_SHARED)
    ADD_LIBRARY(${ARITH}mumps SHARED ${MUMPS_${ARITH}_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/${ARITH}mumps_c.c)
    #SET_TARGET_PROPERTIES(${ARITH}mumps PROPERTIES LINK_FLAGS '-Wl,-z,defs')
    IF(ENABLE_METIS)
     TARGET_LINK_LIBRARIES(${ARITH}mumps ${Metis_LIBRARIES})
    ENDIF(ENABLE_METIS)
  ELSE(ENABLE_SHARED)
    ADD_LIBRARY(${ARITH}mumps STATIC ${MUMPS_${ARITH}_SOURCES} ${CMAKE_CURRENT_BINARY_DIR}/${ARITH}mumps_c.c)
    IF(ENABLE_METIS)
     TARGET_LINK_LIBRARIES(${ARITH}mumps ${Metis_LIBRARIES})
    ENDIF(ENABLE_METIS)
  ENDIF(ENABLE_SHARED)
  INSTALL(TARGETS ${ARITH}mumps DESTINATION lib)
  IF(ENABLE_SEQUENTIAL)
    TARGET_LINK_LIBRARIES(${ARITH}mumps mpiseq)
  ENDIF(ENABLE_SEQUENTIAL)
  IF(ENABLE_PORD)
    TARGET_LINK_LIBRARIES(${ARITH}mumps pord)
  ENDIF(ENABLE_PORD)
  IF(ENABLE_METIS)
    TARGET_LINK_LIBRARIES(${ARITH}mumps ${Metis_LIBRARIES})
  ENDIF(ENABLE_METIS)
  IF(ENABLE_SCOTCH)
    TARGET_LINK_LIBRARIES(${ARITH}mumps ${Scotch_LIBRARIES})
  ENDIF(ENABLE_SCOTCH)
  TARGET_LINK_LIBRARIES(${ARITH}mumps mumps_common ${BLAS_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
  ADD_DEPENDENCIES(${ARITH}mumps mumps_common)
ENDFOREACH(ARITH)
###############################################################################
