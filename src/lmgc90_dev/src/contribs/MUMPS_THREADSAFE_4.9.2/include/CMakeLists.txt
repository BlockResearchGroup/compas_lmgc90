INSTALL(FILES mumps_compat.h mumps_c_types.h DESTINATION include)
FOREACH(ARITH ${MUMPS_BUILD_VERSIONS})
  INSTALL(FILES ${ARITH}mumps_c.h ${ARITH}mumps_root.h ${ARITH}mumps_struc.h DESTINATION include)
ENDFOREACH(ARITH)
