SET(MUMPS_LIB_PORD_SOURCES
bucket.c
ddbisect.c
ddcreate.c
gbipart.c
gbisect.c
gelim.c
graph.c
interface.c
minpriority.c
multisector.c
nestdiss.c
sort.c
symbfac.c
tree.c
)
INCLUDE_DIRECTORIES(${MUMPS_SOURCE_DIR}/PORD/include)

IF(ENABLE_SHARED)
  ADD_LIBRARY(pord SHARED ${MUMPS_LIB_PORD_SOURCES})
  #SET_TARGET_PROPERTIES(pord PROPERTIES LINK_FLAGS '-Wl,-z,defs')
ELSE(ENABLE_SHARED)
  ADD_LIBRARY(pord STATIC ${MUMPS_LIB_PORD_SOURCES})
ENDIF(ENABLE_SHARED)
INSTALL(TARGETS pord DESTINATION lib)

