

include_directories(${UMFPACK_INCLUDE_DIR})

add_library(umfpackBinding umfpackBinding.c)
target_link_libraries(umfpackBinding ${UMFPACK_LIBRARY})

set(SPARSE_LIBRARIES umfpackBinding ${UMFPACK_LIBRAY} PARENT_SCOPE)
