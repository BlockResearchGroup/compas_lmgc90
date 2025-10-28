
if(EXT_WRAP_USER_SRC)
  set(LMGC90_PYTHON_USER_SRCS ${EXT_WRAP_USER_SRC})
else(EXT_WRAP_USER_SRC)
  set(LMGC90_PYTHON_USER_SRCS wrap_user.f90)
  set(EXT_WRAP_USER_HEADER user/wrap_user.h PARENT_SCOPE)
endif(EXT_WRAP_USER_SRC)

ADD_LIBRARY(lmgc_python_user ${LMGC90_PYTHON_USER_SRCS})
TARGET_LINK_LIBRARIES(lmgc_python_user lmgc_core_shared
                                       lmgc_core_mailx
                     )

SET(_headers ${EXT_WRAP_USER_HEADER})
SET(_HEADERS ${EXT_WRAP_USER_HEADER})

set( headers_list ${headers_list} ${_headers} PARENT_SCOPE)

if(BUILD_C_LIB)
  if(EXT_WRAP_USER_SRC)
    set( _sources ${LMGC90_PYTHON_USER_SRCS})
  else(EXT_WRAP_USER_SRC)
    set( _sources ${CMAKE_CURRENT_SOURCE_DIR}/wrap_user.f90 )
  endif(EXT_WRAP_USER_SRC)
  set( sources_list ${sources_list} ${_sources} PARENT_SCOPE)
  install(FILES ${_HEADERS}
          DESTINATION include/LMGC90/user
         )
endif(BUILD_C_LIB)
