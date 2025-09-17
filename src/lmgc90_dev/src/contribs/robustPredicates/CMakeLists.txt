
if( ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  add_definitions(-DLINUX)
endif( ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
add_library(predicates predicates.c)

