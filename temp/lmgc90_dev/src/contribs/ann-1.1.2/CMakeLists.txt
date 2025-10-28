
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

set(NORM_TYPES ann_eucl ann_manh)

add_subdirectory(src)
if(NOT ${NO_TEST})
add_subdirectory(test)
endif(NOT ${NO_TEST})
#add_subdirectory(sample)
#add_subdirectory(ann2fig)

