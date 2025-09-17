
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

set(NORM_TYPES ann_eucl ann_manh)

add_subdirectory(src)
if(NOT ${NO_TEST})
add_subdirectory(test)
endif(NOT ${NO_TEST})
#add_subdirectory(sample)
#add_subdirectory(ann2fig)

if(${BUILD_ANN_BINDING})
  add_subdirectory(wrap)
endif(${BUILD_ANN_BINDING})

