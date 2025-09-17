
#add_executable(ann_test ann_test.cpp rand.cpp)
#target_link_libraries(ann_test ann)

#for this to work, should copy test files input/ouptut and check
#add_test(ann_test ${EXECUTABLE_OUTPUT_PATH}/ann_test))

if(${BUILD_ANN_BINDING})
  add_test(NAME ann_python
           COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_pair.py
          )
  set_property(TEST ann_python
               PROPERTY ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}:${PYTHON_VENV_PATH}
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
               PROPERTY ENVIRONMENT OPENBLAS_NUM_THREADS=1
              )
endif(${BUILD_ANN_BINDING})

