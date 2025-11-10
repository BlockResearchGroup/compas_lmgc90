
# create two tests from name, label, scripts and reference directory
# one to compute and save results, one to compute and compare results.
# if there is not reference directory... then a single test is created
# to only run the input script without doing any test...
function( createTest test_name label gen_script com_script ref_dir)

  #set(COMMAND_ARGS ${ARGN})

  if( NOT gen_script STREQUAL "" )
    file(COPY ${gen_script}
         DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
        )
    set(gen_script ${CMAKE_CURRENT_BINARY_DIR}/${gen_script})
  endif( NOT gen_script STREQUAL "" )

  file(COPY ${com_script}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
      )
  set(com_script ${CMAKE_CURRENT_BINARY_DIR}/${com_script})

  # if ref_dir does not exists
  if( ${ref_dir} STREQUAL "NO_COMP" )

    add_test(NAME    ${test_name}
             COMMAND ${CMAKE_SOURCE_DIR}/ci_scripts/just_run_it.sh
                     ${Python_EXECUTABLE}
                     ${test_name}
                     ${gen_script}
                     ${com_script}
             CONFIGURATIONS "just_run"
            )
    set_tests_properties(${test_name} PROPERTIES
                         LABELS "${label}"
                         ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}
                        )

  else( ${ref_dir} STREQUAL "NO_COMP" )

    add_test(NAME    ${test_name}
             COMMAND ${CMAKE_SOURCE_DIR}/ci_scripts/run_test.sh
                     ${Python_EXECUTABLE}
                     ${test_name}
                     ${gen_script}
                     ${com_script}
                     ${NON_REGRESSION_BASE}/${ref_dir}
             CONFIGURATIONS "non_reg"
            )
    set_tests_properties(${test_name} PROPERTIES
                         LABELS "${label}"
                         ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}
                        )
    set_property(TEST ${test_name}
                 APPEND PROPERTY
                 DEPENDS ${NON_REGRESSION_BASE}
                )


    add_test(NAME    save_${test_name}
             COMMAND ${CMAKE_SOURCE_DIR}/ci_scripts/save_test.sh
                     ${Python_EXECUTABLE}
                     ${test_name}
                     ${gen_script}
                     ${com_script}
                     ${SAVE_REGRESSION_BASE}/${ref_dir}
             CONFIGURATIONS "save_reg"
            )
    set_tests_properties(save_${test_name} PROPERTIES
                         LABELS "${label}"
                         ENVIRONMENT PYTHONPATH=${CMAKE_BINARY_DIR}:$ENV{PYTHONPATH}
                        )
    set_property(TEST save_${test_name}
                 APPEND PROPERTY
                 DEPENDS ${SAVE_REGRESSION_BASE}
                )

  endif( ${ref_dir} STREQUAL "NO_COMP" )

endfunction()
