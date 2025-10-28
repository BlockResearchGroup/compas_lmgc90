
file(COPY DATBOX
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
     PATTERN .svn EXCLUDE
    )
file(COPY command.py
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

add_test (1024DK_BoxJC ${Python_EXECUTABLE} command.py)
#add_test (1024DK_BoxJC_dum ${Python_EXECUTABLE} command_for_dummies.py)

set_property(TEST 1024DK_BoxJC
             PROPERTY ENVIRONMENT PYTHONPATH=${CHIPY_PATH}
            )

#set_property(TEST 1024DK_BoxJC_dum
#             PROPERTY ENVIRONMENT PYTHONPATH=${CHIPY_PATH}
#            )
