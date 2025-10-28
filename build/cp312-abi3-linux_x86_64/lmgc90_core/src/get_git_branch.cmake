
execute_process( COMMAND /usr/bin/git rev-parse --abbrev-ref HEAD
                 WORKING_DIRECTORY /home/pv/brg/code_fortran/compas_lmgc90/external/lmgc90_dev-fortran_lib
                 OUTPUT_VARIABLE GIT_CURRENT_BRANCH
                 OUTPUT_STRIP_TRAILING_WHITESPACE
               )
message(STATUS "git branch oneapi | ${GIT_CURRENT_BRANCH}")
if( NOT "oneapi" STREQUAL "${GIT_CURRENT_BRANCH}" )
  message(FATAL_ERROR "git branch changed... please rerun cmake to confirm")
endif( NOT "oneapi" STREQUAL "${GIT_CURRENT_BRANCH}" )

