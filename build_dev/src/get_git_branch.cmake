
execute_process( COMMAND /usr/bin/git rev-parse --abbrev-ref HEAD
                 WORKING_DIRECTORY /home/pv/brg/code_fortran/compas_lmgc90/src/lmgc90_dev
                 OUTPUT_VARIABLE GIT_CURRENT_BRANCH
                 OUTPUT_STRIP_TRAILING_WHITESPACE
               )
message(STATUS "git branch oneapi | ${GIT_CURRENT_BRANCH}")
if( NOT "oneapi" STREQUAL "${GIT_CURRENT_BRANCH}" )
  message(FATAL_ERROR "git branch changed... please rerun cmake to confirm")
endif( NOT "oneapi" STREQUAL "${GIT_CURRENT_BRANCH}" )

