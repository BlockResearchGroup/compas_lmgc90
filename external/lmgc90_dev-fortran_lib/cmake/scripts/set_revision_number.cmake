# get the result of svnrevision of ${dir} directory, and configure file
# with ${file_in} to ${file_out}
# file of pylmgc90

# It is not included in a CMakeLists.txt to be able to run
# the command as an input script so that is up to date
# when typing 'make'

# can't include because of some path problem
# within an cmake -S instance...
# so duplicate of get_revision in modules directory
#include(get_revision)
function(get_revision GIT_EXECUTABLE dir git_revision git_current_branch release)

  if(GIT_EXECUTABLE)
    execute_process(COMMAND ${GIT_EXECUTABLE} rev-parse HEAD
                    WORKING_DIRECTORY ${dir}
                    OUTPUT_VARIABLE git_revision
                    ERROR_VARIABLE git_error
                    OUTPUT_STRIP_TRAILING_WHITESPACE
                   )
    if(git_error)
      set(git_revision "unknown")
    endif(git_error)
    execute_process(COMMAND ${GIT_EXECUTABLE} rev-parse --abbrev-ref HEAD
                    WORKING_DIRECTORY ${dir}
                    OUTPUT_VARIABLE git_current_branch
                    ERROR_VARIABLE git_error
                    OUTPUT_STRIP_TRAILING_WHITESPACE
                   )
    if(git_error)
      set(git_current_branch "unknown")
    endif(git_error)
  else(GIT_EXECUTABLE)
    set(git_revision "unknown")
    set(git_current_branch "unknown")
  endif(GIT_EXECUTABLE)

  set(git_revision "${git_revision}" PARENT_SCOPE)
  set(git_current_branch "${git_current_branch}" PARENT_SCOPE)
  set(release "${release}" PARENT_SCOPE)

endfunction()


get_revision(${GIT_EXECUTABLE} ${dir} git_revision git_current_branch ${release})

configure_file(${file_in} ${file_out})

