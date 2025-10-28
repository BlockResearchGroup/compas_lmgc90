
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

