
set(dir_name therMAILx_2D)

add_subdirectory(beam)
add_subdirectory(beam_nl)
if(${WITH_MATLIB})
  add_subdirectory(beam_thmec)
  if(${WITH_SPARSE})
    add_subdirectory(cracked_plate)
  endif(${WITH_SPARSE})
endif(${WITH_MATLIB})

