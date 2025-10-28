
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Makefile.in
               ${CMAKE_CURRENT_BINARY_DIR}/Makefile-sphinx
              )

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/source/conf.py.in
               ${CMAKE_CURRENT_BINARY_DIR}/source/conf.py
              )

file(GLOB_RECURSE sphinx_lmgc90_sources *.rst)

#####################################
### generating pictures with tikz ###
set(tikz_figures  contact_law
                  contactors
                  interactions
                  modelingframework
                 )
set(tikz_png_figures )


foreach(f ${tikz_figures})
  
  set(target_f target_${f})
  add_custom_command(OUTPUT ${target_f}
                     COMMAND pdflatex -output-directory ${CMAKE_CURRENT_BINARY_DIR}/source/figures
                     ${CMAKE_CURRENT_BINARY_DIR}/source/figures/${f}.tex
                     COMMAND convert -density 1000x1000
                     ${CMAKE_CURRENT_BINARY_DIR}/source/figures/${f}.pdf
                     ${CMAKE_CURRENT_BINARY_DIR}/source/figures/${f}.png
                     DEPENDS sphinx_copy_lmgc90_sources
                  )
  set(tikz_png_figures ${tikz_png_figures} ${target_f})
endforeach(f)
### generating pictures with tikz ###
#####################################


add_custom_target(sphinx_copy_lmgc90_sources
                  COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/source
                                                             ${CMAKE_CURRENT_BINARY_DIR}/source
                  DEPENDS ${sphinx_lmgc90_sources}
                 )

# ugly, but necessary to get the documentation
# that really uses the current build and not the
# installed module
add_custom_target(lmgc90_sphinx_docs make -f Makefile-sphinx html;
                  COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_BINARY_DIR}/build/html
                                                             ${CMAKE_BINARY_DIR}/docs
                  DEPENDS sphinx_copy_lmgc90_sources ${tikz_png_figures}
                 )

