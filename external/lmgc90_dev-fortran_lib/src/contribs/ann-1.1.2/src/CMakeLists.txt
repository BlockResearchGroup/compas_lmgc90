
set(ANN_SOURCES ANN.cpp
                brute.cpp
                kd_tree.cpp
                kd_util.cpp
                kd_split.cpp
                kd_dump.cpp
                kd_search.cpp
                kd_fix_rad_search.cpp
                kd_pr_search.cpp
                bd_tree.cpp
                bd_search.cpp
                bd_pr_search.cpp
                bd_fix_rad_search.cpp
                perf.cpp
               )

add_library(ann_euclid ${ANN_SOURCES})
set_target_properties(ann_euclid
                      PROPERTIES COMPILE_FLAGS
                      -DEUCLIDIAN_NORM
                     )

add_library(ann_manhattan ${ANN_SOURCES})
set_target_properties(ann_manhattan 
                      PROPERTIES COMPILE_FLAGS
                      -DMANHATTAN_NORM
                     )

