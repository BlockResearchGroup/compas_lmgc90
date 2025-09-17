
set(test_name CDCD_PerioBox)
createTest( ${dir_name}_${test_name} quick gen.py run.py "examples/${dir_name}/${test_name}"
          )
