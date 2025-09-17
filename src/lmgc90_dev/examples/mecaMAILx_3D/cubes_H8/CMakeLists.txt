
set(test_name cubes_H8)
createTest( ${dir_name}_${test_name} quick gen_sample.py command.py "examples/${dir_name}/${test_name}"  )

# is there a good way to create a second test here ?
# because this generate again DATBOX and should not have its output
# in the same directory, which makes things difficult for the comparison...
#createTest( ${dir_name}_${test_name}_explicit quick gen_sample.py command_explicit.py "examples/${dir_name}/${test_name}"  )
