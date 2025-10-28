
# just run tests...
set( ref_path "NO_COMP" )

set(test_name 2Cubes_standard)
createTest( ${dir_name}_${test_name} "all;quick" gen_sample.py command.py ${ref_path} )

set(test_name 2Cubes_SN_globalac)
createTest( ${dir_name}_${test_name} "all;quick" gen_sample.py command_SN_globalac.py ${ref_path} )

set(test_name 2Cubes_SN_globaladmm)
createTest( ${dir_name}_${test_name} "all;quick" gen_sample.py command_SN_globaladmm.py ${ref_path} )

set(test_name 2Cubes_SN_nlgs)
createTest( ${dir_name}_${test_name} "all;quick" gen_sample.py command_SN_nlgs.py ${ref_path} )

set(test_name 2Cubes_SN_nsgs)
createTest( ${dir_name}_${test_name} "all;quick" gen_sample.py command_SN_nsgs.py ${ref_path} )
