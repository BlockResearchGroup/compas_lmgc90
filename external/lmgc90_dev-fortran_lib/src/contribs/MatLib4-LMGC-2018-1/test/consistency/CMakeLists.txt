################################################################################
#  $Id: CMakeLists.txt 169 2015-08-10 09:34:30Z lstainier $
################################################################################
#  This file is part of ZorgLib, a computational simulation framework          #
#  for thermomechanics of solids and structures (systems in general).          #
#                                                                              #
#  Copyright (c) 2001-2015, L. Stainier.                                       #
#  See file LICENSE.txt for license information.                               #
#  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.     #
################################################################################

# create executable
add_executable(MatlConsistencyTest MatlConsistencyTest.cpp)
target_link_libraries( MatlConsistencyTest matlib ${M_LIB} )

# add tests

# elasticity

add_test( NAME ConsistencyTest-isotropic_elasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-isoelasticity.dat )
add_test( NAME ConsistencyTest-orthotropic_elasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-orthotropicelasticity.dat )

# elasticity (finite strains - hyperelasticity)

add_test( NAME ConsistencyTest-isotropic_hyperelasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-isohyperelasticity.dat )
add_test( NAME ConsistencyTest-neohookean_hyperelasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-neohookean.dat )

# elasticity (finite strains - hypoelasticity)

add_test( NAME ConsistencyTest-isotropic_hypoelasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-isohypoelasticity.dat )

# plasticity (small strains)

add_test( NAME ConsistencyTest-linear_isotropic_hardening_J2_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-linisoj2plasticity.dat )
add_test( NAME ConsistencyTest-nonlinear_isotropic_hardening_J2_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-nonlinisoj2plasticity.dat )
add_test( NAME ConsistencyTest-NortonHoff_isotropic_hardening_J2_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-nhisoj2plasticity.dat )

# thermo-plasticity (small strains)

add_test( NAME ConsistencyTest-linear_isotropic_hardening_J2_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-linisoj2thermoplasticity.dat )
add_test( NAME ConsistencyTest-coupled_linear_isotropic_hardening_J2_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-cpldlinisoj2thermoplasticity.dat )
add_test( NAME ConsistencyTest-JohnsonCook_J2_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-jcj2thermoplasticity.dat )
add_test( NAME ConsistencyTest-adiabatic_JohnsonCook_J2_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-adiabjcj2thermoplasticity.dat )
add_test( NAME ConsistencyTest-coupled_JohnsonCook_J2_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-cpldjcj2thermoplasticity.dat )

# thermo-plasticity (finite strains)

add_test( NAME ConsistencyTest-linear_isotropic_hardening_J2_finite_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-linisoj2thermoheplasticity.dat )
add_test( NAME ConsistencyTest-adiabatic_linear_isotropic_hardening_J2_finite_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-adiablinisoj2thermoheplasticity.dat )
add_test( NAME ConsistencyTest-coupled_linear_isotropic_hardening_J2_finite_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-cpldlinisoj2thermoheplasticity.dat )
add_test( NAME ConsistencyTest-adiabatic_nonlinear_isotropic_hardening_J2_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-adiabnonlinisoj2thermoheplasticity.dat )
add_test( NAME ConsistencyTest-JohnsonCook_J2_finite_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-jcj2thermoheplasticity.dat )
add_test( NAME ConsistencyTest-adiabatic_JohnsonCook_J2_finite_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-adiabjcj2thermoheplasticity.dat )
add_test( NAME ConsistencyTest-coupled_JohnsonCook_J2_finite_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-cpldjcj2thermoheplasticity.dat )

# chemo-elasticity

add_test( NAME ConsistencyTest-isotropic_chemo_elasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-isochemoelasticity.dat )
