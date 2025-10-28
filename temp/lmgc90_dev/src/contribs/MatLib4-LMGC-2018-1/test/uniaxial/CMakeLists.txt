################################################################################
#  $Id: CMakeLists.txt 149 2014-08-24 19:21:50Z lstainier $
################################################################################
#  This file is part of ZorgLib, a computational simulation framework          #
#  for thermomechanics of solids and structures (systems in general).          #
#                                                                              #
#  Copyright (c) 2001-2014, L. Stainier.                                       #
#  See file LICENSE.txt for license information.                               #
#  Please report all bugs and problems to <Laurent.Stainier@ec-nantes.fr>.     #
################################################################################

# create executable
add_executable(MatlUniaxialTest MatlUniaxialTest.cpp)
target_link_libraries( MatlUniaxialTest matlib ${M_LIB} )

# add tests

# elasticity

#add_test( NAME ConsistencyTest-isotropic_elasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
#               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlConsistencyTest test-isoelasticity.dat )

# plasticity (small strains)

add_test( NAME UniaxialTest-linear_isotropic_hardening_J2_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlUniaxialTest uniaxial-linisoj2plasticity.dat )
add_test( NAME UniaxialTest-NortonHoff_isotropic_hardening_J2_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlUniaxialTest uniaxial-nhisoj2plasticity.dat )

# thermo-plasticity (small strains)

add_test( NAME UniaxialTest-Adiabatic_JohnsonCook_J2_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlUniaxialTest uniaxial-adiabjcj2thermoplasticity.dat )

# thermo-plasticity (finite strains)

add_test( NAME UniaxialTest-Adiabatic_JohnsonCook_J2_finite_thermo_plasticity  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
               COMMAND ${EXECUTABLE_OUTPUT_PATH}/MatlUniaxialTest uniaxial-adiabjcj2thermoheplasticity.dat )
