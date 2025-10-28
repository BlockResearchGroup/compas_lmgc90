
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.0)

project(LMGC90v2_dev Fortran C CXX)

SET(LMGC90_MAJOR_VERSION 2)
SET(LMGC90_MINOR_VERSION 0)
SET(LMGC90_PATCH_VERSION 0)

MESSAGE(STATUS "You try to install LMGC90v2......")
MESSAGE(STATUS "For every problem with cmake installation send an e-mail: cyril.bordreuil@univ-montp2.fr")
MESSAGE(STATUS "")

OPTION(WITH_MATLIB  "With matlib for external models"        OFF)
OPTION(WITH_VTK     "With vtk bindings for visualisation"    ON)
OPTION(WITH_NEW     "With new simulaiton module of Core "    OFF)
OPTION(BUILD_ChiPy  "Compile les modules python         "    ON)
OPTION(BUILD_CHIC   "Compile Chic                       "    OFF)
OPTION(BUILD_PRE    "Compile some pre-processing functions " ON)

# Quelques messages de precautions
MESSAGE(STATUS "BindingExternalLinearAlgebra  is assumed to be at the same level as dev...")

if(WITH_MATLIB)
	MESSAGE(STATUS " BindingExternalModels  is assumed to be at the same level as LMGC90v2_dev...")
endif(WITH_MATLIB)


# Recherche du package ou sont specifier les options de compilation pour LMGC90
# Variable pour specifier ou se situe les findXXX.cmake pour les packages
SET(CMAKE_MODULE_PATH  ${LMGC90v2_SOURCE_DIR}/tools/cmake/modules ${CMAKE_MODULE_PATH})

#Recherche de LAPACK
FIND_PACKAGE(LAPACK_FOR_LMGC REQUIRED)

# Quelques messages pour l'endroit des bibliothèques
MESSAGE(STATUS "")
MESSAGE(STATUS  "-----External Dependencies-----")

MESSAGE(STATUS "BLAS      Library used         : " ${BLAS_LIBRARY})
MESSAGE(STATUS "LAPACK    Library used         : " ${LAPACK_LIBRARY})
MESSAGE(STATUS "LAPACK 95 Include directory    : " ${LAPACK95_INCLUDE_DIR})
MESSAGE(STATUS "LAPACK 95 Library used         : " ${LAPACK95_LIBRARY})
MESSAGE(STATUS "LAPACK    Library dir          : " ${LAPACK_LIBRARY_DIR})

if(WITH_MATLIB)
	FIND_PACKAGE(MatLib_FOR_LMGC REQUIRED)
	MESSAGE(STATUS "MatLib    Library used         : " ${MATLIB_LIBRARY})
	MESSAGE(STATUS "Matlib binding   found         : " ${MATLIB_F90_MODULE})
endif(WITH_MATLIB)

MESSAGE(STATUS "-------End of External Dependencies----")
MESSAGE(STATUS "")


# Quelques indications pour specifier ou on se trouve
MESSAGE(STATUS "Libraries will be located in   : "  ${CMAKE_BINARY_DIR}/lib)
SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY                  ${CMAKE_BINARY_DIR}/lib)
SET(LIBRARY_OUTPUT_PATH                             ${CMAKE_BINARY_DIR}/lib)
MESSAGE(STATUS "Executables will be located in : "  ${CMAKE_BINARY_DIR}/bin)
SET(EXECUTABLE_OUTPUT_PATH                          ${CMAKE_BINARY_DIR}/bin)
#SET(CMAKE_Fortran_MODULE_DIRECTORY                  ${CMAKE_BINARY_DIR}/modules)
MESSAGE(STATUS "")

# Quelques variables globales pour retrouver les modules
SET(LMGC90_CORE_SHARED_DIR     ${CMAKE_BINARY_DIR}/Core/src/shared)
SET(LMGC90_BINDINGS_DIR        ${CMAKE_BINARY_DIR}/Bindings)
SET(LMGC90_CORE_RIGID2D_DIR    ${CMAKE_BINARY_DIR}/Core/src/rigid_2D)
SET(LMGC90_CORE_CONTACT2D_DIR  ${CMAKE_BINARY_DIR}/Core/src/contact_2D)
SET(LMGC90_CORE_RIGID3D_DIR    ${CMAKE_BINARY_DIR}/Core/src/rigid_3D)
SET(LMGC90_CORE_CONTACT3D_DIR  ${CMAKE_BINARY_DIR}/Core/src/contact_3D)
SET(LMGC90_CORE_MAILx_DIR      ${CMAKE_BINARY_DIR}/Core/src/mailx)
SET(LMGC90_CORE_KERNEL_DIR     ${CMAKE_BINARY_DIR}/Core/src/kernel)
SET(LMGC90_CORE_POST_DIR       ${CMAKE_BINARY_DIR}/Core/src/post)
SET(LMGC90_CORE_GMVWRITE_DIR   ${CMAKE_BINARY_DIR}/Core/src/post/gmvwrite)

if(BUILD_PRE)
SET(LMGC90_CORE_PRE_TOOLS_DIR  ${CMAKE_BINARY_DIR}/Core/src/pre_tools)
endif(BUILD_PRE)

#SET(CMAKE_OSX_ARCHITECTURES   "ppc;i386;ppc64;x86_64")
if(WITH_NEW)
  SET(LMGC90_CORE_SIMULATION_DIR ${CMAKE_BINARY_DIR}/Core/src/simulation)
  SET(LMGC90_CORE_MELIMELO_DIR   ${CMAKE_BINARY_DIR}/Core/src/melimelo)
  SET(LMGC90_CORE_IO_DIR         ${CMAKE_BINARY_DIR}/Core/src/IO)
endif(WITH_NEW)

# On va se balader dans l arborescence pour creer les makefiles
ADD_SUBDIRECTORY(Bindings)
ADD_SUBDIRECTORY(Core)

if(${BUILD_CHIC})
  ADD_SUBDIRECTORY(Chic)
endif(${BUILD_CHIC})

if(${BUILD_ChiPy})
  ADD_SUBDIRECTORY(ChiPy)
endif(${BUILD_ChiPy})

if(${BUILD_PRE})
  ADD_SUBDIRECTORY(Pre)
endif(${BUILD_PRE})

ADD_SUBDIRECTORY(tests)

install(EXPORT lmgc90v2-targets DESTINATION lib/lmgc90v2)

include(InstallRequiredSystemLibraries)

set(CPACK_RESOURCE_FILE_WELCOME ${PROJECT_SOURCE_DIR}/README)
set(CPACK_RESOURCE_FILE_LICENSE ${PROJECT_SOURCE_DIR}/Licence_CeCILL_V1.1-US.txt)
set(CPACK_PACKAGE_NAME "LMGC90v2")
set(CPACK_PACKAGE_VENDOR "LMGC/UM2/UMR5508")
set(CPACK_PACKAGE_VERSION_MAJOR ${LMGC90_MAJOR_VERSION})
set(CPACK_PACKAGE_VERSION_MINOR ${LMGC90_MINOR_VERSION})
set(CPACK_PACKAGE_VERSION_PATCH ${LMGC90_PATCH_VERSION})

include(CPack)

