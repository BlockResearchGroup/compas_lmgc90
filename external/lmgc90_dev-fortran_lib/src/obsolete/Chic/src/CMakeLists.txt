# On entre dans la version Chic de LMGC90
SET(LMGC90_CHIC_SHARED_DIR    ${CMAKE_BINARY_DIR}/Chic/src/shared)
SET(LMGC90_CHIC_RIGID2D_DIR   ${CMAKE_BINARY_DIR}/Chic/src/rigid_2D)
SET(LMGC90_CHIC_RIGID3D_DIR   ${CMAKE_BINARY_DIR}/Chic/src/rigid_3D)
SET(LMGC90_CHIC_MAILX_DIR     ${CMAKE_BINARY_DIR}/Chic/src/mailx)
SET(LMGC90_CHIC_CONTACT2D_DIR ${CMAKE_BINARY_DIR}/Chic/src/contact_2D)
SET(LMGC90_CHIC_CONTACT3D_DIR ${CMAKE_BINARY_DIR}/Chic/src/contact_3D)
SET(LMGC90_CHIC_POST_DIR      ${CMAKE_BINARY_DIR}/Chic/src/post)
SET(LMGC90_CHIC_KERNEL_DIR    ${CMAKE_BINARY_DIR}/Chic/src/kernel)


ADD_SUBDIRECTORY(shared)
ADD_SUBDIRECTORY(rigid_2D)
ADD_SUBDIRECTORY(rigid_3D)
ADD_SUBDIRECTORY(mailx)
ADD_SUBDIRECTORY(contact_2D)
ADD_SUBDIRECTORY(contact_3D)
ADD_SUBDIRECTORY(post)
ADD_SUBDIRECTORY(kernel)

INCLUDE_DIRECTORIES(${LMGC90_CHIC_SHARED_DIR}
	            ${LMGC90_CORE_SHARED_DIR}
		    ${LMGC90_CHIC_KERNEL_DIR}
		    ${LMGC90_CORE_KERNEL_DIR}
		    ${LMGC90_CHIC_MAILX_DIR}
		    ${LMGC90_CORE_MAILX_DIR}
		    ${LMGC90_CORE_CONTACT2D_DIR}
		    ${LMGC90_CHIC_CONTACT2D_DIR}
		    ${LMGC90_CORE_CONTACT3D_DIR}
		    ${LMGC90_CHIC_CONTACT3D_DIR}
		    ${LMGC90_CORE_RIGID2D_DIR}
		    ${LMGC90_CHIC_RIGID2D_DIR}
		    ${LMGC90_CORE_RIGID3D_DIR}
		    ${LMGC90_CHIC_RIGID3D_DIR}
		    ${LMGC90_CORE_POST_DIR}
		    ${LMGC90_CORE_GMVWRITE_DIR}
		    ${LMGC90_CHIC_POST_DIR})
#INCLUDE_DIRECTORIES(${CMAKE_Fortran_MODULE_DIRECTORY})

# Toujours pas homogene pour LAPACK, on regarderea quand on mettra la MKL
LINK_DIRECTORIES(${LAPACK_LIBRARY_DIR} ${LAPACK95_LIBRARY_DIR} ${BLAS_LIBRARY_DIR}
		${CMAKE_BINARY_DIR}/Core/src/post/gmvwrite
		${CMAKE_BINARY_DIR}/lib)

# Creation de l executable lmgc90_2D


ADD_EXECUTABLE(lmgc90_2D JMNR.f90)
SET(LMGC90_2D_TARGET_LINK_LIBRARIES
                                lmgc_bindings 
                                lmgc_core_gmvwrite
				lmgc_chic_mailx 
				lmgc_core_mailx
				lmgc_chic_shared 
				lmgc_core_shared
				lmgc_core_rigid_2d 
				lmgc_chic_rigid_2d
				lmgc_core_contact_2d 
				lmgc_chic_contact_2d
				lmgc_core_post_2d
				lmgc_chic_post_2d
				lmgc_core_kernel_2d
				lmgc_chic_kernel_2d)
# fd pas necessaire				lmgc_core_gmshwrite)

# Si on veut que lmgc90_2D soit lie avec la matlib
if(WITH_MATLIB)
	SET(LMGC90_2D_TARGET_LINK_LIBRARIES ${LMGC90_2D_TARGET_LINK_LIBRARIES} lmgc_external_models  ${MATLIB_LIBRARY} ${STDC++_LIB}  )

endif(WITH_MATLIB)

# On fait l'edition de liens
TARGET_LINK_LIBRARIES(lmgc90_2D ${LMGC90_2D_TARGET_LINK_LIBRARIES}
				lapack95
				${LAPACK_LIB}
				${BLAS_LIB})


LINK_DIRECTORIES(${LAPACK95_LIBRARY_DIR} ${LAPACK_LIBRARY_DIR} ${BLAS_LIBRARY_DIR})
# Creation de l executable 3D
ADD_EXECUTABLE(lmgc90_3D JMNR_3D.f90)


SET(LMGC90_3D_TARGET_LINK_LIBRARIES lmgc_core_contact_3d
				lmgc_chic_mailx 
				lmgc_core_mailx
				lmgc_chic_shared 
				lmgc_core_shared
				lmgc_core_rigid_3d 
				lmgc_chic_rigid_3d
				lmgc_chic_contact_3d
				lmgc_core_post_3d 
				lmgc_chic_post_3d
				lmgc_core_kernel_3d 
				lmgc_chic_kernel_3d
				lmgc_bindings 
				lmgc_core_gmvwrite)

# Avec la matlib
if(WITH_MATLIB)
	SET(LMGC90_3D_TARGET_LINK_LIBRARIES ${LMGC90_3D_TARGET_LINK_LIBRARIES}  lmgc_external_models ${MATLIB_LIBRARY} ${STDC++_LIB})
        
endif(WITH_MATLIB)

# on lie
TARGET_LINK_LIBRARIES(lmgc90_3D ${LMGC90_3D_TARGET_LINK_LIBRARIES}
				${LAPACK95_LIBRARY}
				${LAPACK_LIBRARY}
				${BLAS_LIBRARY})

install(TARGETS lmgc90_2D lmgc90_3D 
        DESTINATION bin
	COMPONENT Runtime
	EXPORT lmgc90v2-targets)
