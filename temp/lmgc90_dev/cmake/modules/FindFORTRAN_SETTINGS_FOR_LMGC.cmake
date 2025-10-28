#if(${UNIX})
#	MESSAGE(STATUS " You are working on UNIX.")
#endif(${UNIX})

if(${APPLE})
	#MESSAGE(STATUS " You are working on APPLE.")
	if(${CMAKE_Fortran_COMPILER} MATCHES g95)
	     MESSAGE(ERROR " This will not compile due to error in cmake")
	endif(${CMAKE_Fortran_COMPILER} MATCHES g95)
endif(${APPLE})

