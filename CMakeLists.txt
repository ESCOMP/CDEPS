cmake_minimum_required(VERSION 3.10)

project(NUOPC_DATA_MODELS LANGUAGES Fortran VERSION 0.1)
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)
include(ExternalProject)
find_package(ESMF REQUIRED)
find_package(PIO REQUIRED)
find_package(FoX REQUIRED)

if (DEFINED CIMEROOT)
  message("Using CIME in ${CIMEROOT} with compiler ${COMPILER}")
  include(${CASEROOT}/Macros.cmake)
  if (${PIO_VERSION} LESS 2)
    message( FATAL_ERROR "Version 2 of the PIO library required")
  endif()
  set(FOX ${LIBROOT})
  set(CMAKE_Fortran_FLAGS "${FFLAGS} -I${LIBROOT}/include -I${LIBROOT}/finclude -I${LIBROOT}/nuopc/esmf/${NINST_VALUE}/include")
else ()
  set(BLD_STANDALONE TRUE)
endif ()
if (NOT DEFINED MPILIB OR NOT ${MPILIB} STREQUAL "mpi-serial")
  find_package(MPI REQUIRED)
endif()

if(BLD_STANDALONE)
  add_subdirectory(share)
  list(APPEND EXTRA_LIBS cdeps_share)
  list(APPEND EXTRA_INCLUDES "${PIO_INCDIR}" "${CMAKE_BINARY_DIR}/share" )
endif()


add_subdirectory(streams)
add_subdirectory(dshr)

foreach(COMP datm dice dlnd docn drof dwav)
  add_subdirectory("${COMP}")
endforeach(COMP)

install(TARGETS streams dshr
        LIBRARY DESTINATION lib)
