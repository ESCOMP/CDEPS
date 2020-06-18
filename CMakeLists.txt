cmake_minimum_required(VERSION 3.10)

project(NUOPC_DATA_MODELS LANGUAGES Fortran VERSION 0.1)
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)
include(ExternalProject)

find_package(MPI REQUIRED)
find_package(ESMF REQUIRED)

if (DEFINED CIMEROOT)
  message("Using CIME in ${CIMEROOT} with compiler ${COMPILER}")
  include(${CASEROOT}/Macros.cmake)
  if (${PIO_VERSION} LESS 2)
    message( FATAL_ERROR "Version 2 of the PIO library required")
  endif()
  set(FOX ${LIBROOT})
  set(CMAKE_Fortran_FLAGS "${FFLAGS} -I${LIBROOT}/include -I${LIBROOT}/finclude -I${LIBROOT}/nuopc/esmf/${NINST_VALUE}/include")
else ()
  message("CIMEROOT not found, downloading cime")
  ExternalProject_Add (cime
    PREFIX ${CMAKE_CURRENT_BINARY_DIR}/cime
    GIT_REPOSITORY https://github.com/ESMCI/cime
    GIT_TAG cd1238247e71f0336fd790bc9716ac6f0bf91c21
    GIT_PROGRESS TRUE
    UPDATE_COMMAND ""
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")
  ExternalProject_Get_Property (cime SOURCE_DIR)
  set (CIMEROOT ${SOURCE_DIR})
  unset (SOURCE_DIR)
endif ()
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod_files)

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
