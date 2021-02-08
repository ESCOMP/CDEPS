cmake_minimum_required(VERSION 3.10)
include(ExternalProject)

if (DEFINED CIMEROOT)
  message("Using CIME in ${CIMEROOT} with compiler ${COMPILER}")
  include(${CASEROOT}/Macros.cmake)
  if (${PIO_VERSION} LESS 2)
    message( FATAL_ERROR "Version 2 of the PIO library required")
  endif()
  if (${MPILIB} STREQUAL "mpi-serial")
    set(CMAKE_C_COMPILER ${SCC})
    set(CMAKE_Fortran_COMPILER ${SFC})
    set(CMAKE_CXX_COMPILER ${SCXX})
  else()
    set(CMAKE_C_COMPILER ${MPICC})
    set(CMAKE_Fortran_COMPILER ${MPIFC})
    set(CMAKE_CXX_COMPILER ${MPICXX})
  endif()
  set(CMAKE_Fortran_FLAGS "${FFLAGS} -I${LIBROOT}/include -I${LIBROOT}/finclude -I${LIBROOT}/nuopc/esmf/${NINST_VALUE}/include")
else()
  set(BLD_STANDALONE TRUE)
endif()

project(NUOPC_DATA_MODELS LANGUAGES Fortran VERSION 0.1)

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

message("CMAKE_MODULE_PATH is ${CMAKE_MODULE_PATH}")

if (TARGET esmf)
  message("Target esmf is already found. Skip find_package ...")
else()
  find_package(ESMF REQUIRED)
endif()

if (DEFINED ENV{PIO_ROOT})
  message("PIO_ROOT is $ENV{PIO_ROOT}")
else()
  if (DEFINED PIO)
    set(PIO_PATH ${PIO})
  else()
    set(PIO_PATH $ENV{PIO})
  endif()
  find_package(PIO REQUIRED COMPONENT C Fortran PATH ${PIO_PATH})
endif()

if (NOT DEFINED MPILIB OR NOT ${MPILIB} STREQUAL "mpi-serial")
  find_package(MPI REQUIRED)
endif()

if(BLD_STANDALONE)
  add_subdirectory(share)
  list(APPEND EXTRA_LIBS cdeps_share)
  list(APPEND EXTRA_INCLUDES "${CMAKE_BINARY_DIR}/share" )
endif()

add_subdirectory(fox)
add_subdirectory(streams)
add_subdirectory(dshr)

foreach(COMP datm dice dlnd docn drof dwav)
  add_subdirectory("${COMP}")
endforeach(COMP)


target_include_directories(streams PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/share>
                                          $<INSTALL_INTERFACE:mod>)
target_include_directories(streams PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>
                                          $<INSTALL_INTERFACE:mod>)
target_include_directories(dshr PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/share>
                                       $<INSTALL_INTERFACE:mod>)
target_include_directories(dshr PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>
                                       $<INSTALL_INTERFACE:mod>)
target_include_directories(dshr PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/streams>
                                       $<INSTALL_INTERFACE:mod>)

foreach(COMP datm dice dlnd docn drof dwav)
  target_include_directories(${COMP} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/share>
                                            $<INSTALL_INTERFACE:mod>)
  target_include_directories(${COMP} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/streams>
                                            $<INSTALL_INTERFACE:mod>)
  target_include_directories(${COMP} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/dshr>
                                            $<INSTALL_INTERFACE:mod>)

  install(TARGETS ${COMP} 
          EXPORT  ${COMP}
          LIBRARY DESTINATION lib
          ARCHIVE DESTINATION lib
          COMPONENT Library)
  install(EXPORT      ${COMP} 
          DESTINATION lib/cmake)
endforeach(COMP)

foreach(DEPS streams dshr cdeps_share FoX_dom FoX_wxml FoX_sax FoX_common FoX_utils FoX_fsys)
  if(NOT BLD_STANDALONE AND ${DEPS} STREQUAL "cdeps_share")
     continue()
  endif()
  install(TARGETS ${DEPS} 
          EXPORT  ${DEPS}
          LIBRARY DESTINATION lib
          ARCHIVE DESTINATION lib
          COMPONENT Library)
  install(EXPORT      ${DEPS} 
          DESTINATION lib/cmake)
endforeach(DEPS)
