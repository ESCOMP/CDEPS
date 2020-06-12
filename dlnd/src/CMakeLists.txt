project(dlnd Fortran)

add_library(dlnd lnd_comp_nuopc.F90)

target_include_directories (dlnd PUBLIC ${ESMF_F90COMPILEPATHS})
target_include_directories (dlnd PUBLIC "${CMAKE_BINARY_DIR}/dshr")
target_include_directories (dlnd PUBLIC "${CMAKE_SOURCE_DIR}")
target_include_directories (dlnd PUBLIC "${CMAKE_BINARY_DIR}/share")
target_include_directories (dlnd PUBLIC "${CMAKE_BINARY_DIR}/streams")
target_include_directories (dlnd PUBLIC "${PIO_INCDIR}")
