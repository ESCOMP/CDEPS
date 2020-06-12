project(drof Fortran)

add_library(drof rof_comp_nuopc.F90)

target_include_directories (drof PUBLIC ${ESMF_F90COMPILEPATHS})
target_include_directories (drof PUBLIC "${CMAKE_BINARY_DIR}/dshr")
target_include_directories (drof PUBLIC "${CMAKE_SOURCE_DIR}")
target_include_directories (drof PUBLIC "${CMAKE_BINARY_DIR}/share")
target_include_directories (drof PUBLIC "${CMAKE_BINARY_DIR}/streams")
target_include_directories (drof PUBLIC "${PIO_INCDIR}")
