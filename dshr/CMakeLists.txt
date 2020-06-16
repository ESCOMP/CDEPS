project(dshr Fortran)

add_library(dshr dshr_dfield_mod.F90
                          dshr_fldlist_mod.F90
                          dshr_mod.F90)

if(BLD_STANDALONE)
  add_dependencies(dshr cdeps_share)
endif()
add_dependencies(dshr streams)

target_include_directories (dshr PUBLIC ${ESMF_F90COMPILEPATHS})
target_include_directories (dshr PUBLIC "${CMAKE_BINARY_DIR}/streams")
target_include_directories (dshr PUBLIC "${CMAKE_BINARY_DIR}/share")
target_include_directories (dshr PUBLIC "${PIO_INCDIR}")
install(TARGETS dshr
        LIBRARY DESTINATION lib)
