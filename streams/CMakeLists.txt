set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod_files)
add_library(streams dshr_methods_mod.F90
                    dshr_strdata_mod.F90
                    dshr_stream_mod.F90
                    dshr_tInterp_mod.F90)
if(BLD_STANDALONE)
  add_dependencies(streams cdeps_share)
endif()
target_include_directories (streams PUBLIC ${ESMF_F90COMPILEPATHS})
target_include_directories (streams PUBLIC ${PIO_INCDIR})
target_include_directories (streams PUBLIC ${FOX}/finclude)
target_include_directories (streams PUBLIC ${CMAKE_BINARY_DIR}/share)
install(TARGETS streams
        LIBRARY DESTINATION lib)
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION include)
