project(docn Fortran)

add_library(docn ocn_comp_nuopc.F90
		 docn_datamode_copyall_mod.F90
		 docn_datamode_som_mod.F90
		 docn_datamode_aquaplanet_mod.F90
		 docn_datamode_iaf_mod.F90
		 docn_set_ofrac_mod.F90)


target_include_directories (docn PUBLIC ${ESMF_F90COMPILEPATHS})
target_include_directories (docn PUBLIC "${CMAKE_BINARY_DIR}/dshr")
target_include_directories (docn PUBLIC "${CMAKE_SOURCE_DIR}")
target_include_directories (docn PUBLIC "${CMAKE_BINARY_DIR}/share")
target_include_directories (docn PUBLIC "${CMAKE_BINARY_DIR}/streams")
target_include_directories (docn PUBLIC "${PIO_INCDIR}")
