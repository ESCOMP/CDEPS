project(datm Fortran)

add_library(datm atm_comp_nuopc.F90
		 datm_datamode_clmncep_mod.F90
		 datm_datamode_core2_mod.F90
		 datm_datamode_jra_mod.F90
		 datm_datamode_era5_mod.F90)

add_dependencies(datm dshr streams)
target_include_directories (datm PUBLIC ${ESMF_F90COMPILEPATHS})
target_include_directories (datm PUBLIC "${CMAKE_BINARY_DIR}/dshr")
target_include_directories (datm PUBLIC "${CMAKE_SOURCE_DIR}")
target_include_directories (datm PUBLIC "${CMAKE_BINARY_DIR}/share")
target_include_directories (datm PUBLIC "${CMAKE_BINARY_DIR}/streams")
target_include_directories (datm PUBLIC "${PIO_Fortran_INCLUDE_DIR}")
