project(dice Fortran)

add_library(dice ice_comp_nuopc.F90
		 dice_datamode_ssmi_mod.F90
		 dice_flux_atmice_mod.F90)

target_include_directories (dice PUBLIC ${ESMF_F90COMPILEPATHS})
target_include_directories (dice PUBLIC "${CMAKE_BINARY_DIR}/dshr")
target_include_directories (dice PUBLIC "${CMAKE_SOURCE_DIR}")
target_include_directories (dice PUBLIC "${CMAKE_BINARY_DIR}/share")
target_include_directories (dice PUBLIC "${CMAKE_BINARY_DIR}/streams")
target_include_directories (dice PUBLIC "${PIO_INCDIR}")
