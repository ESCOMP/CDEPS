project(dice Fortran)
set(SRCFILES ice_comp_nuopc.F90
             dice_datamode_ssmi_mod.F90
	     dice_flux_atmice_mod.F90)

foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.dice/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.dice/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.dice")
  endif()
endforeach()
add_library(dice ${SRCFILES})

target_include_directories (dice PUBLIC ${ESMF_F90COMPILEPATHS})
target_include_directories (dice PUBLIC "${CMAKE_BINARY_DIR}/dshr")
target_include_directories (dice PUBLIC "${CMAKE_SOURCE_DIR}")
target_include_directories (dice PUBLIC "${CMAKE_BINARY_DIR}/share")
target_include_directories (dice PUBLIC "${CMAKE_BINARY_DIR}/streams")
target_include_directories (dice PUBLIC "${PIO_Fortran_INCLUDE_DIR}")
