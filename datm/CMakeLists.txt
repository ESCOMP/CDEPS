project(datm Fortran)
set(SRCFILES atm_comp_nuopc.F90
             datm_datamode_clmncep_mod.F90
             datm_datamode_cplhist_mod.F90
             datm_datamode_core2_mod.F90
             datm_datamode_jra_mod.F90
             datm_datamode_cfsr_mod.F90
             datm_datamode_era5_mod.F90)

foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.datm/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.datm/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.datm")
  endif()
endforeach()

message("DATM srcfiles are ${SRCFILES}")

add_library(datm ${SRCFILES})

add_dependencies(datm dshr streams)
target_include_directories (datm PRIVATE ${ESMF_F90COMPILEPATHS})
target_include_directories (datm PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/dshr)
target_include_directories (datm PRIVATE ${CMAKE_SOURCE_DIR})
target_include_directories (datm PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/share)
target_include_directories (datm PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/streams)
target_include_directories (datm PRIVATE ${PIO_Fortran_INCLUDE_DIR})
