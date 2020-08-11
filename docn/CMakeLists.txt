project(docn Fortran)
set(SRCFILES ocn_comp_nuopc.F90
   	     docn_datamode_copyall_mod.F90
	     docn_datamode_som_mod.F90
	     docn_datamode_aquaplanet_mod.F90
	     docn_datamode_iaf_mod.F90)

foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.docn/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.docn/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.docn")
  endif()
endforeach()

add_library(docn ${SRCFILES})


target_include_directories (docn PUBLIC ${ESMF_F90COMPILEPATHS})
target_include_directories (docn PUBLIC "${CMAKE_BINARY_DIR}/dshr")
target_include_directories (docn PUBLIC "${CMAKE_SOURCE_DIR}")
target_include_directories (docn PUBLIC "${CMAKE_BINARY_DIR}/share")
target_include_directories (docn PUBLIC "${CMAKE_BINARY_DIR}/streams")
target_include_directories (docn PUBLIC "${PIO_Fortran_INCLUDE_DIR}")
