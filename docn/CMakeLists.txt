project(docn Fortran)
set(SRCFILES ocn_comp_nuopc.F90
	     docn_datamode_copyall_mod.F90
	     docn_datamode_som_mod.F90
	     docn_datamode_aquaplanet_mod.F90
	     docn_datamode_iaf_mod.F90
       docn_datamode_cplhist_mod.F90
       docn_import_atmdata_mod.F90)

foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.docn/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.docn/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.docn")
  endif()
endforeach()

message("DOCN srcfiles are ${SRCFILES}")

add_library(docn ${SRCFILES})

add_dependencies(docn dshr streams)
target_include_directories (docn PRIVATE ${ESMF_F90COMPILEPATHS})
target_include_directories (docn PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../dshr)
target_include_directories (docn PRIVATE ${CMAKE_SOURCE_DIR})
target_include_directories (docn PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../share)
target_include_directories (docn PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../streams)
target_include_directories (docn PRIVATE ${PIO_Fortran_INCLUDE_DIR})
target_include_directories (docn PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../fox/include)
