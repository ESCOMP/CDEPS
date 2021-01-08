project(dwav Fortran)
set(SRCFILES wav_comp_nuopc.F90)

foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.dwav/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.dwav/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.dwav")
  endif()
endforeach()

message("DWAV srcfiles are ${SRCFILES}")

add_library(dwav ${SRCFILES})

add_dependencies(dwav dshr streams)
target_include_directories (dwav PUBLIC ${ESMF_F90COMPILEPATHS})
target_include_directories (dwav PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/dshr")
target_include_directories (dwav PUBLIC "${CMAKE_SOURCE_DIR}")
target_include_directories (dwav PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/share")
target_include_directories (dwav PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/streams")
target_include_directories (dwav PUBLIC "${PIO_Fortran_INCLUDE_DIR}")
