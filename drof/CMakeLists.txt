project(drof Fortran)
set(SRCFILES  rof_comp_nuopc.F90)

foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.drof/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.drof/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.drof")
  endif()
endforeach()

add_library(drof ${SRCFILES})

target_include_directories (drof PUBLIC ${ESMF_F90COMPILEPATHS})
target_include_directories (drof PUBLIC "${CMAKE_BINARY_DIR}/dshr")
target_include_directories (drof PUBLIC "${CMAKE_SOURCE_DIR}")
target_include_directories (drof PUBLIC "${CMAKE_BINARY_DIR}/share")
target_include_directories (drof PUBLIC "${CMAKE_BINARY_DIR}/streams")
target_include_directories (drof PUBLIC "${PIO_Fortran_INCLUDE_DIR}")
