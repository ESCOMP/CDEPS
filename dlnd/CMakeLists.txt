project(dlnd Fortran)
set(SRCFILES  lnd_comp_nuopc.F90)

foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.dlnd/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.dlnd/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.dlnd")
  endif()
endforeach()

add_library(dlnd ${SRCFILES})

target_include_directories (dlnd PUBLIC ${ESMF_F90COMPILEPATHS})
target_include_directories (dlnd PUBLIC "${CMAKE_BINARY_DIR}/dshr")
target_include_directories (dlnd PUBLIC "${CMAKE_SOURCE_DIR}")
target_include_directories (dlnd PUBLIC "${CMAKE_BINARY_DIR}/share")
target_include_directories (dlnd PUBLIC "${CMAKE_BINARY_DIR}/streams")
target_include_directories (dlnd PUBLIC "${PIO_Fortran_INCLUDE_DIR}")
