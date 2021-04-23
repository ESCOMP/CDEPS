project(dlnd Fortran)
set(SRCFILES  lnd_comp_nuopc.F90)

foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.dlnd/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.dlnd/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.dlnd")
  endif()
endforeach()

message("DLND srcfiles are ${SRCFILES}")

add_library(dlnd ${SRCFILES})

add_dependencies(dlnd dshr streams)
target_include_directories (dlnd PRIVATE ${ESMF_F90COMPILEPATHS})
target_include_directories (dlnd PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/../dshr")
target_include_directories (dlnd PRIVATE "${CMAKE_SOURCE_DIR}")
target_include_directories (dlnd PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/../share")
target_include_directories (dlnd PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/../streams")
target_include_directories (dlnd PRIVATE "${PIO_Fortran_INCLUDE_DIR}")
target_include_directories (dlnd PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../build/_deps/fox-src/include)
