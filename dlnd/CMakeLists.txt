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
target_include_directories (dlnd PRIVATE "${CMAKE_SOURCE_DIR}")
target_include_directories (dlnd PRIVATE "${PIO_Fortran_INCLUDE_DIR}")
if(NOT DISABLE_FoX)
  target_include_directories (dlnd PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../fox/include)
endif()

if(BLD_STANDALONE)
  # ESMX requires mod files
  foreach (SRC ${SRCFILES})
    string(REGEX REPLACE "[.]F90$" ".mod" MOD ${SRC})
    if (NOT DEFINED CIMEROOT AND MOD STREQUAL lnd_comp_nuopc.mod)
      set(MOD cdeps_dlnd_comp.mod)
    endif()
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${MOD}" DESTINATION include)
  endforeach ()
endif()
