project(drof Fortran)
set(SRCFILES  rof_comp_nuopc.F90)

foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.drof/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.drof/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.drof")
  endif()
endforeach()

message("DROF srcfiles are ${SRCFILES}")

add_library(drof ${SRCFILES})

add_dependencies(drof dshr streams)
target_include_directories (drof PRIVATE ${ESMF_F90COMPILEPATHS})
target_include_directories (drof PRIVATE "${CMAKE_SOURCE_DIR}")
target_include_directories (drof PRIVATE "${PIO_Fortran_INCLUDE_DIR}")
if(NOT DISABLE_FoX)
target_include_directories (drof PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../fox/include)
endif()

if(BLD_STANDALONE)
  # ESMX requires mod files
  foreach (SRC ${SRCFILES})
    string(REGEX REPLACE "[.]F90$" ".mod" MOD ${SRC})
    if (NOT DEFINED CIMEROOT AND MOD STREQUAL rof_comp_nuopc.mod)
      set(MOD cdeps_drof_comp.mod)
    endif()
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${MOD}" DESTINATION include)
  endforeach ()
endif()
