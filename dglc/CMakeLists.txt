project(dglc Fortran)
set(SRCFILES glc_comp_nuopc.F90
             dglc_datamode_noevolve_mod.F90)

foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.dglc/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.dglc/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.dglc")
  endif()
endforeach()

message("DGLC srcfiles are ${SRCFILES}")

add_library(dglc ${SRCFILES})

add_dependencies(dglc dshr streams)
target_include_directories (dglc PRIVATE ${ESMF_F90COMPILEPATHS})
target_include_directories (dglc PRIVATE ${CMAKE_SOURCE_DIR})
target_include_directories (dglc PRIVATE ${PIO_Fortran_INCLUDE_DIR})
if(NOT DISABLE_FoX)
  target_include_directories (dglc PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../fox/include)
endif()

if(BLD_STANDALONE)
  # ESMX requires mod files
  foreach (SRC ${SRCFILES})
    string(REGEX REPLACE "[.]F90$" ".mod" MOD ${SRC})
    if (NOT DEFINED CIMEROOT AND MOD STREQUAL glc_comp_nuopc.mod)
      set(MOD cdeps_dglc_comp.mod)
    endif()
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${MOD}" DESTINATION include)
  endforeach ()
endif()
