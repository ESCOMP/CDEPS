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
target_include_directories (dwav PRIVATE ${ESMF_F90COMPILEPATHS})
target_include_directories (dwav PRIVATE "${CMAKE_SOURCE_DIR}")
target_include_directories (dwav PRIVATE "${PIO_Fortran_INCLUDE_DIR}")
if(NOT DISABLE_FoX)
target_include_directories (dwav PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../fox/include)
endif()

if(BLD_STANDALONE)
  # ESMX requires mod files
  foreach (SRC ${SRCFILES})
    string(REGEX REPLACE "[.]F90$" ".mod" MOD ${SRC})
    if (NOT DEFINED CIMEROOT AND MOD STREQUAL wav_comp_nuopc.mod)
      set(MOD cdeps_dwav_comp.mod)
    endif()
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${MOD}" DESTINATION include)
  endforeach ()
endif()
