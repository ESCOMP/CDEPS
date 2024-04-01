project(dice Fortran)
set(SRCFILES ice_comp_nuopc.F90
             dice_datamode_ssmi_mod.F90
	     dice_flux_atmice_mod.F90)

foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.dice/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.dice/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.dice")
  endif()
endforeach()

message("DICE srcfiles are ${SRCFILES}")

add_library(dice ${SRCFILES})

add_dependencies(dice dshr streams)
target_include_directories (dice PRIVATE ${ESMF_F90COMPILEPATHS})
target_include_directories (dice PRIVATE "${CMAKE_SOURCE_DIR}")
target_include_directories (dice PRIVATE "${PIO_Fortran_INCLUDE_DIR}")
if(NOT DISABLE_FoX)
  target_include_directories (dice PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/../fox/include)
endif()

if(BLD_STANDALONE)
  # ESMX requires mod files
  foreach (SRC ${SRCFILES})
    string(REGEX REPLACE "[.]F90$" ".mod" MOD ${SRC})
    if (NOT DEFINED CIMEROOT AND MOD STREQUAL ice_comp_nuopc.mod)
      set(MOD cdeps_dice_comp.mod)
    endif()
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${MOD}" DESTINATION include)
  endforeach ()
endif()
