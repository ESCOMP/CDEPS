project(dshr Fortran)
set(SRCFILES  dshr_dfield_mod.F90
              dshr_fldlist_mod.F90
              dshr_mod.F90)
foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.cdeps/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.cdeps/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.cdeps")
  endif()
endforeach()
add_library(dshr ${SRCFILES})

if(BLD_STANDALONE)
  add_dependencies(dshr cdeps_share)
endif()
add_dependencies(dshr streams)

target_include_directories (dshr PUBLIC ${ESMF_F90COMPILEPATHS})
target_include_directories (dshr PUBLIC "${CMAKE_BINARY_DIR}/streams")
target_include_directories (dshr PUBLIC "${CMAKE_BINARY_DIR}/share")
target_include_directories (dshr PUBLIC "${PIO_Fortran_INCLUDE_DIR}")
install(TARGETS dshr
        LIBRARY DESTINATION lib)
