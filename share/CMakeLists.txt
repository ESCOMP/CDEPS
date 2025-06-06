project(CDEPS_share Fortran)
include(ExternalProject)
set (GenF90_SRCS shr_infnan_mod.F90
                 shr_frz_mod.F90
                 shr_assert_mod.F90)

add_library(cdeps_share  ${GenF90_SRCS}
                         shr_nl_mod.F90
                         glc_elevclass_mod.F90
                         shr_timer_mod.F90
                         shr_cal_mod.F90
                         shr_kind_mod.F90
                         shr_file_mod.F90
                         shr_sys_mod.F90
                         shr_abort_mod.F90
                         shr_const_mod.F90
                         shr_orb_mod.F90
                         shr_log_mod.F90
                         shr_strconvert_mod.F90
                         shr_precip_mod.F90
                         shr_string_mod.F90
                         shr_is_restart_fh_mod.F90
                         nuopc_shr_methods.F90)

target_include_directories (cdeps_share PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${ESMF_F90COMPILEPATHS} ${PIO_Fortran_INCLUDE_DIRS})

install(TARGETS cdeps_share
        LIBRARY DESTINATION lib)
#===== genf90 =====
if(IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/genf90")
  message(STATUS "genf90 library is already checked out!")
  message(STATUS "genf90 source dir: ${CMAKE_CURRENT_SOURCE_DIR}/genf90")
  set(GENF90_PATH ${CMAKE_CURRENT_SOURCE_DIR}/genf90)
endif()
if (DEFINED GENF90_PATH)
  add_custom_target(genf90
    DEPENDS ${GENF90_PATH}/genf90.pl)
else ()
  ExternalProject_Add (genf90
    PREFIX ${CMAKE_CURRENT_BINARY_DIR}/genf90
    GIT_REPOSITORY https://github.com/PARALLELIO/genf90
    GIT_TAG genf90_200608
    UPDATE_COMMAND ""
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND "")
  ExternalProject_Get_Property (genf90 SOURCE_DIR)
  set (GENF90_PATH ${SOURCE_DIR})
  unset (SOURCE_DIR)
endif ()
add_dependencies (cdeps_share genf90)

#===== Fortran Source Generation with GenF90 =====
foreach (SRC_FILE IN LISTS GenF90_SRCS)
  add_custom_command (OUTPUT ${SRC_FILE}
    COMMAND ${GENF90_PATH}/genf90.pl
    ${CMAKE_CURRENT_SOURCE_DIR}/${SRC_FILE}.in > ${SRC_FILE}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${SRC_FILE}.in genf90)
endforeach ()
