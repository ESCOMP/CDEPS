module datm_pres_aero_mod

  use ESMF             , only : ESMF_SUCCESS, ESMF_State
  use shr_kind_mod     , only : r8=>shr_kind_r8
  use shr_log_mod      , only : shr_log_error
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private ! except

  public :: datm_pres_aero_advertise
  public :: datm_pres_aero_init_pointers
  public :: datm_pres_aero_advance

  ! pointers to export state data
  real(r8), pointer :: Faxa_bcph(:,:)   => null()
  real(r8), pointer :: Faxa_ocph(:,:)   => null()
  real(r8), pointer :: Faxa_dstwet(:,:) => null()
  real(r8), pointer :: Faxa_dstdry(:,:) => null()

  ! pointers to stream data
  real(r8), pointer :: strm_Faxa_bcphidry(:) => null()
  real(r8), pointer :: strm_Faxa_bcphiwet(:) => null()
  real(r8), pointer :: strm_Faxa_bcphodry(:) => null()

  real(r8), pointer :: strm_Faxa_ocphidry(:) => null()
  real(r8), pointer :: strm_Faxa_ocphiwet(:) => null()
  real(r8), pointer :: strm_Faxa_ocphodry(:) => null()

  real(r8), pointer :: strm_Faxa_dstwet1(:)  => null()
  real(r8), pointer :: strm_Faxa_dstwet2(:)  => null()
  real(r8), pointer :: strm_Faxa_dstwet3(:)  => null()
  real(r8), pointer :: strm_Faxa_dstwet4(:)  => null()

  real(r8), pointer :: strm_Faxa_dstdry1(:)  => null()
  real(r8), pointer :: strm_Faxa_dstdry2(:)  => null()
  real(r8), pointer :: strm_Faxa_dstdry3(:)  => null()
  real(r8), pointer :: strm_Faxa_dstdry4(:)  => null()

  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_pres_aero_advertise(fldsExport)

    ! input/output variables
    type(fldlist_type) , pointer :: fldsexport
    !----------------------------------------------------------

    call dshr_fldList_add(fldsExport, 'Faxa_bcph'  , ungridded_lbound=1, ungridded_ubound=3)
    call dshr_fldList_add(fldsExport, 'Faxa_ocph'  , ungridded_lbound=1, ungridded_ubound=3)
    call dshr_fldList_add(fldsExport, 'Faxa_dstwet', ungridded_lbound=1, ungridded_ubound=4)
    call dshr_fldList_add(fldsExport, 'Faxa_dstdry', ungridded_lbound=1, ungridded_ubound=4)

  end subroutine datm_pres_aero_advertise

  !===============================================================================
  subroutine datm_pres_aero_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_pres_aero_init_pointers): '
    !----------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Set module pointers into export state

    call dshr_state_getfldptr(exportState, 'Faxa_bcph', fldptr2=Faxa_bcph, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_ocph', fldptr2=Faxa_ocph, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_dstwet', fldptr2=Faxa_dstwet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Faxa_dstdry', fldptr2=Faxa_dstdry, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Set module pointers into streams and check that they are associated

    call shr_strdata_get_stream_pointer(sdat, 'Faxa_bcphidry' , strm_Faxa_bcphidry, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_bcphodry' , strm_Faxa_bcphodry, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_bcphiwet' , strm_Faxa_bcphiwet, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_ocphidry' , strm_Faxa_ocphidry, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_ocphodry' , strm_Faxa_ocphodry, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_ocphiwet' , strm_Faxa_ocphiwet, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_dstdry1'  , strm_Faxa_dstdry1 , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_dstdry2'  , strm_Faxa_dstdry2 , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_dstdry3'  , strm_Faxa_dstdry3 , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_dstdry4'  , strm_Faxa_dstdry4 , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_dstwet1'  , strm_Faxa_dstwet1 , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_dstwet2'  , strm_Faxa_dstwet2 , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_dstwet3'  , strm_Faxa_dstwet3 , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_dstwet4'  , strm_Faxa_dstwet4 , rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! error check for stream pointers
    if (.not. associated(strm_Faxa_bcphidry)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_bcphidry must be associated if flds_presaero is .true.')
       return
    end if
    if (.not. associated(strm_Faxa_bcphodry)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_bcphodry must be associated if flds_presaero is .true.')
       return
    end if
    if (.not. associated(strm_Faxa_bcphiwet)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_bcphiwet must be associated if flds_presaero is .true.')
       return
    end if
    if (.not. associated(strm_Faxa_ocphidry)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_ocphidry must be associated if flds_presaero is .true.')
       return
    end if
    if (.not. associated(strm_Faxa_ocphodry)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_ocphodry must be associated if flds_presaero is .true.')
       return
    end if
    if (.not. associated(strm_Faxa_ocphiwet)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_ocphiwet must be associated if flds_presaero is .true.')
       return
    end if
    if (.not. associated(strm_Faxa_dstdry1)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_dstdry1 must be associated if flds_presaero is .true.')
       return
    end if
    if (.not. associated(strm_Faxa_dstdry2)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_dstdry2 must be associated if flds_presaero is .true.')
       return
    end if
    if (.not. associated(strm_Faxa_dstdry3)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_dstdry3 must be associated if flds_presaero is .true.')
       return
    end if
    if (.not. associated(strm_Faxa_dstdry4)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_dstdry4 must be associated if flds_presaero is .true.')
       return
    end if
    if (.not. associated(strm_Faxa_dstwet1)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_dstwet1 must be associated if flds_presaero is .true.')
       return
    end if
    if (.not. associated(strm_Faxa_dstwet2)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_dstwet2 must be associated if flds_presaero is .true.')
       return
    end if
    if (.not. associated(strm_Faxa_dstwet3)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_dstwet3 must be associated if flds_presaero is .true.')
       return
    end if
    if (.not. associated(strm_Faxa_dstwet4)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Faxa_dstwet4 must be associated if flds_presaero is .true.')
       return
    end if

  end subroutine datm_pres_aero_init_pointers

  !===============================================================================
  subroutine datm_pres_aero_advance()

    ! The following maps stream input fields to export fields that
    ! have an ungridded dimension

    Faxa_bcph(1,:) = strm_Faxa_bcphidry(:)
    Faxa_bcph(2,:) = strm_Faxa_bcphodry(:)
    Faxa_bcph(3,:) = strm_Faxa_bcphiwet(:)

    Faxa_ocph(1,:) = strm_Faxa_ocphidry(:)
    Faxa_ocph(2,:) = strm_Faxa_ocphodry(:)
    Faxa_ocph(3,:) = strm_Faxa_ocphiwet(:)

    Faxa_dstdry(1,:) = strm_Faxa_dstdry1(:)
    Faxa_dstdry(2,:) = strm_Faxa_dstdry2(:)
    Faxa_dstdry(3,:) = strm_Faxa_dstdry3(:)
    Faxa_dstdry(4,:) = strm_Faxa_dstdry4(:)

    Faxa_dstwet(1,:) = strm_Faxa_dstwet1(:)
    Faxa_dstwet(2,:) = strm_Faxa_dstwet2(:)
    Faxa_dstwet(3,:) = strm_Faxa_dstwet3(:)
    Faxa_dstwet(4,:) = strm_Faxa_dstwet4(:)

  end subroutine datm_pres_aero_advance

end module datm_pres_aero_mod
