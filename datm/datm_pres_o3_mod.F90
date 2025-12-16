module datm_pres_o3_mod

  use ESMF             , only : ESMF_SUCCESS, ESMF_State, ESMF_StateItem_Flag
  use ESMF             , only : ESMF_STATEITEM_NOTFOUND
  use shr_kind_mod     , only : r8=>shr_kind_r8
  use shr_log_mod      , only : shr_log_error
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private ! except

  public :: datm_pres_o3_advertise
  public :: datm_pres_o3_init_pointers
  public :: datm_pres_o3_advance

  ! export state data
  real(r8), pointer :: Sa_o3(:) => null()

  ! stream pointer
  real(r8), pointer :: strm_Sa_o3(:) => null()

  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_pres_o3_advertise(fldsExport)

    ! input/output variables
    type(fldlist_type) , pointer :: fldsexport

    call dshr_fldList_add(fldsExport, 'Sa_o3')

  end subroutine datm_pres_o3_advertise

  !===============================================================================
  subroutine datm_pres_o3_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_o3_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Get pointer to export state
    call dshr_state_getfldptr(exportState, 'Sa_o3', fldptr1=Sa_o3, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Get pointer to stream data that will be used below - if the
    ! following stream fields are not in any sdat streams, then a null value is returned
    call shr_strdata_get_stream_pointer(sdat, 'Sa_o3', strm_Sa_o3, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return


    ! Error checks
    if (.not. associated(Sa_o3)) then
       call shr_log_error(trim(subname)//'ERROR: Sa_o3 must be associated if flds_pres_o3 is .true.')
    end if
    if (.not. associated(strm_Sa_o3)) then
       call shr_log_error(trim(subname)//'ERROR: strm_Sa_o3 must be associated if flds_pres_o3 is .true.')
    end if

  end subroutine datm_pres_o3_init_pointers

  !===============================================================================
  subroutine datm_pres_o3_advance()

    Sa_o3(:) = strm_Sa_o3(:)

  end subroutine datm_pres_o3_advance

end module datm_pres_o3_mod
