module datm_pres_co2_mod

  use ESMF             , only : ESMF_SUCCESS, ESMF_State
  use shr_kind_mod     , only : r8=>shr_kind_r8, cl=>shr_kind_cl
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private

  public :: datm_pres_co2_advertise
  public :: datm_pres_co2_init_pointers
  public :: datm_pres_co2_advance

  ! export state data
  real(r8), pointer :: Sa_co2diag(:) => null()
  real(r8), pointer :: Sa_co2prog(:) => null()

  ! stream pointer
  real(r8), pointer :: strm_Sa_co2diag(:) => null()
  real(r8), pointer :: strm_Sa_co2prog(:) => null()

  character(len=CL) :: datamode

  character(len=*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_pres_co2_advertise(fldsExport, datamode_in)

    ! input/output variables
    type(fldlist_type) , pointer    :: fldsexport
    character(len=*)   , intent(in) :: datamode_in
    !----------------------------------------------------------

    ! Set module variable
    datamode = datamode_in

    call dshr_fldList_add(fldsExport, 'Sa_co2diag')
    call dshr_fldList_add(fldsExport, 'Sa_co2prog')

  end subroutine datm_pres_co2_advertise

  !===============================================================================
  subroutine datm_pres_co2_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_pres_co2_init_pointers): '
    !----------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Get pointer to export state
    call dshr_state_getfldptr(exportState, 'Sa_co2diag', fldptr1=Sa_co2diag, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call dshr_state_getfldptr(exportState, 'Sa_co2prog', fldptr1=Sa_co2prog, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Get pointer to stream data that will be used below
    call shr_strdata_get_stream_pointer(sdat, 'Sa_co2diag', strm_Sa_co2diag, requirePointer=.true., &
         errmsg=subname//'strm_Sa_co2diag must be associated if flds_co2 is .true.', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if (datamode == 'CPLHIST') then
       call shr_strdata_get_stream_pointer(sdat, 'Sa_co2prog', strm_Sa_co2prog, requirePointer=.true., &
            errmsg=subname//'strm_Sa_co2prog must be associated if flds_co2 is .true. '// &
            ' and datamode is CPLHIST', rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
    end if

  end subroutine datm_pres_co2_init_pointers

  !===============================================================================
  subroutine datm_pres_co2_advance()

    if (datamode == 'CPLHIST') then
       Sa_co2diag(:) = strm_Sa_co2diag(:)
       Sa_co2prog(:) = strm_Sa_co2prog(:)
    else
       ! Because we do not currently have any Sa_co2prog in this case,
       ! for now set Sa_co2prog equal to Sa_co2diag
       Sa_co2diag(:) = strm_Sa_co2diag(:)
       Sa_co2prog(:) = strm_Sa_co2diag(:)
    end if

  end subroutine datm_pres_co2_advance

end module datm_pres_co2_mod
