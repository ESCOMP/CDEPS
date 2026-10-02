module datm_pres_ndep_mod

  use ESMF             , only : ESMF_SUCCESS, ESMF_State, ESMF_StateItem_Flag
  use shr_kind_mod     , only : r8=>shr_kind_r8
  use shr_log_mod      , only : shr_log_error
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add

  implicit none
  private

  public :: datm_pres_ndep_advertise
  public :: datm_pres_ndep_init_pointers
  public :: datm_pres_ndep_advance

  ! export state data
  real(r8), pointer :: Faxa_ndep(:,:) => null()

  ! stream data
  real(r8), pointer :: strm_Faxa_ndep_nhx_dry(:) => null() ! stream cmip7 ndep data
  real(r8), pointer :: strm_Faxa_ndep_nhx_wet(:) => null() ! stream cmip7 ndep data
  real(r8), pointer :: strm_Faxa_ndep_noy_dry(:) => null() ! stream cmip7 ndep data
  real(r8), pointer :: strm_Faxa_ndep_noy_wet(:) => null() ! stream cmip7 ndep data

  real(r8), pointer :: strm_Faxa_ndep_nhx(:)     => null() ! pre-cmip7 ndep data and cplhist
  real(r8), pointer :: strm_Faxa_ndep_noy(:)     => null() ! pre-cmip7 ndep data and cplhist


  character(len=*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine datm_pres_ndep_advertise(fldsExport)

    ! input/output variables
    type(fldlist_type) , pointer :: fldsexport
    !----------------------------------------------------------

    call dshr_fldList_add(fldsExport, 'Faxa_ndep', ungridded_lbound=1, ungridded_ubound=2)

  end subroutine datm_pres_ndep_advertise

  !===============================================================================
  subroutine datm_pres_ndep_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(datm_ndep_init_pointers): '
    !----------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Get pointer to export state
    call dshr_state_getfldptr(exportState, 'Faxa_ndep', fldptr2=Faxa_ndep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Get pointer to stream data that will be used below - if the
    ! following stream fields are not in any sdat streams, then a null value is returned

    ! cmip7 ndep forcing
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_ndep_nhx_dry', strm_Faxa_ndep_nhx_dry, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_ndep_nhx_wet', strm_Faxa_ndep_nhx_wet, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_ndep_noy_dry', strm_Faxa_ndep_noy_dry, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer(sdat, 'Faxa_ndep_noy_wet', strm_Faxa_ndep_noy_wet, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! cmip6 ndep forcing and cplhist
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_ndep_nhx', strm_Faxa_ndep_nhx, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Faxa_ndep_noy', strm_Faxa_ndep_noy, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Check that one of the two supported field sets was found.
    if (.not. ((associated(strm_Faxa_ndep_nhx_dry) .and. associated(strm_Faxa_ndep_nhx_wet) .and.  &
                associated(strm_Faxa_ndep_noy_dry) .and. associated(strm_Faxa_ndep_noy_wet)) .or. &
               (associated(strm_Faxa_ndep_nhx)     .and. associated(strm_Faxa_ndep_noy)))) then
       call shr_log_error('datm_pres_ndep: ERROR: no associated stream pointers for ndep forcing', rc=rc)
       return
    end if

  end subroutine datm_pres_ndep_init_pointers

  !===============================================================================
  subroutine datm_pres_ndep_advance()

    ! Separate dry and wet deposition fields are summed where the stream provides
    ! them; otherwise the combined fields are used.
    if (associated(strm_Faxa_ndep_nhx_dry)) then
       Faxa_ndep(1,:) = strm_Faxa_ndep_nhx_dry(:) + strm_Faxa_ndep_nhx_wet(:)
       Faxa_ndep(2,:) = strm_Faxa_ndep_noy_dry(:) + strm_Faxa_ndep_noy_wet(:)
    else
       Faxa_ndep(1,:) = strm_Faxa_ndep_nhx(:)
       Faxa_ndep(2,:) = strm_Faxa_ndep_noy(:)
    end if

  end subroutine datm_pres_ndep_advance

end module datm_pres_ndep_mod
