module dwav_datamode_copyall_mod

  use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use shr_const_mod    , only : SHR_CONST_SPVAL

  implicit none
  private ! except

  public  :: dwav_datamode_copyall_advertise
  public  :: dwav_datamode_copyall_init_pointers
  public  :: dwav_datamode_copyall_advance

  ! export state pointer arrays
  real(r8), pointer :: Sw_lamult(:)  => null()
  real(r8), pointer :: Sw_ustokes(:) => null()
  real(r8), pointer :: Sw_vstokes(:) => null()

  ! stream pointer arrays
  real(r8), pointer :: strm_Sw_lamult(:)  => null()
  real(r8), pointer :: strm_Sw_ustokes(:) => null()
  real(r8), pointer :: strm_Sw_vstokes(:) => null()

  character(*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine dwav_datamode_copyall_advertise(exportState, fldsexport, flds_scalar_name, rc)

    ! input/output variables
    type(esmf_State)   , intent(inout) :: exportState
    type(fldlist_type) , pointer       :: fldsexport
    character(len=*)   , intent(in)    :: flds_scalar_name
    integer            , intent(out)   :: rc

    ! local variables
    type(fldlist_type), pointer :: fldList
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Advertise export fields
    call dshr_fldList_add(fldsExport, trim(flds_scalar_name))
    call dshr_fldList_add(fldsExport, 'Sw_lamult' )
    call dshr_fldList_add(fldsExport, 'Sw_ustokes')
    call dshr_fldList_add(fldsExport, 'Sw_vstokes')

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(dwav_comp_advertise): Fr_ocn'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine dwav_datamode_copyall_advertise

  !===============================================================================
  subroutine dwav_datamode_copyall_init_pointers(exportState, rc)

    ! input/output variables
    type(ESMF_State) , intent(inout) :: exportState
    integer          , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(dwav_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Initialize module ponters
    call dshr_state_getfldptr(exportState, 'Sw_lamult' , fldptr1=Sw_lamult, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sw_ustokes', fldptr1=Sw_ustokes , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Sw_ustokes', fldptr1=Sw_vstokes , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Initialize module pointers
    call shr_strdata_get_stream_pointer( sdat, 'Sw_lamult', strm_Sw_lamult, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Sw_lamult must be associated for dwav', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sw_ustokes', strm_Sw_ustokes, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Sw_ustokes must be associated for dwav', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Sw_vstokes', strm_Sw_vstokes, requirePointer=.true., &
         errmsg=trim(subname)//'ERROR: strm_Sw_vstokes must be associated for dwav', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine dwav_datamode_copyall_init_pointers

  !===============================================================================
  subroutine dwav_datamode_copyall_advance()

    Sw_lamult(:) = strm_Sw_lamult(:)
    Sw_ustokes(:) = strm_Sw_ustokes(:)
    Sw_vstokes(:) = strm_Sw_vstokes(:)

  end subroutine dwav_datamode_copyall_advance

end module dwav_datamode_copyall_mod
