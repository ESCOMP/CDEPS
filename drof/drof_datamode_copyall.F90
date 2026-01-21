module drof_datamode_copyall_mod

  use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use shr_const_mod    , only : SHR_CONST_SPVAL

  implicit none
  private

  public  :: drof_datamode_copyall_advertise
  public  :: drof_datamode_copyall_init_pointers
  public  :: drof_datamode_copyall_advance

  ! export state pointer arrays
  real(r8), pointer :: Forr_rofl(:) => null()
  real(r8), pointer :: Forr_rofi(:) => null()

  ! stream pointer arrays
  real(r8), pointer :: strm_Forr_rofl(:) => null() ! always required
  real(r8), pointer :: strm_Forr_rofi(:) => null() ! sometimes present in stream

  character(len=*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine drof_datamode_copyall_advertise(exportState, fldsexport, flds_scalar_name, rc)

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
    call dshr_fldlist_add(fldsExport, "Forr_rofl")
    call dshr_fldlist_add(fldsExport, "Forr_rofi")

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(drof_comp_advertise): Fr_ocn'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine drof_datamode_copyall_advertise

  !===============================================================================
  subroutine drof_datamode_copyall_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(drof_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Initialize module ponters
    call dshr_state_getfldptr(exportState, 'Forr_rofl' , fldptr1=Forr_rofl , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Forr_rofi' , fldptr1=Forr_rofi , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Initialize module pointers
    call shr_strdata_get_stream_pointer( sdat, 'Forr_rofl', strm_Forr_rofl, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Forr_rofl must be associated for drof', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call shr_strdata_get_stream_pointer( sdat, 'Forr_rofi', strm_Forr_rofi, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (.not. associated(strm_Forr_rofi)) then
       Forr_rofi(:) = 0._r8
    end if

  end subroutine drof_datamode_copyall_init_pointers

  !===============================================================================
  subroutine drof_datamode_copyall_advance()

    ! local variables
    integer :: ni
    !-------------------------------------------------------------------------------

    ! zero out "special values" of export fields
    do ni = 1, size(Forr_rofl)
       if (abs(strm_Forr_rofl(ni)) < 1.e28_r8) then
          Forr_rofl(ni) = strm_Forr_rofl(ni)
       else
          Forr_rofl(ni) = 0.0_r8
       end if
    enddo

    if (associated(strm_Forr_rofi)) then
       do ni = 1, size(Forr_rofi)
          if (abs(strm_Forr_rofi(ni)) < 1.e28_r8) then
             Forr_rofi(ni) = strm_Forr_rofi(ni)
          else
             Forr_rofi(ni) = 0.0_r8
          end if
       end do
    end if

  end subroutine drof_datamode_copyall_advance

end module drof_datamode_copyall_mod
