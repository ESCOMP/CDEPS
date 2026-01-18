module drof_datamode_cplhist_mod

  use ESMF             , only : ESMF_State, ESMF_LOGMSG_INFO, ESMF_LogWrite, ESMF_SUCCESS
  use NUOPC            , only : NUOPC_Advertise
  use shr_kind_mod     , only : r8=>shr_kind_r8
  use dshr_fldlist_mod , only : fldlist_type, dshr_fldlist_add
  use dshr_methods_mod , only : dshr_state_getfldptr, chkerr
  use dshr_strdata_mod , only : shr_strdata_type, shr_strdata_get_stream_pointer
  use shr_const_mod    , only : SHR_CONST_SPVAL

  implicit none
  private

  public :: drof_datamode_cplhist_advertise
  public :: drof_datamode_cplhist_init_pointers
  public :: drof_datamode_cplhist_advance

  ! export state pointer arrays
  real(r8), pointer :: Forr_rofl(:)     => null() ! mediator sends this to ocn
  real(r8), pointer :: Forr_rofl_glc(:) => null() ! mediator sends this to ocn
  real(r8), pointer :: Forr_rofi(:)     => null() ! mediator sends this to ocn
  real(r8), pointer :: Forr_rofi_glc(:) => null() ! mediator sends this to ocn
  real(r8), pointer :: Flrr_flood(:)    => null() ! mediator sends this to lnd
  real(r8), pointer :: Flrr_volr(:)     => null() ! mediator sends this to lnd
  real(r8), pointer :: Flrr_volrmch(:)  => null() ! mediator sends this to lnd

  ! stream pointer arrays
  real(r8), pointer :: strm_Forr_rofl(:)     => null()
  real(r8), pointer :: strm_Forr_rofi(:)     => null()
  real(r8), pointer :: strm_Forr_rofl_glc(:) => null()
  real(r8), pointer :: strm_Forr_rofi_glc(:) => null()
  real(r8), pointer :: strm_Flrr_flood(:)    => null()
  real(r8), pointer :: strm_Flrr_volr(:)     => null()
  real(r8), pointer :: strm_Flrr_volrmch(:)  => null()

  character(len=*) , parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine drof_datamode_cplhist_advertise(exportState, fldsexport, flds_scalar_name, rc)

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
    call dshr_fldlist_add(fldsExport, "Forr_rofl_glc")
    call dshr_fldlist_add(fldsExport, "Forr_rofi")
    call dshr_fldlist_add(fldsExport, "Forr_rofi_glc")
    call dshr_fldlist_add(fldsExport, "Flrr_flood")
    call dshr_fldlist_add(fldsExport, "Flrr_volr")
    call dshr_fldlist_add(fldsExport, "Flrr_volrmch")

    fldlist => fldsExport ! the head of the linked list
    do while (associated(fldlist))
       call NUOPC_Advertise(exportState, standardName=fldlist%stdname, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       call ESMF_LogWrite('(drof_comp_advertise): Fr_drof'//trim(fldList%stdname), ESMF_LOGMSG_INFO)
       fldList => fldList%next
    enddo

  end subroutine drof_datamode_cplhist_advertise

  !===============================================================================
  subroutine drof_datamode_cplhist_init_pointers(exportState, sdat, rc)

    ! input/output variables
    type(ESMF_State)       , intent(inout) :: exportState
    type(shr_strdata_type) , intent(in)    :: sdat
    integer                , intent(out)   :: rc

    ! local variables
    character(len=*), parameter :: subname='(drof_datamode_cplhist_init_pointers): '
    !-------------------------------------------------------------------------------

    rc = ESMF_SUCCESS

    ! Initialize module export state pointers
    call dshr_state_getfldptr(exportState, 'Forr_rofl'    , fldptr1=Forr_rofl    , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Forr_rofl_glc', fldptr1=Forr_rofl_glc, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Forr_rofi'    , fldptr1=Forr_rofi    , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Forr_rofi_glc', fldptr1=Forr_rofi_glc, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Flrr_flood'   , fldptr1=Flrr_flood   , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Flrr_volr'    , fldptr1=Flrr_volr    , rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call dshr_state_getfldptr(exportState, 'Flrr_volrmch' , fldptr1=Flrr_volrmch, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Initialize module stream pointers
    call shr_strdata_get_stream_pointer( sdat, 'Forr_rofl', strm_Forr_rofl, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Forr_rofl must be associated for drof cplhist mode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Forr_rofl_glc', strm_Forr_rofl_glc, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Forr_rofl_glc must be associated for drof cplhist mode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Forr_rofi', strm_Forr_rofi, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Forr_rofi must be associated for drof cplhist mode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Forr_rofi_glc', strm_Forr_rofi_glc, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Forr_rofi_glc must be associated for drof cplhist mode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Flrr_flood', strm_Flrr_flood, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Flrr_flood must be associated for drof cplhist mode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Flrr_volr', strm_Flrr_volr, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Flrr_volr must be associated for drof cplhist mode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call shr_strdata_get_stream_pointer( sdat, 'Flrr_volrmch', strm_Flrr_volrmch, requirePointer=.true., &
         errmsg=subname//'ERROR: strm_Flrr_volrmch must be associated for drof cplhist mode', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine drof_datamode_cplhist_init_pointers

  !===============================================================================
  subroutine drof_datamode_cplhist_advance()

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

    do ni = 1, size(Forr_rofl_glc)
       if (abs(strm_Forr_rofl_glc(ni)) < 1.e28_r8) then
          Forr_rofl_glc(:) = strm_Forr_rofl_glc(:)
       else
          Forr_rofl_glc(ni) = 0.0_r8
       end if
    end do

    do ni = 1, size(Forr_rofi)
       if (abs(strm_Forr_rofi(ni)) < 1.e28_r8) then
          Forr_rofi(:) = strm_Forr_rofi(:)
       else
          Forr_rofi(ni) = 0.0_r8
       end if
    end do

    do ni = 1, size(Forr_rofi_glc)
       if (abs(strm_Forr_rofi_glc(ni)) < 1.e28_r8) then
          Forr_rofi_glc(:) = strm_Forr_rofi_glc(:)
       else
          Forr_rofi_glc(ni) = 0.0_r8
       end if
    end do

    do ni = 1, size(Flrr_flood)
       if (abs(strm_Flrr_flood(ni)) < 1.e28_r8) then
          Flrr_flood(ni) = strm_Flrr_flood(ni)
       else
          Flrr_flood(ni) = 0.0_r8
       end if
    enddo

    do ni = 1, size(Flrr_volr)
       if (abs(strm_Flrr_volr(ni)) < 1.e28_r8) then
          Flrr_volr(ni) = strm_Flrr_volr(ni)
       else
          Flrr_volr(ni) = 0.0_r8
       end if
    enddo

    do ni = 1, size(Flrr_volrmch)
       if (abs(strm_Flrr_volrmch(ni)) < 1.e28_r8) then
          Flrr_volrmch(ni) = strm_Flrr_volrmch(ni)
       else
          Flrr_volrmch(ni) = 0.0_r8
       end if
    enddo

  end subroutine drof_datamode_cplhist_advance

end module drof_datamode_cplhist_mod
